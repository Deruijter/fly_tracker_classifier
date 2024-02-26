# This script examines the differences in behavior within individual flies

script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(script_dir)

source("packages.r")
source("constants.r")
source("basic_functions.r")
source("create_dt_videos.r")

dt = GetDtVideos()



intra_diff = dt %>%
  group_by(fid) %>%
  summarise('sex_m' = first(sex_m),
            'exp_type' = first(exp_type),
            'time_of_day' = first(time_of_day),
            'velocity_non_zero_mean' = rrmse(velocity_non_zero_mean[!is.na(velocity_non_zero_mean)], mean(dt$velocity_non_zero_mean, na.rm = T)),
            'velocity_non_zero_sd' = rrmse(velocity_non_zero_sd[!is.na(velocity_non_zero_sd)], mean(dt$velocity_non_zero_sd, na.rm=T)),
            'duration_still' = rrmse(duration_still, mean(dt[,'duration_still'])),
            'duration_grooming' = rrmse(duration_grooming, mean(dt[,'duration_grooming'])),
            'duration_moving' = rrmse(duration_moving, mean(dt[,'duration_moving'])),
            'distance_total' = rrmse(distance_total, mean(dt[,'distance_total']))) %>%
  as.data.frame()


m = glm('duration_still ~ sex_m + exp_type + time_of_day', data=intra_diff)
summary(m)
m = glm('duration_grooming ~ sex_m + exp_type + time_of_day', data=intra_diff)
summary(m)
m = glm('duration_moving ~ sex_m + exp_type + time_of_day', data=intra_diff)
summary(m)
m = glm('velocity_non_zero_mean ~ sex_m + exp_type + time_of_day', data=intra_diff)
summary(m)
m = glm('velocity_non_zero_sd ~ sex_m + exp_type + time_of_day', data=intra_diff)
summary(m)


permuted_vel_non_zero_mean = c()
permuted_vel_non_zero_sd = c()
permuted_duration_still = c()
permuted_duration_grooming = c()
permuted_duration_moving = c()
permuted_distance_total = c()
for(i in c(1:10000)){
  vids = dt[sample(1:233,6),]
  a = rrmse(vids[vids$duration_moving>0.01,'velocity_non_zero_mean'], mean(vids[vids$duration_moving>0.01,'velocity_non_zero_mean']))
  b = rrmse(vids[vids$duration_moving>0.01,'velocity_non_zero_sd'], mean(vids[vids$duration_moving>0.01,'velocity_non_zero_sd']))
  c = rrmse(vids[,'duration_still'], mean(vids[,'duration_still']))
  c = rrmse(vids[,'duration_grooming'], mean(vids[,'duration_grooming']))
  c = rrmse(vids[,'duration_moving'], mean(vids[,'duration_moving']))
  d = rrmse(vids[,'distance_total'], mean(vids[,'distance_total']))
  permuted_vel_non_zero_mean = c(permuted_vel_non_zero_mean,a)
  permuted_vel_non_zero_sd = c(permuted_vel_non_zero_sd, b)
  permuted_duration_still = c(permuted_duration_still, c)
  permuted_duration_grooming = c(permuted_duration_grooming, c)
  permuted_duration_moving = c(permuted_duration_moving, c)
  permuted_distance_total = c(permuted_distance_total, d)
}

t.test(intra_diff$velocity_non_zero_mean, permuted_vel_non_zero_mean)
dt_hist = data.frame('value'=c(intra_diff$velocity_non_zero_mean, permuted_vel_non_zero_mean), 'group'=c(rep('a',nrow(intra_diff)), rep('b', length(permuted_vel_non_zero_mean))))
ggplot(dt_hist, aes(x=value, group=group, fill=group)) +
  geom_density(alpha=0.5, show.legend = F) +
  xlab('Non-zero velocity (mean)') + 
  xlim(c(0,150)) + 
  scale_fill_manual(name="",values=c("black","#FF1800"),labels=c("real","perm.")) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())

t.test(intra_diff$velocity_non_zero_sd, permuted_vel_non_zero_sd)
dt_hist = data.frame('value'=c(intra_diff$velocity_non_zero_sd, permuted_vel_non_zero_sd), 'group'=c(rep('a',nrow(intra_diff)), rep('b', length(permuted_vel_non_zero_sd))))
ggplot(dt_hist, aes(x=value, group=group, fill=group)) +
  geom_density(alpha=0.5, show.legend = T) +
  xlab('Non-zero velocity (SD)') + 
  xlim(c(0,150)) + 
  scale_fill_manual(name="",values=c("black","#FF1800"),labels=c("real","sim.")) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = c(0.8, 0.8))

t.test(intra_diff$duration_still, permuted_duration_still)
dt_hist = data.frame('value'=c(intra_diff$duration_still, permuted_duration_still), 'group'=c(rep('a',nrow(intra_diff)), rep('b', length(permuted_duration_still))))
ggplot(dt_hist, aes(x=value, group=group, fill=group)) +
  geom_density(alpha=0.5, show.legend = F) +
  xlab('normalized RMSD (%)') + 
  #xlim(c(0,150)) + 
  scale_fill_manual(name="",values=c("black","#FF1800"),labels=c("real","perm.")) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())

t.test(intra_diff$duration_grooming, permuted_duration_grooming)
dt_hist = data.frame('value'=c(intra_diff$duration_grooming, permuted_duration_grooming), 'group'=c(rep('a',nrow(intra_diff)), rep('b', length(permuted_duration_grooming))))
ggplot(dt_hist, aes(x=value, group=group, fill=group)) +
  geom_density(alpha=0.5, show.legend = F) +
  xlab('normalized RMSD (%)') + 
  #xlim(c(0,150)) + 
  scale_fill_manual(name="",values=c("black","#FF1800"),labels=c("real","perm.")) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())

t.test(intra_diff$duration_moving, permuted_duration_moving)
dt_hist = data.frame('value'=c(intra_diff$duration_moving, permuted_duration_moving), 'group'=c(rep('a',nrow(intra_diff)), rep('b', length(permuted_duration_moving))))
ggplot(dt_hist, aes(x=value, group=group, fill=group)) +
  geom_density(alpha=0.5, show.legend = F) +
  xlab('normalized RMSD (%)') + 
  xlim(c(0,150)) + 
  scale_fill_manual(name="",values=c("black","#FF1800"),labels=c("real","perm.")) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())

t.test(intra_diff$distance_total, permuted_distance_total)
dt_hist = data.frame('value'=c(intra_diff$distance_total, permuted_distance_total), 'group'=c(rep('a',nrow(intra_diff)), rep('b', length(permuted_distance_total))))
ggplot(dt_hist, aes(x=value, group=group, fill=group)) +
  geom_density(alpha=0.5, show.legend = F) +
  xlab('normalized RMSD (%)') + 
  xlim(c(0,150)) + 
  scale_fill_manual(name="",values=c("black","#FF1800"),labels=c("real","perm.")) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())

grid.arrange(p1,p2,p3,p4, ncol=2)

pdf(file=sprintf('%s/tmp/intra-fly_motion_comp.pdf',path_output), width = 6, height=3)
grid.arrange(p1,p2,p3,p4, ncol=2)
dev.off()

write.csv(intra_diff, sprintf('%s/%s', path_output, 'intra_fly_diff.csv'), row.names = F)
write.csv(permuted_vel_non_zero_mean, sprintf('%s/%s', path_output, 'permuted_vel_non_zero_mean.csv'), row.names = F)
write.csv(permuted_vel_non_zero_sd, sprintf('%s/%s', path_output, 'permuted_vel_non_zero_sd.csv'), row.names = F)
write.csv(permuted_dur_velocity_non_zero, sprintf('%s/%s', path_output, 'permuted_dur_velocity_non_zero.csv'), row.names = F)
write.csv(permuted_distance_total, sprintf('%s/%s', path_output, 'permuted_distance_total.csv'), row.names = F)





# PERFORM BAYESIAN STATISTICS (THANKS ORESTE!)
install.packages(c("coda","mvtnorm","devtools","loo","dagitty"))
devtools::install_github("rmcelreath/rethinking@slim")
library(rethinking)

vel_mean = intra_diff$vel_non_zero_mean
vel_sd = intra_diff$vel_non_zero_sd
duration = intra_diff$dur_velocity_non_zero
distance = intra_diff$distance_total

# Calculating the differences between the real values and the permuted ones
difference_vel_mean = vel_mean - permuted_vel_non_zero_mean[1:40]
hist(difference_vel_mean)
difference_vel_sd = vel_sd - permuted_vel_non_zero_sd
hist(difference_vel_sd)
difference_duration = duration - permuted_dur_velocity_non_zero
hist(difference_duration)
difference_distance = distance - permuted_distance_total
hist(difference_distance)

# Assuming the prior distributions
prior_mean = alist(
  difference_vel_mean ~ dnorm(mi, sigma),
  mi ~ dnorm(0, 50),
  sigma ~ dunif(0, 50)
)

prior_sd = alist(
  difference_vel_sd ~ dnorm(mi, sigma),
  mi ~ dnorm(0, 50),
  sigma ~ dunif(0, 50)
)

prior_dur = alist(
  difference_duration ~ dnorm(mi, sigma),
  mi ~ dnorm(0, 50),
  sigma ~ dunif(0, 50)
)

prior_dist = alist(
  difference_distance ~ dnorm(mi, sigma),
  mi ~ dnorm(0, 50),
  sigma ~ dunif(0, 50)
)

# To calculate the posterior, the vectors should become dataframes
difference_vel_mean = as.data.frame(difference_vel_mean)
difference_vel_sd = as.data.frame(difference_vel_sd)
difference_duration = as.data.frame(difference_duration)
difference_distance = as.data.frame(difference_distance)
# Calculating the posterior, using the quadratic approximation
posterior_vel_mean = quap(prior_mean , data = difference_vel_mean)
posterior_vel_sd = quap(prior_sd, data = difference_vel_sd)
posterior_duration = quap(prior_dur, data = difference_duration)
posterior_distance = quap(prior_dist, data = difference_distance)
# Output of the results
precis(posterior_vel_mean, prob = 0.95)
precis(posterior_vel_sd, prob = 0.95)
precis(posterior_duration, prob = 0.95)
precis(posterior_distance, prob = 0.95)
# This was a rather simple calculation, but you might want to have a vission
# of the approximated posterior
post_sampling = extract.samples(posterior_vel_mean, n = 40)
hist(post_sampling$mi)
