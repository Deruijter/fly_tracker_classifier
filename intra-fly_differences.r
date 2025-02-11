# This script examines the differences in behavior within individual flies

script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(script_dir)

source("packages.r")
source("constants.r")
source("basic_functions.r")
source("create_dt_videos.r")

dt = GetDtVideos()

# dt = dt[dt$exp_type=='1b',]

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



# perform simple multiple linear regression to get an idea of which factors may have influenced the association
# m = glm('duration_still ~ sex_m + exp_type + time_of_day', data=intra_diff)
# summary(m)
# m = glm('duration_grooming ~ sex_m + exp_type + time_of_day', data=intra_diff)
# summary(m)
# m = glm('duration_moving ~ sex_m + exp_type + time_of_day', data=intra_diff)
# summary(m)
# m = glm('velocity_non_zero_mean ~ sex_m + exp_type + time_of_day', data=intra_diff)
# summary(m)
# m = glm('velocity_non_zero_sd ~ sex_m + exp_type + time_of_day', data=intra_diff)
# summary(m)
# m = glm('distance_total ~ sex_m + exp_type + time_of_day', data=intra_diff)
# summary(m)


# Create permutation data sets that we can compare to the real data
set.seed(1337)
permuted_vel_non_zero_mean = c()
permuted_vel_non_zero_sd = c()
permuted_duration_still = c()
permuted_duration_grooming = c()
permuted_duration_moving = c()
permuted_distance_total = c()
for(i in c(1:10000)){
  vids = dt[sample(1:nrow(dt),6),]
  a = rrmse(vids[vids$duration_moving>0.01,'velocity_non_zero_mean'], mean(vids[vids$duration_moving>0.01,'velocity_non_zero_mean']))
  b = rrmse(vids[vids$duration_moving>0.01,'velocity_non_zero_sd'], mean(vids[vids$duration_moving>0.01,'velocity_non_zero_sd']))
  c = rrmse(vids[,'duration_still'], mean(vids[,'duration_still']))
  d = rrmse(vids[,'duration_grooming'], mean(vids[,'duration_grooming']))
  e = rrmse(vids[,'duration_moving'], mean(vids[,'duration_moving']))
  f = rrmse(vids[,'distance_total'], mean(vids[,'distance_total']))
  permuted_vel_non_zero_mean = c(permuted_vel_non_zero_mean,a)
  permuted_vel_non_zero_sd = c(permuted_vel_non_zero_sd, b)
  permuted_duration_still = c(permuted_duration_still, c)
  permuted_duration_grooming = c(permuted_duration_grooming, d)
  permuted_duration_moving = c(permuted_duration_moving, e)
  permuted_distance_total = c(permuted_distance_total, f)
}

# Permutation test - Velocity (mean)
t.test(intra_diff$velocity_non_zero_mean, permuted_vel_non_zero_mean)$p.value
dt_hist = data.frame('value'=c(intra_diff$velocity_non_zero_mean, permuted_vel_non_zero_mean), 'group'=c(rep('a',nrow(intra_diff)), rep('b', length(permuted_vel_non_zero_mean))))
p1 = ggplot(dt_hist, aes(x=value, group=group, fill=group)) +
  geom_density(alpha=0.5, show.legend = F) +
  xlab('Non-zero velocity (mean)') + 
  xlim(c(0,150)) + 
  ggtitle('Velocity (mean)') +
  scale_fill_manual(name="",values=c("black","#FF1800"),labels=c("real","perm.")) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())

# Permutation test - Velocity (SD)
t.test(intra_diff$velocity_non_zero_sd, permuted_vel_non_zero_sd)$p.value
dt_hist = data.frame('value'=c(intra_diff$velocity_non_zero_sd, permuted_vel_non_zero_sd), 'group'=c(rep('a',nrow(intra_diff)), rep('b', length(permuted_vel_non_zero_sd))))
p2 = ggplot(dt_hist, aes(x=value, group=group, fill=group)) +
  geom_density(alpha=0.5, show.legend = T) +
  xlab('Non-zero velocity (SD)') + 
  xlim(c(0,150)) + 
  ggtitle('Velocity (SD)') +
  scale_fill_manual(name="",values=c("black","#FF1800"),labels=c("real","perm.")) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = c(0.8, 0.8))

# Permutation test - Duration (still)
t.test(intra_diff$duration_still, permuted_duration_still)$p.value
dt_hist = data.frame('value'=c(intra_diff$duration_still, permuted_duration_still), 'group'=c(rep('a',nrow(intra_diff)), rep('b', length(permuted_duration_still))))
p3 = ggplot(dt_hist, aes(x=value, group=group, fill=group)) +
  geom_density(alpha=0.5, show.legend = F) +
  # xlab('normalized RMSD (%)') + 
  xlim(c(0,150)) + 
  ggtitle('Duration still') +
  scale_fill_manual(name="",values=c("black","#FF1800"),labels=c("real","perm.")) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())

# Permutation test - Duration (grooming)
t.test(intra_diff$duration_grooming, permuted_duration_grooming)$p.value
dt_hist = data.frame('value'=c(intra_diff$duration_grooming, permuted_duration_grooming), 'group'=c(rep('a',nrow(intra_diff)), rep('b', length(permuted_duration_grooming))))
p4 = ggplot(dt_hist, aes(x=value, group=group, fill=group)) +
  geom_density(alpha=0.5, show.legend = F) +
  # xlab('normalized RMSD (%)') + 
  xlim(c(0,150)) + 
  ggtitle('Duration grooming') +
  scale_fill_manual(name="",values=c("black","#FF1800"),labels=c("real","perm.")) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())

# Permutation test - Duration (moving)
t.test(intra_diff$duration_moving, permuted_duration_moving)$p.value
dt_hist = data.frame('value'=c(intra_diff$duration_moving, permuted_duration_moving), 'group'=c(rep('a',nrow(intra_diff)), rep('b', length(permuted_duration_moving))))
p5 = ggplot(dt_hist, aes(x=value, group=group, fill=group)) +
  geom_density(alpha=0.5, show.legend = F) +
  xlab('RSD (%)') + 
  xlim(c(0,150)) + 
  ggtitle('Duration moving') +
  scale_fill_manual(name="",values=c("black","#FF1800"),labels=c("real","perm.")) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())

# Permutation test - Total distance travelled
t.test(intra_diff$distance_total, permuted_distance_total)$p.value
dt_hist = data.frame('value'=c(intra_diff$distance_total, permuted_distance_total), 'group'=c(rep('a',nrow(intra_diff)), rep('b', length(permuted_distance_total))))
p6 = ggplot(dt_hist, aes(x=value, group=group, fill=group)) +
  geom_density(alpha=0.5, show.legend = F) +
  xlab('RSD (%)') + 
  xlim(c(0,150)) + 
  ggtitle('Distance travelled') +
  scale_fill_manual(name="",values=c("black","#FF1800"),labels=c("real","perm.")) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())

grid.arrange(p1,p2,p3,p4,p5,p6, ncol=2)

pdf(file=sprintf('%s/intra-fly_motion_comp.pdf',path_output), width = 6, height=3)
grid.arrange(p1,p2,p3,p4,p5,p6, ncol=2)
dev.off()

write.csv(intra_diff, sprintf('%s/%s', path_output, 'intra_fly_diff.csv'), row.names = F)
write.csv(permuted_vel_non_zero_mean, sprintf('%s/%s', path_output, 'permuted_vel_non_zero_mean.csv'), row.names = F)
write.csv(permuted_vel_non_zero_sd, sprintf('%s/%s', path_output, 'permuted_vel_non_zero_sd.csv'), row.names = F)
write.csv(permuted_dur_velocity_non_zero, sprintf('%s/%s', path_output, 'permuted_dur_velocity_non_zero.csv'), row.names = F)
write.csv(permuted_distance_total, sprintf('%s/%s', path_output, 'permuted_distance_total.csv'), row.names = F)








