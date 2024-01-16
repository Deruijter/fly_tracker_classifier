# This code contains the analysis of the fly behavior as tracked using SLEAP
# Author: Markus de Ruijter, 2023

library(plyr)
library(ggplot2)
library(reshape2)
library(devtools)
library(ggbiplot)
library(RColorBrewer)
library(dplyr)
library(factoextra)
library(numbers)
library(data.table)
library(BiocManager)
#BiocManager::install('rhdf5')
library(rhdf5)
library(paletteer)
library(zoo)
require(gridExtra)
library(animation)
library(egg)


# Consider any velocity under this value as zero (based on histogram visualization).
# Reason for this is that the camera will always record some kind of speed due to vibrations/conversion of the videos and stuff like that
non_zero = 0.4 # Unit: pixels per time frame

id_head = 1
id_thorax = 2
id_abdomen = 3
id_leg1 = 4
id_leg2 = 5
id_leg3 = 6
id_leg4 = 7
id_leg5 = 8
id_leg6 = 9
id_wing_l = 10
id_wing_r = 11
id_eye_l = 12
id_eye_r = 13
id_x = 1
id_y = 2

path_data = '/home/leven/work/uppsala_helgi/proj_fly/data/behavior'
path_tracks = '/home/leven/work/uppsala_helgi/proj_fly/data/track_exports'
path_tracks_csv = '/home/leven/work/uppsala_helgi/proj_fly/data/track_exports_csv'
path_speed_csv = '/home/leven/work/uppsala_helgi/proj_fly/data/track_speed_csv'
path_output = '/home/leven/work/uppsala_helgi/proj_fly/output'
exp_details = read.csv('/home/leven/work/uppsala_helgi/proj_fly/data/tests/data.txt', header=T, sep='\t')


################### CREATE THE DATA SET ########################################
nr_clusters=3-1 #-1 because the numbering starts at 0
nr_comb_cl = 3 - 1
files = list.files(path_data, '*.csv')
dt = data.frame('fid'=character(),
                'date'=character(), 
                'rec_nr'=numeric(), 
                'l_r'=character(), 
                'sex_m'=numeric(), 
                'exp_type'=character(),
                'time_of_day'=character(),
                'rec_nr_cor'=numeric(),
                'velocity_non_zero_mean'=numeric(),
                'velocity_non_zero_sd'=numeric(),
                'duration_still'=numeric(),
                'duration_grooming'=numeric(),
                'duration_moving'=numeric(),
                'distance_total'=numeric()
                )
for (i in c(0:nr_clusters)){
  # dt = cbind(dt, data.frame('tmp'=numeric()))
  # colnames(dt)[length(colnames(dt))] = sprintf('freq_%s',i)
  # dt = cbind(dt, data.frame('tmp'=numeric()))
  # colnames(dt)[length(colnames(dt))] = sprintf('mdur_%s',i)
  dt = cbind(dt, data.frame('tmp'=numeric()))
  colnames(dt)[length(colnames(dt))] = sprintf('frms_%s',i)
}

# Loop through the b-soid cluster files and append the data to our data frame
for (file in files){
  tmp = read.csv(sprintf('%s/%s', path_data, file), header=T, sep =",", )
  # Group similar clusters together
  # 0 = Rest
  # 1 = Groom
  # 2 = Move
  tmp[tmp$B.SOiD.labels %in% c(0,1,2,3,4,5,6,7,
                               9,10,11,12,13,
                               15,16,17,
                               19,20,21,22,23,24,25,
                               27,
                               33,
                               35,
                               38,
                               41,42,
                               51), 'B.SOiD.labels'] = 0
  tmp[tmp$B.SOiD.labels %in% c(8,14,18,28,30,31,32,34,39,40,43,44,45), 'B.SOiD.labels'] = 1
  tmp[tmp$B.SOiD.labels %in% c(26,29,36,37,46,47,48,49,50), 'B.SOiD.labels'] = 2
  
  
  date = substring(file, 31, 40)
  session = substring(file, 42, 42)
  l_r = substring(file, 29, 29)
  rec_nr = substring(file, 44,44)
  print(sprintf('%s_%s_%s', date, session, l_r))
  
  fid = sprintf('%s_%s_%s', date, session, l_r)
  
  exp_detail = exp_details[exp_details$date==date & exp_details$couple_id==session & exp_details$testnr==rec_nr,]
  sex_m = tolower(exp_detail[sprintf('sex_%s', l_r)]) == 'm'
  exp_type = tolower(exp_detail$test_type)
  exp_time = 'morning'
  if(as.numeric(substring(exp_detail$time_rec,1,2)) >= 12) exp_time = 'afternoon'
  
  h5details = GetH5Details(date, session, rec_nr, l_r)
  
  a = data.frame(fid,date,as.numeric(rec_nr),l_r,sex_m,exp_type,exp_time, 0, 
                 h5details$velocity_non_zero_mean, h5details$velocity_non_zero_sd,
                 h5details$duration_still, h5details$duration_grooming, h5details$duration_moving,
                 h5details$distance_total)
  for (bgroup in c(0:nr_comb_cl)){
    freq = length(tmp$B.SOiD.labels[tmp$B.SOiD.labels==bgroup])
    mdur = 0
    frms = 0
    if (freq > 0) mdur = mean(tmp$Run.lengths[tmp$B.SOiD.labels==bgroup])
    if (freq > 0) frms = sum(tmp$Run.lengths[tmp$B.SOiD.labels==bgroup])
    a = cbind(a, data.frame(frms)) # ignore the mean duration and freq for now, (freq, mdur, frms)
  }
  dt[nrow(dt)+1,] = a 
}

fids = unique(dt$fid)
dt$rec_nr_cor = 0
# Correct the recording numbers (some flies entered the arena at the 2nd recording)
for(fid in fids){
  min_rec_nr = min(dt[dt$fid==fid,'rec_nr'])
  dt[dt$fid==fid,'rec_nr_cor'] = dt[dt$fid==fid,'rec_nr'] - (min_rec_nr-1)
}

dt_all = dt


################### INTRA FLY DIFFERENCES ######################################
intra_diff = dt %>%
  group_by(fid) %>%
  summarise('vel_non_zero_mean' = rrmse(velocity_non_zero_mean[velocity_non_zero_mean>0], mean(dt[dt$duration_velocity_non_zero>0,'velocity_non_zero_mean'])),
            'vel_non_zero_sd' = rrmse(velocity_non_zero_sd[velocity_non_zero_sd>0], mean(dt[dt$duration_velocity_non_zero>0,'velocity_non_zero_sd'])),
            'dur_velocity_non_zero' = rrmse(duration_velocity_non_zero, mean(dt[,'duration_velocity_non_zero'])),
            'distance_total' = rrmse(distance_total, mean(dt[,'distance_total']))) %>%
  as.data.frame()

permuted_vel_non_zero_mean = c()
permuted_vel_non_zero_sd = c()
permuted_dur_velocity_non_zero = c()
permuted_distance_total = c()
for(i in c(1:40)){
  vids = dt[sample(1:233,6),]
  a = rrmse(vids[vids$velocity_non_zero_mean>0,'velocity_non_zero_mean'], mean(vids[vids$velocity_non_zero_mean>0,'velocity_non_zero_mean']))
  b = rrmse(vids[vids$velocity_non_zero_sd>0,'velocity_non_zero_sd'], mean(vids[vids$velocity_non_zero_sd>0,'velocity_non_zero_sd']))
  c = rrmse(vids[,'duration_velocity_non_zero'], mean(vids[,'duration_velocity_non_zero']))
  d = rrmse(vids[,'distance_total'], mean(vids[,'distance_total']))
  permuted_vel_non_zero_mean = c(permuted_vel_non_zero_mean,a)
  permuted_vel_non_zero_sd = c(permuted_vel_non_zero_sd, b)
  permuted_dur_velocity_non_zero = c(permuted_dur_velocity_non_zero, c)
  permuted_distance_total = c(permuted_distance_total, d)
}

t.test(intra_diff$vel_non_zero_mean, permuted_vel_non_zero_mean)
dt_hist = data.frame('value'=c(intra_diff$vel_non_zero_mean, permuted_vel_non_zero_mean), 'group'=c(rep('a',nrow(intra_diff)), rep('b', length(permuted_vel_non_zero_mean))))
p1 = ggplot(dt_hist, aes(x=value, group=group, fill=group)) +
  geom_density(alpha=0.5, show.legend = F) +
  xlab('Non-zero velocity (mean)') + 
  xlim(c(0,150)) + 
  scale_fill_manual(name="",values=c("black","#FF1800"),labels=c("real","perm.")) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())

t.test(intra_diff$vel_non_zero_sd, permuted_vel_non_zero_sd)
dt_hist = data.frame('value'=c(intra_diff$vel_non_zero_sd, permuted_vel_non_zero_sd), 'group'=c(rep('a',nrow(intra_diff)), rep('b', length(permuted_vel_non_zero_sd))))
p2 = ggplot(dt_hist, aes(x=value, group=group, fill=group)) +
  geom_density(alpha=0.5, show.legend = T) +
  xlab('Non-zero velocity (SD)') + 
  xlim(c(0,150)) + 
  scale_fill_manual(name="",values=c("black","#FF1800"),labels=c("real","sim.")) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = c(0.8, 0.8))

t.test(intra_diff$dur_velocity_non_zero, permuted_dur_velocity_non_zero)
dt_hist = data.frame('value'=c(intra_diff$dur_velocity_non_zero, permuted_dur_velocity_non_zero), 'group'=c(rep('a',nrow(intra_diff)), rep('b', length(permuted_dur_velocity_non_zero))))
p3 = ggplot(dt_hist, aes(x=value, group=group, fill=group)) +
  geom_density(alpha=0.5, show.legend = F) +
  xlab('normalized RMSD (%)') + 
  xlim(c(0,150)) + 
  scale_fill_manual(name="",values=c("black","#FF1800"),labels=c("real","perm.")) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())

t.test(intra_diff$distance_total, permuted_distance_total)
dt_hist = data.frame('value'=c(intra_diff$distance_total, permuted_distance_total), 'group'=c(rep('a',nrow(intra_diff)), rep('b', length(permuted_distance_total))))
p4 = ggplot(dt_hist, aes(x=value, group=group, fill=group)) +
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



################### INTER FLY DIFFERENCES ######################################

fly_means = dt %>%
  group_by(fid) %>%
  summarise('vel_non_zero_mean' = mean(velocity_non_zero_mean[duration_velocity_non_zero>0]),
            'vel_non_zero_sd' = mean(velocity_non_zero_sd[duration_velocity_non_zero>0]),
            'dur_velocity_non_zero' = mean(duration_velocity_non_zero),
            'distance_total' = mean(distance_total)) %>%
  as.data.frame()
inter_diff_vel_mean = rrmse(fly_means$vel_non_zero_mean, mean(dt[dt$velocity_non_zero_mean>0,'velocity_non_zero_mean']))
inter_diff_vel_sd = rrmse(fly_means$vel_non_zero_sd, mean(dt[dt$velocity_non_zero_sd>0,'velocity_non_zero_sd']))
inter_diff_vel_dur = rrmse(fly_means$dur_velocity_non_zero, mean(dt[,'duration_velocity_non_zero']))
inter_diff_dist_total = rrmse(fly_means$distance_total, mean(dt[,'distance_total']))

hist(intra_diff$vel_non_zero_mean, xlim=c(0,100), breaks=10)
t.test(intra_diff$vel_non_zero_mean, mu=inter_diff_vel_mean)

hist(intra_diff$vel_non_zero_sd, xlim=c(0,100), breaks=10)
t.test(intra_diff$vel_non_zero_sd, mu=inter_diff_vel_sd)

hist(intra_diff$dur_velocity_non_zero, xlim=c(0,100), breaks=10)
t.test(intra_diff$dur_velocity_non_zero, mu=inter_diff_vel_dur)

hist(intra_diff$distance_total, xlim=c(0,100), breaks=10)
t.test(intra_diff$distance_total, mu=inter_diff_dist_total)

ggplot(intra_diff, aes(x=vel_non_zero_mean)) +
  geom_histogram() +
  xlim(c(0,100)) + 
  geom_vline(aes(xintercept=inter_diff_vel_mean)) +
  theme_minimal()

ggplot(dt[dt$fid%in%fids[c(4,6,35)],], aes(x=velocity_non_zero_mean, fill=fid)) +
  geom_histogram(bins=10) +
  theme_minimal()



################### HIERARCHICAL CLUSTERING ####################################
library(factoextra)
library(cluster)

simple_factors = c('velocity_non_zero_mean',
                   'velocity_non_zero_sd',
                   'duration_velocity_non_zero',
                   #'duration_velocity_zero',
                   'distance_total')
dt_tmp = dt
dt_clust = dt_tmp[,c(simple_factors)]
rownames(dt_clust) = sprintf('%s_%s', dt_tmp$fid, dt_tmp$rec_nr)
dt_clust = scale(dt_clust)
clust <- agnes(dt_clust, diss=F, method = "weighted")
groups <- cutree(clust, k=5)
dt_tmp$clust = groups
tmp = dt_tmp %>% group_by(fid) %>% summarise(nclust = length(unique(clust))) %>% as.data.frame()
table(tmp$nclust)
ggplot(tmp, aes(x=nclust)) + 
  geom_bar(stat='count') + 
  theme_minimal()

res.dist <- get_dist(dt_clust, stand=T, method = "euclidean")

fviz_dist(res.dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

fviz_nbclust(clust, method = "gap_stat")
fviz_dend(clust, k=5)

library(heatmaply)
library(Hmisc)
library(reticulate)
library(dendextend)
corr_plot_vars = heatmaply_cor(dt_clust,   # You need save_image() and the crap below to save a static pdf
                               limits=c(min(dt_clust),max(dt_clust)),
                               node_type = "heatmap",
                               distfun='pearson',
                               #hclust_method = 'ward',
                               show_dendrogram=c(T,F),
                               na.value = '#AADDAA',
                               #key.title = 'Correlation',
                               col = paletteer_c("ggthemes::Classic Red-White-Black", 30),
                              # plot_method='ggplot'
)
corr_plot_vars

# install.packages('reticulate')  # all this shit is needed to run fucking save_image()
# reticulate::install_miniconda()
# reticulate::conda_install('r-reticulate', 'python-kaleido')
# reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
# reticulate::use_miniconda('r-reticulate')
# reticulate::py_run_string("import sys")
save_image(corr_plot_vars, file=sprintf('%s/%s',path_output,'corr_plot_vars.svg'),width=700 ,height = 3000)#


### Heatmap + dendrogram
library(colorBlindness)
dt_clust = fly_means[,c(2,3,4)]
rownames(dt_clust) = fly_means$fid
dt_clust = scale(dt_clust)
method='complete'
k = 6
clust = agnes(dt_clust, diss=F, method =method)
clust = as.dendrogram(clust)
clust = color_branches(clust, k=k, col = paletteer_c("ggthemes::Classic Red-Black", 6, direction = -1))
#clust = reorder(clust, c(1:40))
clust = reorder(clust, c(17:40, 1:3, 5:16))
corr_plot_vars = heatmaply_cor(dt_clust,   # You need save_image() and the crap below to save a static pdf
                               limits=c(min(dt_clust),max(dt_clust)),
                               node_type = "heatmap",
                               #hclust_method = 'single',
                               Rowv = as.dendrogram(clust),
                               show_dendrogram=c(T,F),
                               na.value = '#AADDAA',
                               #key.title = 'Correlation',
                               col = paletteer_c("ggthemes::Classic Red-White-Black", 30),
                               plot_method='ggplot',
                               branches_lwd = 0.9
)
corr_plot_vars

### Compute the stats for each cluster
dt_fly_single = fly_means[,c(1,2,3,4)]
groups <- cutree(clust, k=k)
dt_fly_single$clust = letters[c(2, 6, 5, 4, 1, 3)][groups] # reorder based on how they appear in the plot
tmp = dt_fly_single %>% group_by(clust) %>% summarise(mean_vel = mean(vel_non_zero_mean), 
                                             mean_sd_vel = mean(vel_non_zero_sd),
                                             mean_duration_move = mean(dur_velocity_non_zero)/90,
                                             count = n())%>% as.data.frame()
tmp
speed_conversion = 4.56852789 # pixels/frame to mm/second
tmp[,c(2,3)] = tmp[,c(2,3)] * speed_conversion
sprintf('%0.2f - %0.2f', mean(fly_means$vel_non_zero_mean), sd(fly_means$vel_non_zero_mean))
sprintf('%0.2f - %0.2f', mean(fly_means$vel_non_zero_sd), sd(fly_means$vel_non_zero_sd))
sprintf('%0.2f - %0.2f', mean(fly_means$dur_velocity_non_zero), sd(fly_means$dur_velocity_non_zero))
sprintf('%0.2f - %0.2f', mean(fly_means$distance_total), sd(fly_means$distance_total))

### Compute how well we would predict the correct/same cluster for each recording of an individual fly
dt_tmp = dt
dt_clust = dt_tmp[,c(simple_factors)]
rownames(dt_clust) = sprintf('%s_%s', dt_tmp$fid, dt_tmp$rec_nr)
dt_clust = scale(dt_clust)
clust <- agnes(dt_clust, diss=F, method =method)
groups <- cutree(clust, k=k)
dt_tmp$clust = groups
tmp = dt_tmp %>% group_by(fid) %>% summarise(nclust = length(unique(clust)), 
                                             clust_same = max(table(clust)),
                                             count = n(),
                                             clust_per = max(table(clust))/n())%>% as.data.frame()
hist(tmp$clust_per, xlim=c(0,1), breaks=5)
mean(tmp$clust_per)

table(tmp$nclust)
ggplot(tmp, aes(x=nclust)) + 
  geom_bar(stat='count') + 
  theme_minimal()
# ward 6 not bad
# complete 5 with mean, sd, dist

# install.packages('reticulate')  # all this shit is needed to run fucking save_image()
# reticulate::install_miniconda()
# reticulate::conda_install('r-reticulate', 'python-kaleido')
# reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
# reticulate::use_miniconda('r-reticulate')
#reticulate::py_run_string("import sys")
save_image(corr_plot_vars, file=sprintf('%s/%s',path_output,'corr_plot_vars_single2.pdf'),width=500 ,height = 800)#



################### PLOTS ######################################################
#dt = dt_all[dt_all$exp_type=='1a' & dt_all$time_of_day=='afternoon',]
dt_molten = melt(dt[,c(1,2,8,4,7,c(9,10,11,13))], id=c('fid','date','rec_nr_cor','l_r','time_of_day'))
dt_molten[dt_molten$variable=='velocity_non_zero_mean','value'] = dt_molten[dt_molten$variable=='velocity_non_zero_mean','value'] * 27000 / max(dt_molten[dt_molten$variable=='velocity_non_zero_mean','value'])
dt_molten[dt_molten$variable=='velocity_non_zero_sd','value'] = dt_molten[dt_molten$variable=='velocity_non_zero_sd','value'] * 27000 / max(dt_molten[dt_molten$variable=='velocity_non_zero_sd','value'])
dt_molten[dt_molten$variable=='distance_total','value'] = dt_molten[dt_molten$variable=='distance_total','value'] * 27000 / max(dt_molten[dt_molten$variable=='distance_total','value'])
#dt_molten$variable = substring(dt_molten$variable, 6)
#dt_molten$variable = factor(dt_molten$variable, levels=c(0:nr_comb_cl))
dt_molten$variable_nr = as.numeric(dt_molten$variable)
dt_molten$value = dt_molten$value/27000
dt_molten$date = substring(dt_molten$date, 6)
dt_molten$time_of_day = sprintf('%s.',substring(dt_molten$time_of_day, 0,3))

# CHECK IF EACH RECORDING STILL HAS 27000 FRAMES
tmp = dt_molten %>% 
  group_by(fid,rec_nr) %>% 
  summarise(nr_frames = sum(value)) %>%
  as.data.frame()
tmp[,c('fid','rec_nr','nr_frames')]
a = dt_molten[dt_molten$fid==fids[12] & dt_molten$rec_nr_cor==5,]
a
sum(a$value)


ggplot(dt_molten, aes(x=variable, y=value, fill=rec_nr_cor, group=rec_nr_cor)) +
  geom_bar(stat='identity',position='dodge', show.legend = F) +
  geom_vline( aes(xintercept=variable, group=rec_nr_cor), col='red')

library(paletteer)
# FACET GRID
ggplot(dt_molten,aes(x=variable_nr, y=value, group=rec_nr_cor)) +
  #geom_bar(data=dt_molten, mapping=aes(x=variable, y=value, fill=rec_nr_cor, group=rec_nr_cor), stat='identity',position='dodge', show.legend = F) + +
  geom_rect(aes(xmin=variable_nr-.5, xmax=variable_nr+0.5, ymin=0, ymax=1, group=interaction(variable_nr), fill=mod((variable_nr-1),4)*-1.5), show.legend=F) +
  #geom_hline(aes(yintercept=0), col='#333333', size=0.3, linetype=2) +
  #geom_hline(aes(yintercept=0.25), col='#333333', size=0.3, linetype=2) +
  geom_hline(aes(yintercept=0.5), col='#333333', size=0.3, linetype=2) +
  #geom_hline(aes(yintercept=0.75), col='#333333', size=0.3, linetype=2) +
  #geom_vline(aes(xintercept=variable,group=c(rec_nr_cor)), col='red') +
  #geom_vline(aes(xintercept=variable, group=c(fid)), col='#00000033', linewidth=10) +
  #geom_vline(data=dt_molten,mapping=aes(xintercept=variable, group=c(rec_nr_cor)), col=c('#00000044','white'), size=5) +
  geom_bar(aes(fill=11-rec_nr_cor), stat='identity',position='dodge', show.legend = F) +
  scale_x_continuous(breaks=dt_molten$variable_nr, labels=dt_molten$variable) +
  scale_y_continuous(breaks=c(0,0.5,1)) +
  #scale_fill_viridis_c(option = "plasma", end = 0.7) +
  #scale_fill_paletteer_c("grDevices::Grays",direction=-1) +
  #scale_color_viridis_c(option = "plasma", end = 0.7) +
  scale_fill_gradient2(low='blue',mid='white',high='black') +
  #ylim(c(0,1)) +
  theme_linedraw() +
  labs(x='', y='Frames') +
  theme(plot.title = element_text(size=9),
        axis.title = element_text(size=9),
        axis.text = element_text(size=8),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family="sans"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()
        #panel.grid.major.y = element_line(color='#333333', linetype='99'),
        #panel.grid.major.x = element_line(color='#33333333',size=4),
        ) +
  facet_grid(rows=vars(date,time_of_day),cols=vars(l_r))


ggsave(sprintf('%s/tmp/fly_behavior_frames_comb.pdf',path_output), bg='white',dpi = 300, width = 6, height=10)
ggsave(sprintf('%s/tmp/fly_behavior_frames_comb.png',path_output), bg='white',dpi = 300, width = 6, height=12)

pdf(file=sprintf('%s/tmp/fly_behavior_frames_comb.pdf',path_output), width = 8, height=20)
print(p)
dev.off()




################### PCA ########################################################
#dt_frms = dt[,seq(11,ncol(dt),3)]
tmp = dt[dt$sex_m==1,]
dt_frms = tmp[,simple_factors]
dt_frms = dt_frms[,colSums(dt_frms)>0]
pc = prcomp(dt_frms, center=T, scale.=T)
print(pc)

plot(pc, type = "l")

p = ggbiplot(pc, groups=tmp[,'fid'], choices = c(1,2), obs.scale = 1, var.scale = 1, ellipse = T, var.axes = T, circle = F) + 
  scale_color_discrete(name = '') + 
  theme(legend.direction = 'horizontal', 
        legend.position = 'top') + 
  theme_minimal()

pdf(file=sprintf('%s/tmp/pca_flies.pdf',path_output), width = 8, height=5.5)
print(p)
dev.off()

ggbiplot(pc, groups=dt$fid, choices = c(3,4), obs.scale = 1, var.scale = 1, ellipse = TRUE, var.axes = FALSE, circle = F) + 
  scale_color_discrete(name = '') + 
  theme(legend.direction = 'horizontal', 
        legend.position = 'top') + 
  theme_minimal()
#ggsave(sprintf('%s/tmp/pca_flies.svg',path_output), bg='white',dpi = 300, width=6, height=4.5)





################### TEST MEUK ##################################################
dt_molten[dt_molten$variable==0,]
fd = 'l_2022-11-16_2_4'

tmp = read.csv(sprintf('%s/Mar-16-2023bout_lengths_90Hz%s.csv', path_data, fd), header=T, sep =",", )
tmp = tmp[tmp$Run.lengths>10,]
tmp = tmp %>% 
  group_by(grp = rleid(B.SOiD.labels), B.SOiD.labels) %>% 
  summarise(len = sum(Run.lengths), .groups = 'drop', start=first(Start.time..frames.)) %>%
  as.data.frame()
tmp$start = sprintf('%s:%02.0f', floor((tmp$start/90)/60), mod(round(tmp$start/90), 60))

tmp2 = tmp %>% group_by(B.SOiD.labels) %>% summarize(sm=sum(Run.lengths), cn=n()) %>% as.data.frame()
tmp2
tmp[tmp$B.SOiD.labels==22,]



################### PLOT WALKED PATH ###########################################

h5file = sprintf('%s/%s.h5', path_tracks, 'l_2022-10-12_1_1')
tracks = h5read(h5file,'tracks')
tracks_x = tracks[,1,1,1] # time frame, body part,x/y, Fly id
tracks_y = tracks[,1,2,1]
plot(tracks_x, tracks_y, col='#ff000066',pch=16,cex=0.5,asp=1)
thorax_x = tracks[,2,1,1] # time frame, body part,x/y, Fly id
thorax_y = tracks[,2,2,1]
thorax_xy = tracks[,2,,1]
points(thorax_x, thorax_y, col='#00000066',pch=16,cex=0.5)
tracks_x = tracks[,3,1,1] # time frame, body part,x/y, Fly id
tracks_y = tracks[,3,2,1]
points(tracks_x, tracks_y, col='#00aa0066',pch=16,cex=0.5)



######################### CONVERT H5 TO CSV FOR VAME ###########################

files = list.files(path_data, '*.csv')
for(file in files){
  date = substring(file, 31, 40)
  session = substring(file, 42, 42)
  l_r = substring(file, 29, 29)
  rec_nr = substring(file, 44,44)
  file_id = sprintf('%s_%s_%s_%s', l_r, date, session, rec_nr)
  h5file = sprintf('%s/%s.h5', path_tracks, file_id)
  tracks = h5read(h5file,'tracks')
  tracks_csv = data.frame(0:(length(tracks[,1,1,1])-1), 
                          tracks[,1,1,1], tracks[,1,2,1], rep(1,length(tracks[,1,1,1])),
                          tracks[,2,1,1], tracks[,2,2,1], rep(1,length(tracks[,1,1,1])),
                          tracks[,3,1,1], tracks[,3,2,1], rep(1,length(tracks[,1,1,1])),
                          tracks[,4,1,1], tracks[,4,2,1], rep(1,length(tracks[,1,1,1])),
                          tracks[,5,1,1], tracks[,5,2,1], rep(1,length(tracks[,1,1,1])),
                          tracks[,6,1,1], tracks[,6,2,1], rep(1,length(tracks[,1,1,1])),
                          tracks[,7,1,1], tracks[,7,2,1], rep(1,length(tracks[,1,1,1])),
                          tracks[,8,1,1], tracks[,8,2,1], rep(1,length(tracks[,1,1,1])),
                          tracks[,9,1,1], tracks[,9,2,1], rep(1,length(tracks[,1,1,1])),
                          tracks[,10,1,1], tracks[,10,2,1], rep(1,length(tracks[,1,1,1])),
                          tracks[,11,1,1], tracks[,11,2,1], rep(1,length(tracks[,1,1,1])), 
                          tracks[,12,1,1], tracks[,12,2,1], rep(1,length(tracks[,1,1,1])), 
                          tracks[,13,1,1], tracks[,13,2,1], rep(1,length(tracks[,1,1,1])), 
                          c(tracks[1,2,1,1], tracks[1:(length(tracks[,1,1,1])-1),2,1,1]), c(tracks[1,2,2,1], tracks[1:(length(tracks[,1,1,1])-1),2,2,1]), rep(1,length(tracks[,1,1,1])) )
  colnames(tracks_csv) = c('coords',
                           'x','y','likelihood',
                           'x','y','likelihood',
                           'x','y','likelihood',
                           'x','y','likelihood',
                           'x','y','likelihood',
                           'x','y','likelihood',
                           'x','y','likelihood',
                           'x','y','likelihood',
                           'x','y','likelihood',
                           'x','y','likelihood',
                           'x','y','likelihood',
                           'x','y','likelihood',
                           'x','y','likelihood',
                           'x','y','likelihood')
  # interpolate missing head or thorax data because we need those for egocentric alignment in VAME
  # NOTE: VAME ALREADY DOES INTERPOLATION
  # if(any(is.na(tracks_csv[,c(2,3,5,6)]))){
  #   tracks_csv = na.approx(tracks_csv) #Note: this doesn't catch NAs if they are in the first or last row
  #   if(any(is.na(tracks_csv[,c(2,3,5,6)]))) print(sprintf('NAs in %s', file))
  # }
  # If limbs are missing for the entire duration of the video, set them to the thorax location (otherwise VAME can't process them)
  for(i in c(seq(8,43,3))){
    if(all(is.na(tracks_csv[,i]))){
      tracks_csv[,i] = tracks_csv[,5]
      tracks_csv[,i+1] = tracks_csv[,6]
    }
  }
  write.csv(tracks_csv, file=sprintf('%s/video_%s.csv', path_tracks_csv, file_id), sep=',',row.names=F,quote=F)
}

file_id = 'l_2022-10-12_1_1'
date = '2022-10-12'
session = 1
rec_nr = 1
l_r = 'l'
h5file = sprintf('%s/%s.h5', path_tracks, file_id)
tracks = h5read(h5file,'tracks')
tracks_csv = data.frame(0:(length(tracks[,1,1,1])-1), 
              tracks[,1,1,1], tracks[,1,2,1], rep(1,length(tracks[,1,1,1])),
              tracks[,2,1,1], tracks[,2,2,1], rep(1,length(tracks[,1,1,1])),
              tracks[,3,1,1], tracks[,3,2,1], rep(1,length(tracks[,1,1,1])),
              tracks[,4,1,1], tracks[,4,2,1], rep(1,length(tracks[,1,1,1])),
              tracks[,5,1,1], tracks[,5,2,1], rep(1,length(tracks[,1,1,1])),
              tracks[,6,1,1], tracks[,6,2,1], rep(1,length(tracks[,1,1,1])),
              tracks[,7,1,1], tracks[,7,2,1], rep(1,length(tracks[,1,1,1])),
              tracks[,8,1,1], tracks[,8,2,1], rep(1,length(tracks[,1,1,1])),
              tracks[,9,1,1], tracks[,9,2,1], rep(1,length(tracks[,1,1,1])),
              tracks[,10,1,1], tracks[,10,2,1], rep(1,length(tracks[,1,1,1])),
              tracks[,11,1,1], tracks[,11,2,1], rep(1,length(tracks[,1,1,1])), 
              tracks[,12,1,1], tracks[,12,2,1], rep(1,length(tracks[,1,1,1])), 
              tracks[,13,1,1], tracks[,13,2,1], rep(1,length(tracks[,1,1,1])), 
              c(tracks[1,2,1,1], tracks[1:(length(tracks[,1,1,1])-1),2,1,1]), c(tracks[1,2,2,1], tracks[1:(length(tracks[,1,1,1])-1),2,2,1]), rep(1,length(tracks[,1,1,1])) )
colnames(tracks_csv) = c('coords',
                         'x','y','likelihood',
                         'x','y','likelihood',
                         'x','y','likelihood',
                         'x','y','likelihood',
                         'x','y','likelihood',
                         'x','y','likelihood',
                         'x','y','likelihood',
                         'x','y','likelihood',
                         'x','y','likelihood',
                         'x','y','likelihood',
                         'x','y','likelihood',
                         'x','y','likelihood',
                         'x','y','likelihood',
                         'x','y','likelihood')
write.csv(tracks_csv, file=sprintf('%s/video_%s.csv', path_tracks_csv, file_id), sep=',',row.names=F,quote=F)
# h5details = GetH5Details(date, session, rec_nr, l_r)
# speeds = data.frame(0:(length(tracks[,1,1,1])-1), h5details$speeds) #pixels per timeframe
# colnames(speeds) = c('frame','speed')
# write.csv(speeds, file=sprintf('%s/video_%s.csv', path_speed_csv, file_id), sep=',',row.names=F,quote=F)


################################################################################
######################### FUNCTIONS ############################################
################################################################################

GetH5Details = function(date, session, rec_nr, l_r){
  # body parts:
  # 1: head
  # 2: thorax
  # 3: abdomen
  # 4-9: legs
  # 10 & 11: wings
  # 12 & 13: eyes
  tracks = GetTracks(date, session, rec_nr, l_r)
  dist_thorax = GetH5Distances(tracks, 2)
  
  # Get the speed of the fastest leg (should we look at wing speed too?)
  dist_limbs_max = numeric(length(dist_thorax))
  for (i_l in c(4,5,6,7,8,9)){
    dist_limbs = GetH5Distances(tracks, i_l)
    dist_limbs_max = pmax(dist_limbs_max, dist_limbs)
  }
  
  dist_thorax_non_zero = dist_thorax[dist_thorax>=non_zero]
  dist_thorax_zero = dist_thorax[dist_thorax<non_zero]
  
  nr_frames_still = sum(dist_thorax<non_zero & dist_limbs_max < non_zero)
  nr_frames_grooming = sum(dist_thorax<non_zero & dist_limbs_max >= non_zero)
  nr_frames_moving = sum(dist_thorax>=non_zero & dist_limbs_max >= non_zero)
  nr_frames_other = sum(dist_thorax>=non_zero & dist_limbs_max < non_zero) # thorax is moving but the limbs aren't? not sure what to make of this
  
  #plot(1:length(dist),dist,type='l',ylim=c(0,30))
  #hist(dist, breaks=300, xlim=c(0,15))
  
  # Unit are pixels per time frame
  vl_mean = if_else(length(dist_thorax_non_zero)==0, 0, mean(dist_thorax_non_zero))
  vl_sd = if_else(length(dist_thorax_non_zero)==0, 0, sd(dist_thorax_non_zero))
  dt = data.frame('velocity_non_zero_mean'=vl_mean,
                  'velocity_non_zero_sd'=vl_sd,
                  'duration_still'=nr_frames_still,
                  'duration_grooming'=nr_frames_grooming,
                  'duration_moving'=nr_frames_moving,
                  'distance_total'=sum(dist_thorax_non_zero))
  return(dt)
}

GetH5Distances = function(tracks, body_part){
  # Perform additional smoothing step on the thorax
  # The exact pixel may have been slighly different between different video frames causing a "shake"
  if(body_part == id_thorax){
    window_size=5
    total_time_frames = length(tracks[,1,1,])
    tracks[3:(total_time_frames-2),id_thorax,id_x,] = zoo::rollmean(tracks[,id_thorax,id_x,], k=window_size, align='center', fill=list(NULL, NA, NULL))
    tracks[3:(total_time_frames-2),id_thorax,id_y,] = zoo::rollmean(tracks[,id_thorax,id_y,], k=window_size, align='center', fill=list(NULL, NA, NULL))
  }
  
  # print(body_part)
  xy_coords = tracks[,body_part,,1]
  
  xy_coords = as.data.frame(xy_coords)
  
  # INTERPOLATE NAs
  if(all(is.na(xy_coords))) # IF WE NEVER FOUND THIS BODY PART SET ALL TO 0
    xy_coords = data.frame(matrix(0, nrow = nrow(xy_coords), ncol = ncol(xy_coords)))
  xy_coords = as.data.frame(na.approx(xy_coords))
  colnames(xy_coords) = c('x','y')
  
  # SMOOTH THE COORDINATES AS THE CAMERA + TRACKING SOFTWARE INTRODUCES JITTER 
  window_size=5 
  xy_coords$x <- zoo::rollmean(xy_coords$x, k = window_size, align='center', fill = NA)
  xy_coords$y <- zoo::rollmean(xy_coords$y, k = window_size, align='center', fill = NA)
  
  # Compute the distances between each time frame
  xy_coords = xy_coords[complete.cases(xy_coords),]
  dist <- sqrt(diff(xy_coords$x)^2 + diff(xy_coords$y)^2)
  
  return(dist)
}

GetH5DistancesPlusMotion = function(date, session, rec_nr, l_r){
  tracks = GetTracks(date, session, rec_nr, l_r)
  tracks = GetTracksSmoothed(tracks)
  dist_thorax = GetH5Distances(tracks, 2)
  
  # Get the speed of the fastest leg/wing
  dist_limbs_max = numeric(length(dist_thorax))
  for (i_l in c(4,5,6,7,8,9,10,11)){
    dist_limbs = GetH5Distances(tracks, i_l)
    dist_limbs_max = pmax(dist_limbs_max, dist_limbs)
  }
  
  dt = data.frame('dist_thorax'=dist_thorax,'dist_limbs_max'=dist_limbs_max,'motion_type'=rep_len('unknown',length(dist_thorax)))
  
  dt[dist_thorax<non_zero & dist_limbs_max < non_zero,'motion_type'] = 'still'
  dt[dist_thorax<non_zero & dist_limbs_max >= non_zero,'motion_type'] = 'grooming'
  dt[dist_thorax>=non_zero & dist_limbs_max >= non_zero,'motion_type'] = 'moving'
  
  return(dt)
}

GetH5DistancesPlusMotion(date,session,rec_nr, l_r)

GetTracks = function(date,session,rec_nr, l_r){
  h5file = sprintf('%s/%s_%s_%s_%s.h5', path_tracks, l_r, date, session, rec_nr)
  tracks = h5read(h5file,'tracks') #  time frame, body part, [x coords, y coords], Fly id
  return(tracks)
}

GetTracksStabilized = function(date,session,rec_nr,l_r){
  tracks = GetTracks(date,session,rec_nr, l_r)
  tracks[,,id_x,] = tracks[,,id_x,] - tracks[,id_thorax,id_x,]
  tracks[,,id_y,] = tracks[,,id_y,] - tracks[,id_thorax,id_y,]
  # Calculate the angle of rotation in radians
  angle <- atan2(tracks[,id_head,id_x,],tracks[,id_head,id_y,])
  
  for(i in c(1,3,4,5,6,7,8,9,10,11,12,13)){
    tracks[,i,,] = rotate(tracks[,i,,], angle)
  }
  
  return(tracks)
}

GetTracksSmoothed = function(tracks){
  # Perform additional smoothing step on the thorax
  # The exact pixel may have been slighly different between different video frames causing a "shake"
  for(i in c(1:13)){
    window_size=5
    total_time_frames = length(tracks[,1,1,])
    tracks[3:(total_time_frames-2),i,id_x,] = zoo::rollmean(tracks[,i,id_x,], k=window_size, align='center', fill=list(NULL, NA, NULL))
    tracks[3:(total_time_frames-2),i,id_y,] = zoo::rollmean(tracks[,i,id_y,], k=window_size, align='center', fill=list(NULL, NA, NULL))
  }
  
  return(tracks)
}

PlotFly = function(tracks, t, scale){
  tracks1x = tracks[t,,1,] # time frame, body part,x/y, Fly id
  tracks1y = tracks[t,,2,]
  
  lines_data = data.frame(from_x=c(tracks1x[2], tracks1x[2], tracks1x[2], tracks1x[2], tracks1x[2], tracks1x[2], tracks1x[2], tracks1x[2], tracks1x[2], tracks1x[2]), 
                          from_y=c(tracks1y[2], tracks1y[2], tracks1y[2], tracks1y[2], tracks1y[2], tracks1y[2], tracks1y[2], tracks1y[2], tracks1y[2], tracks1y[2]), 
                          to_x= c(tracks1x[1], tracks1x[3], tracks1x[4], tracks1x[5], tracks1x[6], tracks1x[7], tracks1x[8], tracks1x[9], tracks1x[10], tracks1x[11]), 
                          to_y= c(tracks1y[1], tracks1y[3], tracks1y[4], tracks1y[5], tracks1y[6], tracks1y[7], tracks1y[8], tracks1y[9], tracks1y[10], tracks1y[11]),
                          width=(c(4, 4, 1, 1, 1, 1, 1, 1, 4, 4)/2) * scale,
                          clr=c('black','black','black','black','black','black','black','black','grey','grey'), stringsAsFactors = F)
  p <- ggplot(lines_data, aes(x = from_x, y = from_y, xend = to_x, yend = to_y, linewidth=width, color=clr)) +
    geom_segment() + 
    # xlim(c(1,1000)) +
    # ylim(c(1,1000)) +
    scale_color_identity() +
    scale_linewidth_identity() +
    coord_fixed() + 
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank())
  return(p)
}
PlotFly_old = function(date,session,rec_nr, l_r, time_frame){
  tracks = GetTracks(date,session,rec_nr, l_r)
  tracks1x = tracks[time_frame,,1,] # time frame, body part,x/y, Fly id
  tracks1y = tracks[time_frame,,2,]
  lines(c(tracks1x[2], tracks1x[1]), c(tracks1y[2], tracks1y[1]), lwd=5) # thorax => head
  lines(c(tracks1x[2], tracks1x[3]), c(tracks1y[2], tracks1y[3]), lwd=5) # thorax => abdomen
  lines(c(tracks1x[2], tracks1x[4]), c(tracks1y[2], tracks1y[4])) # thorax => legs
  lines(c(tracks1x[2], tracks1x[5]), c(tracks1y[2], tracks1y[5]))
  lines(c(tracks1x[2], tracks1x[6]), c(tracks1y[2], tracks1y[6]))
  lines(c(tracks1x[2], tracks1x[7]), c(tracks1y[2], tracks1y[7]))
  lines(c(tracks1x[2], tracks1x[8]), c(tracks1y[2], tracks1y[8]))
  lines(c(tracks1x[2], tracks1x[9]), c(tracks1y[2], tracks1y[9]))
  lines(c(tracks1x[2], tracks1x[10]), c(tracks1y[2], tracks1y[10]), lwd=5, col='gray') # thorax => wings
  lines(c(tracks1x[2], tracks1x[11]), c(tracks1y[2], tracks1y[11]), lwd=5, col='gray')
  points(tracks1x[2], tracks1y[2], pch=16, col='red', cex=0.5) # thorax
  points(tracks1x[1], tracks1y[1], pch=16, col='#00DD00', cex=0.5) # head
  points(tracks1x[3], tracks1y[3], pch=16, col='#00AAFF', cex=0.5) # abdomen
  
}

PlotFlyStabilized_old = function(date,session,rec_nr, l_r, time_frame){
  tracks = GetTracksStabilized(date,session,rec_nr, l_r)
  tracks1x = tracks[time_frame,,1,] # time frame, body part,x/y, Fly id
  tracks1y = tracks[time_frame,,2,]
  lines(c(tracks1x[2], tracks1x[1]), c(tracks1y[2], tracks1y[1]), lwd=5) # thorax => head
  lines(c(tracks1x[2], tracks1x[3]), c(tracks1y[2], tracks1y[3]), lwd=5) # thorax => abdomen
  lines(c(tracks1x[2], tracks1x[4]), c(tracks1y[2], tracks1y[4])) # thorax => legs
  lines(c(tracks1x[2], tracks1x[5]), c(tracks1y[2], tracks1y[5]))
  lines(c(tracks1x[2], tracks1x[6]), c(tracks1y[2], tracks1y[6]))
  lines(c(tracks1x[2], tracks1x[7]), c(tracks1y[2], tracks1y[7]))
  lines(c(tracks1x[2], tracks1x[8]), c(tracks1y[2], tracks1y[8]))
  lines(c(tracks1x[2], tracks1x[9]), c(tracks1y[2], tracks1y[9]))
  lines(c(tracks1x[2], tracks1x[10]), c(tracks1y[2], tracks1y[10]), lwd=5, col='gray') # thorax => wings
  lines(c(tracks1x[2], tracks1x[11]), c(tracks1y[2], tracks1y[11]), lwd=5, col='gray')
  points(tracks1x[2], tracks1y[2], pch=16, col='red', cex=0.5) # thorax
  points(tracks1x[1], tracks1y[1], pch=16, col='#00DD00', cex=0.5) # head
  points(tracks1x[3], tracks1y[3], pch=16, col='#00AAFF', cex=0.5) # abdomen
}

# Define a function to perform the rotation
rotate = function(coords, angle) {
  a = coords
  a[,1] = coords[,1] * cos(angle) - coords[,2] * sin(angle)
  a[,2] = coords[,1] * sin(angle) + coords[,2] * cos(angle)
  return(a)
}

# Compute the relative root mean square error
# basically root mean square error as a percentage of the mean value in the 
# entire data set
rmse = function(a){
  rmse_a = sqrt(sum((a - mean(a))^2)/(length(a)-1))
  #rmse_a = sqrt(sum((a-mean(a))^2)/length(a)) # same as above but this is how mathematicians write it
  return(rmse_a)
}
rrmse = function(a, data_mean){
  rrmse_a = rmse(a) * (1/data_mean) * 100
  return(rrmse_a)
}

# Create a function to calculate the mode within a window
mode_within_window <- function(x) {
  unique_vals <- unique(x)
  count_vals <- sapply(unique_vals, function(val) sum(x == val))
  unique_vals[which.max(count_vals)]
}


######################### CREATE A VIDEO #######################################
date = '2022-10-11'
session=1
rec_nr = 5
l_r = 'l'
t=500

tracks = GetTracks(date, session, rec_nr, l_r)
tracks_stabilized = GetTracksStabilized(date, session, rec_nr, l_r)
tracks_stabilized_smoothed = GetTracksSmoothed(tracks_stabilized)
dist_motion = GetH5DistancesPlusMotion(date, session, rec_nr, l_r)
max_time_frames = nrow(dist_motion)
dist_motion$time_frame = as.numeric(row.names(dist_motion))
dist_motion$motion_type2 = dist_motion$motion_type#, levels=c('still','grooming','moving')
window_size=13
dist_motion$motion_type2[7:(max_time_frames-6)] = rollapply(dist_motion$motion_type2, width = window_size, FUN = mode_within_window, align = "center", fill=list(NULL, NA, NULL))
dist_motion$motion_type_fact2 = factor(dist_motion$motion_type2, levels=c('still','grooming','moving'))

video_name = sprintf('%s/%s_%s_%s_%s_combined2.mp4', path_output, l_r, date, session, rec_nr)
theme_set(theme_classic(base_size=20))
saveVideo(
    for (t in c(1:8000)){
      p1 = PlotFly(tracks, t, 1)
      if(l_r == 'l')
        p1 = p1 + lims(x=c(1,1000), y=c(1,1000))
      if(l_r == 'r')
        p1 = p1 + lims(x=c(1001,2000), y=c(1001,2000))

      p2 = PlotFly(tracks_stabilized_smoothed, t, 3)
      p2 = p2 + lims(x=c(-80,80), y=c(-80,80))
      
      
      p3 = ggplot(data = dist_motion[max(1, min(t-250, max_time_frames-500)):max(min(t+250, max_time_frames),500),], aes(x=time_frame, y=motion_type_fact2, group=1)) +
        geom_line() +
        scale_fill_discrete(drop=FALSE) +
        scale_y_discrete(drop=FALSE) +
        geom_vline(xintercept = t, col='red') +
        xlab("Time Frame") + 
        theme(axis.title.x=element_blank(),
              axis.text.x = element_blank(),
              axis.title.y=element_blank())
      
      p4 = ggplot(data = dist_motion[max(1, min(t-250, max_time_frames-500)):max(min(t+250, max_time_frames),500),], aes(x=time_frame, y=dist_thorax)) +
        geom_line() +
        geom_vline(xintercept = t, col='red') +
        ylim(c(0,max(dist_motion$dist_thorax))) +
        xlab("Time Frame") + 
        ylab("Velocity (pxl/frame)") +
        theme()
      
      stats1 = data.frame('name'=c('Moving', 'Grooming', 'Still'),
                          'vals'=c(nrow(dist_motion[dist_motion$time_frame<=t & dist_motion$motion_type=='moving',]),
                         nrow(dist_motion[dist_motion$time_frame<=t & dist_motion$motion_type=='grooming',]),
                         nrow(dist_motion[dist_motion$time_frame<=t & dist_motion$motion_type=='still',])), stringsAsFactors = T)
      stats1$name = with(stats1, factor(name, levels = c('Still','Grooming','Moving')))
      p6 = ggplot(stats1, aes(x=name, y=vals, fill=name))+
        geom_bar(stat='identity', show.legend = F) + 
        coord_flip() +
        ylab('Nr. Frames') +
        theme(axis.title.y=element_blank())
      
      if(length(dist_motion[dist_motion$time_frame<=t & dist_motion$motion_type=='moving','dist_thorax']) == 0){
        mean_speed = 0
        sd_speed = 0
      } else {
        mean_speed = mean(dist_motion[dist_motion$time_frame<=t & dist_motion$motion_type=='moving','dist_thorax'])
        sd_speed = sd(dist_motion[dist_motion$time_frame<=t & dist_motion$motion_type=='moving','dist_thorax'])
      }
      stats2 = data.frame('name'=c('Mean move speed','SD move speed'),
                          'vals'=c(mean_speed, sd_speed))
      p7 = ggplot(stats2, aes(x=name, y=vals, fill=name))+
        geom_bar(stat='identity', show.legend = F) + 
        coord_flip() +
        ylim(c(0,10)) +
        ylab('Velocity (pxl/frame)') +
        theme(axis.title.y=element_blank())
      
      ggarrange(p1, p2, p3, p6, p4, p7, ncol=2, heights = c(1, 0.3,0.3))
      print(t)
  },
  video.name = video_name,
  ffmpeg = ani.options("ffmpeg"),
  other.opts = if (grepl("[.]mp4$", video_name)) "-pix_fmt yuv420p",
  interval = 0.0111,
  ani.width=960,
  ani.height=1080
)

distances_motion = GetH5DistancesPlusMotion(date, session, rec_nr, l_r)
tracks = GetTracks(date, session, rec_nr, l_r)
tracks_stabilized = GetTracksStabilized(date, session, rec_nr, l_r)
plot(x=seq(1,t), as.factor(distances_motion$motion_type[1:t]), type='l')#, xlim=c(1,nrow(distances_motion)))
table(distances_motion$motion_type)

# Speed
plot(x=seq(1,t), y=distances_motion$dist_thorax[1:t], type='l')
hist(distances_motion$dist_thorax, breaks=seq(0,40,0.1), xlim=c(0,5))






################### EMPTY SPACE ################################################






# FIX NANs FIRST USING INTERPOLATION
if(all(is.na(xy_coords))) # IF WE NEVER FOUND THIS BODY PART SET ALL TO 0
  xy_coords = data.frame(matrix(0, nrow = nrow(xy_coords), ncol = ncol(xy_coords)))

print(first(xy_coords))
print( first(xy_coords[complete.cases(xy_coords),]))
print(last(xy_coords))
print(last(xy_coords[complete.cases(xy_coords),]))

if(any(is.na(first(xy_coords)))) # MAKE SURE THE FIRST  AND LAST ROWS ARE NOT NA, OTHERWISE WE CAN'T INTERPOLATE
  xy_coords[1,] = first(xy_coords[complete.cases(xy_coords),])
if(any(is.na(last(xy_coords)))) # MAKE SURE THE FIRST  AND LAST ROWS ARE NOT NA, OTHERWISE WE CAN'T INTERPOLATE
  xy_coords[nrow(xy_coords),] = last(xy_coords[complete.cases(xy_coords),])
i = 1
print('a2')


print(first(xy_coords))
print(first(xy_coords[complete.cases(xy_coords),]))
print(last(xy_coords))
print(last(xy_coords[complete.cases(xy_coords),]))


while(any(is.na(xy_coords))){
  xy_coords = xy_coords %>% mutate(
    across(where(is.numeric),
           ~if_else(!is.na(.), ., (dplyr::lead(.) + dplyr::lag(., i)) / 2)
    )
  )
  i=i+1
  if(i %% 100 == 1)
    print(i)
}

dist = xy_coords %>% mutate(
  across(where(is.numeric), ~((dplyr::lead(., offset) - dplyr::lag(., offset))/(offset*2) ))
)


dist[1:(offset+1),] = cbind(seq(0, dist[offset+1,1], length.out=offset+1),seq(0, dist[offset+1,2], length.out=offset+1))
dist[(nrow(dist)-offset):nrow(dist),] = cbind(seq(dist[nrow(dist)-offset,1], 0, length.out=offset+1),seq(dist[nrow(dist)-offset,2], 0, length.out=offset+1))








time_frame = 1
tracks = GetTracks(date,session,rec_nr, l_r)

tracks[,,id_x,] = tracks[,,id_x,] - tracks[,id_thorax,id_x,]
tracks[,,id_y,] = tracks[,,id_y,] - tracks[,id_thorax,id_y,]
tracks_tf = tracks[time_frame,,,] # timeframe 1

# Calculate the angle of rotation in radians
angle <- atan2(tracks_tf[id_head,id_x],tracks_tf[id_head,id_y])

for(i in c(1,3,4,5,6,7,8,9,10,11,12,13)){
  tracks[,i,,] = rotate(tracks[,i,,], angle)
}


# Rotate the Head coordinate
new_head <- rotate(head, angle)
new_leg1 = rotate(leg1, angle)
new_leg2 = rotate(leg2, angle)

plot(rbind(center, head, leg1, leg2), asp=1)
plot(rbind(center, new_head, new_leg1, new_leg2), asp=1)
# Print the rotated coordinates
print(new_head)



z <- zoo(c(NA, 2, NA, 1, NA, 5, 2, NA))
na.fill(z, "extend")
na.fill(z, c("extend", NA))
na.fill(z, -(1:3))
na.fill(z, list(NULL, NA, NULL))
na.fill(z, NULL)


# Create a data frame with start and end points for multiple lines, line widths, and colors
lines_data <- data.frame(
  x_start = c(1, 2, 3),
  y_start = c(1, 2, 3),
  x_end = c(4, 3, 2),
  y_end = c(4, 2, 1),
  line_width = c(1, 2, 3),
  line_color = c("black", "blue", "green")  # Line colors for each line
)

# Create a ggplot object and add a geom_segment layer with line width and color
p <- ggplot(lines_data, aes(x = x_start, y = y_start, xend = x_end, yend = y_end, size = line_width, color = line_color)) +
  geom_segment()

# Customize the plot (e.g., add labels, titles, themes)
p + labs(x = "X-axis", y = "Y-axis", title = "Multiple Lines Plot") +
  scale_size_continuous(range = c(0.5, 3)) +  # Adjust the size range as needed
  scale_color_identity()





# Sample data frame with a categorical color column
df <- data.frame(
  Time = 1:12,
  Color = c("red", "red", "yellow", "red", "red", 
            "red", "blue", "yellow", "yellow", "yellow", 
            "red","yellow")
)

# Perform run-length encoding on the Color column
rle_result <- rle(df$Color)

# Identify and smooth sequences of consecutive values with a length of 1
j = 1
for(i in rle_result$lengths){
  if(i == 1){
    index = sum(rle_result$lengths[c(1:j)])
    print(df$Color[index])
    df$Color[index] = df$Color[index-1]
  }
  j = j+1
}

# Doesn't work properly:
for (i in seq_along(rle_result$values)) {
  if (rle_result$lengths[i] == 1) {
    index <- which(df$Color == rle_result$values[i])
    if (index > 1 && index < nrow(df)) {
      print(index)
      df$Color[index] <- df$Color[c(index - 1)]
      rle_result <- rle(df$Color)
    }
  }
}

# Print the data frame with smoothed colors
print(df)




# Sample data frame with a categorical column
df <- data.frame(
  Time = 1:10,
  Category = c("A", "A", "A", "B", "A", "A", "A", "C", "B", "A")
)

# Define the window size
window_size <- 5


# Calculate the moving mode using zoo::rollapply
smoothed_categories <- rollapply(df$Category, width = window_size, FUN = mode_within_window, align = "center", fill = NA)

# Create a data frame with the smoothed data
smoothed_df <- data.frame(Time = df$Time, Smoothed_Category = smoothed_categories)

# Print the smoothed data frame
print(smoothed_df)
