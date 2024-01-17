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
library(stringr)

script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(script_dir)

# Consider any velocity under this value as zero (based on histogram visualization).
# Reason for this is that the camera will always record some kind of speed due to vibrations/conversion of the videos and stuff like that
non_zero = 0.4 # Unit: pixels per time frame

# Indexes of the different body parts of the flies
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
# Indexes of the coordinates
id_x = 1
id_y = 2

path_sleap = './data/sleap_exports'
path_sleap_csv = './data/sleap_exports_csv'
path_output = './output'

# Details of each experiment
exp_details = read.csv('../data/tests/data.csv', header=T, sep='\t')

source("basic_functions.r")
source("create_video.r")

# Initialize a data set for all the data related to specific videos
dt_videos = data.frame('fid'=character(),
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



################### CREATE THE DATA SET ########################################
files = list.files(path_sleap_csv, '*.csv')

# Loop through the SLEAP files and append the data to our data frame
for (file in files){
  file = "video_l_2022-10-11_1_1.csv"
  file_no_ext = strsplit(files, '[.]')[[1]][1]
  file_no_ext_split = strsplit(file_no_ext, '_')[[1]]
  date = file_no_ext_split[3]
  session = file_no_ext_split[4] # Morning or afternoon
  l_r = file_no_ext_split[2]
  rec_nr = file_no_ext_split[5]
  
  print(sprintf('%s_%s_%s', date, session, l_r))
  
  # Generate a "fly ID"
  fid = sprintf('%s_%s_%s', date, session, l_r)
  
  exp_detail = exp_details[exp_details$date==date & exp_details$couple_id==session & exp_details$testnr==rec_nr,]
  sex_m = tolower(exp_detail[sprintf('sex_%s', l_r)]) == 'm'
  exp_type = tolower(exp_detail$test_type)
  exp_time = 'morning'
  if(as.numeric(substring(exp_detail$time_rec,1,2)) >= 12) exp_time = 'afternoon'
  
  # Ok now we need to append the behavioral data
  # % still, grooming, moving
  # SD velocity, mean velocity, total distance
  # Preferably we create a new file that has each frame + the behavior data: behavior type (S, G, M) and speed (from that we can compute the total distance, mean and SD velocity)
  
  h5details = GetH5Details(date, session, rec_nr, l_r)
  
  a = data.frame(fid,date,as.numeric(rec_nr),l_r,sex_m,exp_type,exp_time, 0, 
                 h5details$velocity_non_zero_mean, h5details$velocity_non_zero_sd,
                 h5details$duration_still, h5details$duration_grooming, h5details$duration_moving,
                 h5details$distance_total)
  
  dt_videos[nrow(dt_videos)+1,] = a 
}

# Correct the recording numbers (some flies entered the arena at the 2nd recording)
fids = unique(dt$fid)
dt$rec_nr_cor = 0
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
