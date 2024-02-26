# This script examines differences between different flies

script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(script_dir)

source("packages.r")
source("constants.r")
source("basic_functions.r")
source("create_dt_videos.r")

dt = GetDtVideos()


fly_means = dt %>%
  group_by(fid) %>%
  summarise('velocity_non_zero_mean' = mean(velocity_non_zero_mean[duration_moving>0.01]),
            'velocity_non_zero_sd' = mean(velocity_non_zero_sd[duration_moving>0.01]),
            'duration_still' = mean(duration_still),
            'duration_grooming' = mean(duration_grooming),
            'duration_moving' = mean(duration_moving),
            'distance_total' = mean(distance_total),
            'sex_m' = first(sex_m),
            'l_r' = first(l_r),
            'time_of_day' = first(time_of_day),
            'exp_type' = first(exp_type)) %>%
  as.data.frame()
inter_diff_vel_mean = rrmse(fly_means$velocity_non_zero_mean, mean(dt[dt$velocity_non_zero_mean>0,'velocity_non_zero_mean'], na.rm = T))
inter_diff_vel_sd = rrmse(fly_means$velocity_non_zero_sd, mean(dt[dt$velocity_non_zero_sd>0,'velocity_non_zero_sd'], na.rm=T))
inter_diff_dur_stl = rrmse(fly_means$duration_still, mean(dt[,'duration_still']))
inter_diff_dur_grm = rrmse(fly_means$duration_grooming, mean(dt[,'duration_grooming']))
inter_diff_dur_mov = rrmse(fly_means$duration_moving, mean(dt[,'duration_moving']))
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

# simple_factors = c('velocity_non_zero_mean',
#                    'velocity_non_zero_sd',
#                    'duration_grooming',
#                    'duration_moving',
#                    'distance_total')
# dt_tmp = dt[complete.cases(dt),]
# dt_clust = dt_tmp[,c(simple_factors)]
# rownames(dt_clust) = sprintf('%s_%s', dt_tmp$fid, dt_tmp$rec_nr)
# dt_clust = scale(dt_clust)

# clust <- agnes(dt_clust, diss=F, method = "weighted")
# groups <- cutree(clust, k=5)
# dt_tmp$clust = groups
# tmp = dt_tmp %>% group_by(fid) %>% summarise(nclust = length(unique(clust))) %>% as.data.frame()
# table(tmp$nclust)
# ggplot(tmp, aes(x=nclust)) + 
#   geom_bar(stat='count') + 
#   theme_minimal()
# 
# res.dist <- get_dist(dt_clust, stand=T, method = "euclidean")
# 
# fviz_dist(res.dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
# 
# fviz_nbclust(clust, method = "gap_stat")
# fviz_dend(clust, k=5)

library(heatmaply)
library(Hmisc)
library(reticulate)
library(dendextend)
# corr_plot_vars = heatmaply_cor(dt_clust,   # You need save_image() and the crap below to save a static pdf
#                                limits=c(min(dt_clust),max(dt_clust)),
#                                node_type = "heatmap",
#                                distfun='pearson',
#                                #hclust_method = 'ward',
#                                show_dendrogram=c(T,F),
#                                na.value = '#AADDAA',
#                                #key.title = 'Correlation',
#                                col = paletteer_c("ggthemes::Classic Red-White-Black", 30),
#                                # plot_method='ggplot'
# )
# corr_plot_vars
# 
# # install.packages('reticulate')  # all this shit is needed to run fucking save_image()
# # reticulate::install_miniconda()
# # reticulate::conda_install('r-reticulate', 'python-kaleido')
# # reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
# # reticulate::use_miniconda('r-reticulate')
# # reticulate::py_run_string("import sys")
# save_image(corr_plot_vars, file=sprintf('%s/%s',path_output,'corr_plot_vars.svg'),width=700 ,height = 3000)#


### Heatmap + dendrogram
library(colorBlindness)
dt_clust = fly_means[,c(4,5,6,2,3)]
rownames(dt_clust) = fly_means$fid
dt_clust = scale(dt_clust)
method='complete'
k = 5
clust = agnes(dt_clust, diss=F, method =method)
clust = as.dendrogram(clust)
clust = color_branches(clust, k=k, col = paletteer_c("ggthemes::Classic Red-Black", 5, direction = 1))
#clust = reorder(clust, c(1:40))
#clust = reorder(clust, c(17:40, 1:3, 5:16)) # Make the gradient more apparent
corr_plot_vars = heatmaply_cor(dt_clust,   # You need save_image() and the crap below to save a static pdf
                               limits=c(min(min(dt_clust), -max(dt_clust)), max(-min(dt_clust), max(dt_clust))),
                               node_type = "heatmap",
                               #hclust_method = 'single',
                               Rowv = as.dendrogram(clust),
                               Colv = NA, # Disable clustering on the columns so we maintain our original column order
                               show_dendrogram=c(T,F),
                               na.value = '#AADDAA',
                               #key.title = 'Correlation',
                               col = paletteer_c("ggthemes::Classic Red-White-Black", 30, direction=-1),
                               plot_method='ggplot',
                               branches_lwd = 0.9,
                               #Colv = as.dendrogram(c('duration_still','duration_grooming','duration_moving','velocity_non_zero_mean','velocity_non_zero_sd'))
)
corr_plot_vars
# install.packages('reticulate')  # all this shit is needed to run fucking save_image()
# reticulate::install_miniconda()
# reticulate::conda_install('r-reticulate', 'python-kaleido')
# reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
# reticulate::use_miniconda('r-reticulate')
# reticulate::py_run_string("import sys")
save_image(corr_plot_vars, file=sprintf('%s/%s',path_output,'corr_plot_vars.svg'),width=700 ,height = 3000)#





### Compute the stats for each cluster
dt_fly_single = fly_means
groups <- cutree(clust, k=k)
dt_fly_single$clust = letters[c(2, 6, 5, 4, 1, 3)][groups] # reorder based on how they appear in the plot
tmp = dt_fly_single %>% group_by(clust) %>% summarise(mean_vel = mean(velocity_non_zero_mean), 
                                                      mean_sd_vel = mean(velocity_non_zero_sd),
                                                      mean_duration_stl = mean(duration_still, na.rm=T),
                                                      mean_duration_grm = mean(duration_grooming, na.rm=T),
                                                      mean_duration_mov = mean(duration_moving, na.rm=T),
                                                      count = n())%>% as.data.frame()
tmp
speed_conversion = 4.56852789 # approximate pixels/frame to mm/second
tmp[,c(2,3)] = tmp[,c(2,3)] * speed_conversion
sprintf('%0.2f - %0.2f', mean(fly_means$velocity_non_zero_mean), sd(fly_means$velocity_non_zero_mean))
sprintf('%0.2f - %0.2f', mean(fly_means$velocity_non_zero_sd), sd(fly_means$velocity_non_zero_sd))
sprintf('%0.2f - %0.2f', mean(fly_means$duration_still, na.rm=T), sd(fly_means$duration_still, na.rm=T))
sprintf('%0.2f - %0.2f', mean(fly_means$duration_grooming, na.rm=T), sd(fly_means$duration_grooming, na.rm=T))
sprintf('%0.2f - %0.2f', mean(fly_means$duration_moving, na.rm=T), sd(fly_means$duration_moving, na.rm=T))
sprintf('%0.2f - %0.2f', mean(fly_means$distance_total), sd(fly_means$distance_total))

# Group by sex
tmp = fly_means %>% group_by(sex_m) %>% summarise(mean_vel = mean(velocity_non_zero_mean), 
                                                      mean_sd_vel = mean(velocity_non_zero_sd),
                                                      mean_duration_stl = mean(duration_still, na.rm=T),
                                                      mean_duration_grm = mean(duration_grooming, na.rm=T),
                                                      mean_duration_mov = mean(duration_moving, na.rm=T),
                                                      count = n())%>% as.data.frame()
tmp
tmp = fly_means
tmp$sex_m = as.character(tmp$sex_m)
ggplot(tmp, aes(x=duration_moving, group=sex_m, fill=sex_m)) +
  geom_density(alpha=0.5) +
  lims(x=c(0,1))
ggplot(tmp, aes(x=duration_grooming, group=sex_m, fill=sex_m)) +
  geom_density(alpha=0.5) +
  lims(x=c(0,1))
ggplot(tmp, aes(x=duration_still, group=sex_m, fill=sex_m)) +
  geom_density(alpha=0.5) +
  lims(x=c(0,1))
ggplot(tmp, aes(x=velocity_non_zero_mean, group=sex_m, fill=sex_m)) +
  geom_density(alpha=0.5)
ggplot(tmp, aes(x=velocity_non_zero_sd, group=sex_m, fill=sex_m)) +
  geom_density(alpha=0.5)

# Group by experiment type
tmp = fly_means %>% group_by(exp_type) %>% summarise(mean_vel = mean(velocity_non_zero_mean), 
                                                  mean_sd_vel = mean(velocity_non_zero_sd),
                                                  mean_duration_stl = mean(duration_still, na.rm=T),
                                                  mean_duration_grm = mean(duration_grooming, na.rm=T),
                                                  mean_duration_mov = mean(duration_moving, na.rm=T),
                                                  count = n())%>% as.data.frame()
tmp
tmp = fly_means
ggplot(tmp, aes(x=duration_moving, group=exp_type, fill=exp_type)) +
  geom_density(alpha=0.5) +
  lims(x=c(0,1))
ggplot(tmp, aes(x=duration_grooming, group=exp_type, fill=exp_type)) +
  geom_density(alpha=0.5) +
  lims(x=c(0,1))
ggplot(tmp, aes(x=duration_still, group=exp_type, fill=exp_type)) +
  geom_density(alpha=0.5) +
  lims(x=c(0,1))
ggplot(tmp, aes(x=velocity_non_zero_mean, group=exp_type, fill=exp_type)) +
  geom_density(alpha=0.5)
ggplot(tmp, aes(x=velocity_non_zero_sd, group=exp_type, fill=exp_type)) +
  geom_density(alpha=0.5)

# Group by time of day
tmp = fly_means %>% group_by(time_of_day) %>% summarise(mean_vel = mean(velocity_non_zero_mean), 
                                                     mean_sd_vel = mean(velocity_non_zero_sd),
                                                     mean_duration_stl = mean(duration_still, na.rm=T),
                                                     mean_duration_grm = mean(duration_grooming, na.rm=T),
                                                     mean_duration_mov = mean(duration_moving, na.rm=T),
                                                     count = n())%>% as.data.frame()
tmp
tmp = fly_means
ggplot(tmp, aes(x=duration_moving, group=time_of_day, fill=time_of_day)) +
  geom_density(alpha=0.5) +
  lims(x=c(0,1))
ggplot(tmp, aes(x=duration_grooming, group=time_of_day, fill=time_of_day)) +
  geom_density(alpha=0.5) +
  lims(x=c(0,1))
ggplot(tmp, aes(x=duration_still, group=time_of_day, fill=time_of_day)) +
  geom_density(alpha=0.5) +
  lims(x=c(0,1))
ggplot(tmp, aes(x=velocity_non_zero_mean, group=time_of_day, fill=time_of_day)) +
  geom_density(alpha=0.5)
ggplot(tmp, aes(x=velocity_non_zero_sd, group=time_of_day, fill=time_of_day)) +
  geom_density(alpha=0.5)

# Group by left / right chamber
tmp = fly_means %>% group_by(l_r) %>% summarise(mean_vel = mean(velocity_non_zero_mean), 
                                                        mean_sd_vel = mean(velocity_non_zero_sd),
                                                        mean_duration_stl = mean(duration_still, na.rm=T),
                                                        mean_duration_grm = mean(duration_grooming, na.rm=T),
                                                        mean_duration_mov = mean(duration_moving, na.rm=T),
                                                        count = n())%>% as.data.frame()
tmp
tmp = fly_means
ggplot(tmp, aes(x=duration_moving, group=l_r, fill=l_r)) +
  geom_density(alpha=0.5) +
  lims(x=c(0,1))
ggplot(tmp, aes(x=duration_grooming, group=l_r, fill=l_r)) +
  geom_density(alpha=0.5) +
  lims(x=c(0,1))
ggplot(tmp, aes(x=duration_still, group=l_r, fill=l_r)) +
  geom_density(alpha=0.5) +
  lims(x=c(0,1))
ggplot(tmp, aes(x=velocity_non_zero_mean, group=l_r, fill=l_r)) +
  geom_density(alpha=0.5)
ggplot(tmp, aes(x=velocity_non_zero_sd, group=l_r, fill=l_r)) +
  geom_density(alpha=0.5)


ggplot(tmp[tmp$exp_type =='1a',], aes(x=duration_moving, group=paste(exp_type, sex_m), fill=paste(exp_type, sex_m))) +
  geom_histogram(alpha=0.5)


m = glm('duration_still ~ sex_m + exp_type + time_of_day', data=tmp)
summary(m)
m = glm('duration_grooming ~ sex_m + exp_type + time_of_day', data=tmp)
summary(m)
m = glm('duration_moving ~ sex_m + exp_type + time_of_day', data=tmp)
summary(m)
m = glm('velocity_non_zero_mean ~ sex_m + exp_type + time_of_day', data=tmp)
summary(m)
m = glm('velocity_non_zero_sd ~ sex_m + exp_type + time_of_day', data=tmp)
summary(m)


# Check the groups and see if they are more or less homogeneos
fly_means %>% group_by(l_r) %>% count(sex_m)
fly_means %>% group_by(l_r) %>% count(exp_type)
fly_means %>% group_by(l_r) %>% count(time_of_day)

fly_means %>% group_by(exp_type) %>% count(l_r)
fly_means %>% group_by(exp_type) %>% count(sex_m)
fly_means %>% group_by(exp_type) %>% count(time_of_day)

fly_means %>% group_by(sex_m) %>% count(l_r)
fly_means %>% group_by(sex_m) %>% count(exp_type)
fly_means %>% group_by(sex_m) %>% count(time_of_day)



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



