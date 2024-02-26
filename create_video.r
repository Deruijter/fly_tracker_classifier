# Create a video with some informational plots for a specific fly recording
# TODO: Want to re-write this so it uses the motion data as input


CreateVideo = function(date, session, rec_nr, l_r) {

  tracks = GetTracks(date, session, rec_nr, l_r)
  #date = "2022-10-11"
  #session = '1'
  #rec_nr = '1'
  #l_r = 'l'
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
    for (t in c(1:max_time_frames)){
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
}
