# Putting various plots in here



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
