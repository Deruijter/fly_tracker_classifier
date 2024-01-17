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

