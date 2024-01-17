# Some code that process the output generated by VAME
# VAME: https://github.com/LINCellularNeuroscience/VAME/
#
# Note: I've had some trouble getting VAME to produce usable results so this
# script hasn't been worked on a lot (might contain some errors after 
# refactoring)

# CONVERT H5 (SLEAP) TO CSV (for VAME)

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

