# Smaller functions used to aggregate data / do some simple calculations


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
    if(length(dist_limbs) > 0)
      dist_limbs_max = pmax(dist_limbs_max, dist_limbs)
  }
  
  dt = data.frame('dist_thorax'=dist_thorax,'dist_limbs_max'=dist_limbs_max,'motion_type'=rep_len('unknown',length(dist_thorax)))
  
  dt[dist_thorax<non_zero & dist_limbs_max < non_zero_limbs,'motion_type'] = 'still'
  dt[dist_thorax<non_zero & dist_limbs_max >= non_zero_limbs,'motion_type'] = 'grooming'
  dt[dist_thorax>=non_zero,'motion_type'] = 'moving'
  
  return(dt)
}

GetTracks = function(date,session,rec_nr, l_r){
  h5file = sprintf('%s/%s_%s_%s_%s.h5', path_sleap, l_r, date, session, rec_nr)
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