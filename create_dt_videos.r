# Process the tracking information for each video and append behavioral data

script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(script_dir)

source("packages.r")
source("constants.r")
source("basic_functions.r")

GetDtVideos = function() {
  if (file_test("-f", sprintf('%s/%s',path_data, 'dt_videos.csv'))) {
    dt_videos = read.table(sprintf('%s/%s',path_data, 'dt_videos.csv'), sep='\t',header=T)
  } else {
    dt_videos = CreateDtVideos()
  }
  return(dt_videos)
}


CreateDtVideos = function() {
  # Details of each experiment
  exp_details = read.csv('./data/tests/data.csv', header=T, sep='\t')
  
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
  files = list.files(path_sleap, '*.h5')
  
  # Loop through the SLEAP files and append the data to our data frame
  for (file in files){
    #`file = "r_2022-11-01_1_5.h5"
    file_no_ext = strsplit(file, '[.]')[[1]][1]
    file_no_ext_split = strsplit(file_no_ext, '_')[[1]]
    date = file_no_ext_split[2]
    session = file_no_ext_split[3] # Morning or afternoon
    l_r = file_no_ext_split[1]
    rec_nr = file_no_ext_split[4]
    
    # Generate a "fly ID"
    fid = sprintf('%s_%s_%s', date, session, l_r)
    
    print(fid)
    
    exp_detail = exp_details[exp_details$date==date & exp_details$couple_id==session & exp_details$testnr==rec_nr,]
    sex_m = tolower(exp_detail[sprintf('sex_%s', l_r)]) == 'm'
    exp_type = tolower(exp_detail$test_type)
    exp_time = 'morning'
    if(as.numeric(substring(exp_detail$time_rec,1,2)) >= 12) exp_time = 'afternoon'
    
    # Compute the behavior
    motion_details = GetH5DistancesPlusMotion(date, session, rec_nr, l_r)
    
    # Aggregate results
    duration_still = nrow(motion_details[motion_details$motion_type == 'still',]) / nrow(motion_details)
    duration_grooming = nrow(motion_details[motion_details$motion_type == 'grooming',]) / nrow(motion_details)
    duration_moving = nrow(motion_details[motion_details$motion_type == 'moving',]) / nrow(motion_details)
    velocity_non_zero_mean = mean(motion_details[motion_details$motion_type == 'moving', 'dist_thorax'])
    velocity_non_zero_sd = sd(motion_details[motion_details$motion_type == 'moving', 'dist_thorax'])
    distance_total = sum(motion_details[motion_details$motion_type == 'moving', 'dist_thorax'])
    
    a = data.frame(fid, date, as.numeric(rec_nr), l_r, sex_m, exp_type, exp_time, 0, 
                   velocity_non_zero_mean, 
                   velocity_non_zero_sd,
                   duration_still, 
                   duration_grooming, 
                   duration_moving,
                   distance_total)
    
    dt_videos[nrow(dt_videos)+1,] = a 
  }
  
  # Correct the recording numbers (some flies entered the arena at the 2nd recording)
  fids = unique(dt_videos$fid)
  dt_videos$rec_nr_cor = 0
  for(fid in fids){
    min_rec_nr = min(dt_videos[dt_videos$fid==fid,'rec_nr'])
    dt_videos[dt_videos$fid==fid,'rec_nr_cor'] = dt_videos[dt_videos$fid==fid,'rec_nr'] - (min_rec_nr-1)
  }
  
  write.table(dt_videos, './data/dt_videos.csv', quote=F, sep="\t", row.names=F)
  
  return(dt_videos)
}

