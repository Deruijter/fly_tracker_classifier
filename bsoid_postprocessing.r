# Some code that process the output generated by BSOID
# BSOID: https://bsoid.org/
#
# Note: I've had some trouble getting BSOID to produce usable results so this
# script hasn't been worked on a lot (might contain some errors after 
# refactoring)

path_bsoid_data = './data/bsoid'

################### CREATE THE DATA SET ########################################
nr_clusters=3-1 #-1 because the numbering starts at 0
nr_comb_cl = 3 - 1
files = list.files(path_bsoid_data, '*.csv')
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

# Create a column for each BSOID cluster
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
  tmp = read.csv(sprintf('%s/%s', path_bsoid_data, file), header=T, sep =",", )
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



################### TEST MEUK ##################################################
dt_molten[dt_molten$variable==0,]
fd = 'l_2022-11-16_2_4'

tmp = read.csv(sprintf('%s/Mar-16-2023bout_lengths_90Hz%s.csv', path_bsoid_data, fd), header=T, sep =",", )
tmp = tmp[tmp$Run.lengths>10,]
tmp = tmp %>% 
  group_by(grp = rleid(B.SOiD.labels), B.SOiD.labels) %>% 
  summarise(len = sum(Run.lengths), .groups = 'drop', start=first(Start.time..frames.)) %>%
  as.data.frame()
tmp$start = sprintf('%s:%02.0f', floor((tmp$start/90)/60), mod(round(tmp$start/90), 60))

tmp2 = tmp %>% group_by(B.SOiD.labels) %>% summarize(sm=sum(Run.lengths), cn=n()) %>% as.data.frame()
tmp2
tmp[tmp$B.SOiD.labels==22,]
