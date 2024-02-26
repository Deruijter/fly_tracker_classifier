# This code contains the analysis of the fly behavior as tracked using SLEAP
# Author: Markus de Ruijter, 2023

script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(script_dir)

source("packages.r")
source("constants.r")
source("basic_functions.r")
source("create_dt_videos.r")
source('plots.r')
source('create_video.r')

dt_videos = GetDtVideos()

CreateVideo('2022-10-11','1','5','l')
CreateVideo('2022-10-12','1','1','l')

CreateVideo('2022-11-16','2','6','l')


date = '2022-11-16'
session = '2'
rec_nr = '5'
l_r = 'r'
tracks = GetTracks(date, session, rec_nr, l_r)
tracks_stabilized = GetTracksStabilized(date, session, rec_nr, l_r)
tracks_stabilized_smoothed = GetTracksSmoothed(tracks_stabilized)
dist_motion = GetH5DistancesPlusMotion(date, session, rec_nr, l_r)
hist(dist_motion[dist_motion$motion_type!='moving', 'dist_limbs_max'], breaks = 500)
abline(v=1.25)
abline(v=1)
abline(v=1.2)

max_time_frames = nrow(dist_motion)
dist_motion$time_frame = as.numeric(row.names(dist_motion))
dist_motion$motion_type2 = dist_motion$motion_type#, levels=c('still','grooming','moving')
window_size=13
dist_motion$motion_type2[7:(max_time_frames-6)] = rollapply(dist_motion$motion_type2, width = window_size, FUN = mode_within_window, align = "center", fill=list(NULL, NA, NULL))
dist_motion$motion_type_fact2 = factor(dist_motion$motion_type2, levels=c('still','grooming','moving'))



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
