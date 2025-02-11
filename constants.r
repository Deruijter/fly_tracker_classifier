# Consider any velocity under this value as zero (based on histogram visualization).
# Reason for this is that the camera will always record some kind of speed due to vibrations/conversion of the videos and stuff like that
non_zero = 0.4 # Unit: pixels per time frame, this is for the thorax, which is relatively stable
non_zero_limbs = 1.2 # Unit: pixels per time frame, for the limbs, which have more jitter

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

# Paths
path_data = './data'
path_sleap = './data/sleap_exports_h5'
path_sleap_csv = './data/sleap_exports_csv'
path_output = './output'