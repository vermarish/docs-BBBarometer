# Get waveforms (peaks) from a ZERO-CENTERED signal at a set of times
# In: signal: a tibble with two columns: time and data
#     time: a vector of doubles.
# Ou: list of waveforms. (each waveform is itself represented as a list)
pull_waveforms <- function(signal, times) {
  features <- list()
  
  for (time in times) {
    center <- which(signal$time > time)[1]
    polarity <- sign(signal$data[center])
    left <- center
    right <- center
    
    while (left > 1 && sign(signal$data[left - 1]) == polarity) {
      left <- left - 1
    }
    while (right < nrow(signal) && sign(signal$data[right + 1]) == polarity) {
      right <- right + 1
    }
    feature <- signal$data[left:right]
    features[[length(features)+1]] = feature
  }
  return(features)
}



# Mine features from gyroscope data and pressure data at given times
# In: Input a dataframe with gyroscope data and a touch_confidence dataframe
# Ou: feature tibble containing each wave
mine_features <- function(data, touch_confidence, times) {
  # Mining features with pull_waveforms works better when delayed by 7-10 samples, to get closer to the center of a peak.
  gyro <- data %>% filter(type=="gyroscope") %>% arrange(time)
  delay <- 10
  period <- gyro$time %>% diff %>% mean
  gyro_times <- times + delay*period
  
  # mine each waveform
  gyro_one <- pull_waveforms(signal=gyro %>% select(time=time, data=one),
                             times=gyro_times)
  gyro_two <- pull_waveforms(signal=gyro %>% select(time=time, data=two),
                             times=gyro_times)
  gyro_three <- pull_waveforms(signal=gyro %>% select(time=time, data=three),
                               times=gyro_times)
  # mine the extremum of each waveform as a feature
  extremum_one <- gyro_one %>% sapply(function(waveform) {sign(waveform[[1]][1]) * max(abs(waveform[[1]]))})
  extremum_two <- gyro_two %>% sapply(function(waveform) {sign(waveform[[1]][1]) * max(abs(waveform[[1]]))})
  extremum_three <- gyro_three %>% sapply(function(waveform) {sign(waveform[[1]][1]) * max(abs(waveform[[1]]))})
  
  
  pressure_data <- touch_confidence %>% arrange(time)
  delay <- 2  # pressure spike usually happens two samples after a touch event
  period <- pressure$time %>% diff %>% mean
  pressure_times <- times + delay*period
  
  i <- 1
  extremum_pressure <- c()
  extremum_p_model <- c()
  for (time in times) {
    # for each time, get 2 pressure values before it and 5 pressure values after it
    index <- which(pressure_data$time > time)[1]
    values <- 
      # put the peak in the features
      extremum_pressure[i] <- max(pressure_data$one[(index-2):(index+4)])
    extremum_p_model[i] <- max(pressure_data$p[(index-2):(index+4)])
    i <- i + 1
  }
  
  return(tibble(gyro_one, gyro_two, gyro_three, 
                extremum_one, extremum_two, extremum_three,
                extremum_pressure, extremum_p_model,
                time=times))
}


# Given a dataset containing sensor and (touch | touch_predict), create a dataframe 
# to predict touch location from sensor features.
# In: data tibble with gyroscope and touch entries.
#     touch can be "touch" with x/y in headers $one $two. use labeled=TRUE.
#     or touch can be "touch_guess" with no x/y. use labeled=FALSE.
# Ou: if labeled=TRUE, collect the waveform and features with x/y labels.
# Ou: if labeled=FALSE, collect the waveform and features
build_df_touch <- function(data, touch_confidence, labeled=TRUE) {
  data <- data %>% arrange(time)
  
  ## Use gyro and touch data to mine features
  if (labeled) {
    times <- data %>%
      filter(type == "touch") %>%
      .$time
  } else {
    times <- data %>% 
      filter(type == "touch_predict") %>%
      .$time
  }
  features <- mine_features(data, touch_confidence, times)
  
  ## Add labels
  if (labeled) {
    labels <- data %>%
      filter(type=="touch") %>%
      select(x=one, y=two) %>%
      mutate(u=x-540, v=1200-y) %>%
      mutate(r=sqrt(u^2 + v^2),
             d=pmin(540-abs(u),1200-abs(v)),
             m=abs(u)+abs(v)) %>%
      mutate(col = 1*(x<380) + 2*(380<x&x<700) + 3*(700<x),
             row = 1*(y<1550) + 2*(1550<y&y<1680) + 3*(1680<y&y<1880) + 4*(1880<y)) %>%
      mutate(digit = ifelse(row==4, 0, col+3*row-3)) %>%
      mutate(digit = as.factor(digit)) %>%
      select(-col, -row, -u, -v)
    
    labels %>%
      ggpairs(mapping=aes(color=digit),
              columns=c("x","y","r","d","m"))
    
    features <- features %>% bind_cols(labels)
  }
#   
#   TODO I've got to label this data by row, col, number.
#     (c1)  (c3)
# (r1)  1  2  3    
#       4  5  6
#       7  8  9
# (r4)     0       
# 
# c1-c2-c3 breaks at 380 and 700
# 
# r1-r2-r3-r4 breaks at 1550, 1680, 1880
# 
# then the digit at row $r < 4$ and col $c$ is $$c+ 3r - 3$$,
# and the digit at row 4 is 0.

  
  return(features)
}
