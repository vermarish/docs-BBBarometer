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
  extremum_one <- gyro_one %>% sapply(function(waveform) {sign(waveform[[1]][1]) * max(abs(waveform))})
  extremum_two <- gyro_two %>% sapply(function(waveform) {sign(waveform[[1]][1]) * max(abs(waveform))})
  extremum_three <- gyro_three %>% sapply(function(waveform) {sign(waveform[[1]][1]) * max(abs(waveform))})
  
  
  pressure_data <- touch_confidence %>% arrange(time)
  delay <- 2  # pressure spike usually happens two samples after a touch event
  period <- pressure_data$time %>% diff %>% median
  pressure_times <- times + delay*period
  
  i <- 1
  extremum_pressure <- c()
  extremum_p_model <- c()
  for (time in pressure_times) {
    # for each time, get 2 pressure values before it and 5 pressure values after it
    index <- which(pressure_data$time > time)[1]
    
    # put the peak in the features
    extremum_pressure[i] <- max(pressure_data$one[(index-2):(index+4)])
    extremum_p_model[i] <- max(pressure_data$p[(index-2):(index+4)])
    i <- i + 1
  }
  
  return(tibble(#gyro_one, gyro_two, gyro_three, 
                extremum_one, extremum_two, extremum_three,
                extremum_pressure, extremum_p_model,
                time=times))
}




# OKAY NOW WE'RE REDOING mine_features()
# uncomment for debugging
times <- data %>% filter(type=="touch_predict") %>% pull(time)

# In: dataframe with columns `time` and `signal`
# Out: A named 
features_from_windowed_gyro <- function(data) {
  
}


# Mine features from gyroscope data and pressure data at given times
# In: Input a dataframe with gyroscope data and a touch_confidence dataframe
# Out: feature tibble containing each wave
mine_features <- function(data, touch_confidence, times) {
  gyro <- data %>% filter(type=="gyroscope")
  
  period <- gyro$time %>% diff %>% median
  lead <- 20*period  # we know touch_predicts tend to happen 1 or 2 pressure samples after touch events
                     # which is about 15 or 30 gyroscope samples
  times_gyro <- times - lead
  
  features <- matrix(data=NA,
                     nrow=length(times_gyro),
                     ncol=9)#,
                     
                     #dimnames=list(NULL,c("foo", "bar")))
  
  # need the bottom height, top height, t width, subsequent data points (?)
  
  for (t_i in 1:length(times_gyro)) {
    t = times_gyro[t_i]
    open = t-period*25
    close = t+period*25
    windowed_signal = gyro %>% filter(time > open & time < close)
    cols = c("one", "two", "three")
    for (c in 1:3) {
      col = cols[c]
      waveform = windowed_signal %>% 
        select(time=time, signal=col) %>%
        mutate(energy = abs(lead(signal,2)-lag(signal,2)),
               diff = lead(signal,1) - signal,
               i = 1:nrow(waveform))
      # # to view
      # waveform %>% ggplot(aes(x=time)) + geom_point(aes(y=signal), color="black") + geom_point(aes(y=diff), color="blue")
      
      sample_center = waveform %>% 
        mutate(i = 1:nrow(waveform)) %>%
        slice_max(energy) %>% slice(1)
      center = sample_center$i
      left = sample_center$i
      right = sample_center$i
      while (sign(waveform$diff[left]) == sign(waveform$diff[center]) & left > 1) {
        left = left - 1
      }
      while (sign(waveform$diff[right] == sign(waveform$diff[center])) & right < 49) {
        right = right + 1
      }
      signal_features = c(waveform$signal[left], waveform$signal[right], left-right)
      w = length(signal_features)
      features[t_i, ((c-1)*w+1):(c*w)] = signal_features
    }
  }
  return(features)
}





















# Input a dataframe with columns x, y
# Return the dataframe with added columns `col`, `row`, `digit`
xy2digit <- function(df) {
  df %>%
    mutate(col = 1*(x<380) + 2*(380<x&x<700) + 3*(700<x),
           row = 1*(y<1550) + 2*(1550<y&y<1680) + 3*(1680<y&y<1880) + 4*(1880<y)) %>%
    mutate(digit = ifelse(row==4, 0, col+3*row-3)) %>%
    mutate(digit = factor(digit,
                          levels=c("1","4","7","2","5","8","0","3","6","9")))
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
      mutate(theta = atan2(u,-v)) %>%
      select(-u, -v) %>%
      xy2digit()
      
    
    
    # A plot, to view the relations between each of the possible responses
    
    
    features <- features %>% bind_cols(labels)
  }
  return(features)
}



# from filepath or tidbits, predict touch and build a dataframe of touch_predicts 
build_experiment <- function(tidbits=NULL, path="data/trial.csv") {
  if (is.null(tidbits)) {
    tidbits <- read_csv(path, show_col_types=FALSE)
  }
  
  data <- clean_tidbits(tidbits)
  
  ## Portion data into train and test
  prop = 0.6
  data <- data %>%
    mutate(set = ifelse(time < quantile(time, prop),
                        "training", "testing"))
  
  # Predict touch ~ pressure (training dataset)
  touch_confidence <- train_pressure_model(data, width=5, incidences=4)
  
  # Select a threshold (using training dataset)
  threshold_trials <- evaluate_thresholds(data, touch_confidence, sets="training")
  threshold <- mean(c(threshold_trials$threshold_F1, threshold_trials$threshold_Fhalf))
  thresholded <- threshold_pressure(touch_confidence, threshold)
  
  # put the touch_predicts aside and into the big data tibble
  touch_predicts <- thresholded %>% filter(type == "touch_predict") %>% select(-p)
  data <- bind_rows(data, touch_predicts)
  
  # mine features
  exp_data_labeled <- build_df_touch(data, touch_confidence, labeled=TRUE)
  exp_data_unlabeled <- build_df_touch(data, touch_confidence, labeled=FALSE)
  
  # join touch_predicts with touches
  times_touch <- exp_data_labeled$time
  times_predict <- exp_data_unlabeled$time
  event_id_touch = integer(length(times_touch))
  event_id_predict = integer(length(times_predict))
  radius = 3 * quantile(diff(touch_confidence$time), 0.5)
  j = 1
  k = 1
  event_id = 1
  while (j < length(times_touch) & k < length(times_predict)) {
    t_touch = times_touch[j]
    t_predict = times_predict[k]
    if (abs(t_touch-t_predict) < radius) {
      event_id_touch[j] = event_id
      event_id_predict[k] = event_id
      k = k + 1
    } else {
      if (t_touch > t_predict) {
        k = k + 1
      } else {
        j = j + 1
        event_id = event_id + 1
      }
    }
  }
  event_id_touch[event_id_touch == 0] = NA
  event_id_predict[event_id_predict == 0] = NA
  exp_data_labeled$event_id = event_id_touch
  exp_data_unlabeled$event_id = event_id_predict
  
  events <- full_join(exp_data_labeled, exp_data_unlabeled,
                      by="event_id",
                      na_matches="never",
                      suffix=c("_touch", "_predict")) %>%
    mutate(isTouch = !is.na(time_touch),
           isPredict = !is.na(time_predict))
  
  result = list(data=data,
                touch_confidence=touch_confidence,
                threshold=threshold,
                threshold_trials=threshold_trials,
                events=events)
  
  return(result)
}