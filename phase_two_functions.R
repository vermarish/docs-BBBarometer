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
                     ncol=14)
  set = character(length(times_gyro))
  
  for (t_i in 1:length(times_gyro)) {
    t = times_gyro[t_i]
    
    open = t-period*25
    close = t+period*50
    windowed_signal = gyro %>% filter(time > open & time < close)
    cols = c("one", "two", "three")
    for (c in 1:3) {
      col = cols[c]
      waveform = windowed_signal %>% 
        select(time=time, signal=col, set) %>%
        mutate(energy = abs(lead(signal,2)-lag(signal,2)),
               diff = lead(signal,1) - signal)
      
      # get the sample center
      sample_center = waveform %>% 
        mutate(i = 1:nrow(waveform)) %>%
        slice_max(energy) %>% slice(1)
      
      # find the indices for the left/right edge of the sample
      center = sample_center$i
      left = sample_center$i
      right = sample_center$i
      while (sign(waveform$diff[left]) == sign(waveform$diff[center]) & left > 1) {
        left = left - 1
      }
      while (sign(waveform$diff[right]) == sign(waveform$diff[center]) & right < 74) {
        right = right + 1
      }
      
      # mine them features
      signal_features = c(waveform$signal[left], waveform$time[left], waveform$signal[right], waveform$time[right])
      w = length(signal_features)
      features[t_i, ((c-1)*w+1):(c*w)] = signal_features
    }
    set[t_i] = sample_center$set
  }
  
  
  # include timestamps
  features[,dim(features)[2]] = times
  
  # include pressure data
  pressure = touch_confidence
  d = pressure$time %>% diff %>% median()
  pressure$time = pressure$time + 2*d
  times_df = tibble(time=times)
  max_pressure_df <- difference_inner_join(times_df, touch_confidence, by="time", max_dist=3*d) %>%
    group_by(time.x) %>%
    summarise(pressure = max(one)) %>%
    select(time=time.x, pressure)
  max_pressure_df_calibrated <- left_join(times_df, max_pressure_df, by="time")
  features[,dim(features)[2] - 1] = max_pressure_df_calibrated$pressure
  
  
  
  
  # create feature names
  contents = c("left", "left_time", "right", "right_time")
  feature_names = c()
  for (i in 1:3) {
    feature_names = c(feature_names, paste(cols[i], contents, sep="_"))
  }
  feature_names = c(feature_names, "pressure", "time")

  
  # conversion to tibble for return
  feature_tibble <- as_tibble(features)
  names(feature_tibble) = feature_names
  feature_tibble$set = factor(set)
  return(feature_tibble)
}





















# Input a dataframe with columns x, y
# Return the dataframe with added columns `col`, `row`, `digit`
xy2digit <- function(df) {
  if ("x" %in% names(df)) {
    df <- df %>% mutate(col = 1*(x<380) + 2*(380<x&x<700) + 3*(700<x))
  }
  if ("y" %in% names(df)) {
    df <- df %>% mutate(row = 1*(y<1550) + 2*(1550<y&y<1680) + 3*(1680<y&y<1880) + 4*(1880<y))
  }
  if ("x" %in% names(df) & "y" %in% names(df)) {
    df <- df %>% 
      mutate(digit = ifelse(row==4, 0, col+3*row-3)) %>%
      mutate(digit = factor(digit,
                            levels=c("1","4","7","2","5","8","0","3","6","9")))
  }
  return(df)
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
           isPredict = !is.na(time_predict)) %>%
    mutate(set = coalesce(set_touch, set_predict)) %>%
    select(-set_predict, -set_touch)
    
  
  
  result = list(data=data,
                touch_confidence=touch_confidence,
                threshold=threshold,
                threshold_trials=threshold_trials,
                events=events)
  
  return(result)
}