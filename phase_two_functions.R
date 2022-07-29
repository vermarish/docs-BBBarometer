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
      select(x=one, y=two)
    features <- features %>% bind_cols(labels)
  }
  
  return(features)
}
