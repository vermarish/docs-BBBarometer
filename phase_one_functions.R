# For labeling snapshots of pressure data with 1 or 0
# input: a dataframe containing pressure data and touch data
# output: a dataframe where each row is the result of point-wise multiplication 
#         between factor 1 and factor 2, and is labeled.
#         factor 1:  iterated difference of pressure signal
#                    (a naive first derivative)
#         factor 2:  an impulse of specified width
#                    e.g. width=3 -> factor_2=[0 ... 0 1 1 1 0 ... 0]
#                    (the impulse is iterated for each row)
#       
#
# and a 5th column for the label
build_df <- function(pressure_data, width) {
  df <- data.frame(matrix(rep(NA,width + 1), nrow=1))
  df <- na.omit(df)
  colnames(df)[ncol(df)] = "label"
  
  i = 1
  quota = 0
  while (i <= nrow(pressure_data) - width) {
    event <- pressure_data %>%
      slice(i:(i+width+3))
    
    pressure_reading <- event %>%
      filter(type == "pressure") %>%
      head(n=width+1) %>%
      .$one
    
    
    if (event$type[1] == "touch") {
      # then don't process it.
      # 
      # instead, start labeling subsequent pressure_events as 1
      # quota is a "hyperparameter" for labeling data:
      #     the number of windows recorded following a touch event
      quota = 2
    } else {  # event$type[1] == "pressure"
      # then label as 1 only if we need to meet the quota
      if (quota > 0) {
        label = 1
        quota = quota - 1
      } else {
        label = 0
      }
      df[nrow(df)+1,] = c(diff(pressure_reading), label)
    }
    i = i + 1
  }
  return(df)
}






# input: tibbles with the tidbits, and a width parameter
# this function will restructure the tidbits for regression,
#                    train the model,
#                    and return the fitting for train and test.
# the width describes the size of the window passed across the signal
# the windowed signal is used in logistic regression
train_pressure_model <- function(train, test, width=4) {
  buffer = width
  
  ## Arrange the data
  train_df <- build_df(train, width=width)
  test_df <- build_df(test, width=width)
  
  ## Fit the model
  touch_model <- glm(label ~ . + 0, 
                     data=train_df,
                     family = "binomial")
  
  
  ## TODO let's try fitting a model with lasso regularization
  touch_model_lasso <- glmnet(x=as.matrix(train_df %>% select(-label)),
                              y=train_df$label,
                              family="binomial")
  
  
  ## Handle the values fitted in training
  # Store the fitted values
  result_training <- bind_cols(train %>% 
                                 filter(type == "pressure") %>% 
                                 slice((buffer):(buffer+nrow(train_df)-1)),
                               touch_model$fitted.values) %>%
    rename(p = ...6)
  
  # Shift the fitted values
  shift <- 3
  result_training_shifted <- result_training
  result_training_shifted$p <- c(
    result_training_shifted$p[(shift+1):nrow(result_training_shifted)],
    rep(0, shift)
  )
  
  ## Handle the values predicted in testing
  # Compute the predicted values
  output <- predict(touch_model, 
                    newdata=test_df)
  fitted.values <- 1/(1+exp(-1*(output)))
  
  # Store the predicted values
  result_testing <- bind_cols(test %>% 
                                filter(type == "pressure") %>% 
                                slice((buffer):(buffer+nrow(test_df)-1)),
                              fitted.values) %>%
    rename(p = ...6)
  
  # Shift the fitted
  result_testing_shifted <- result_testing
  result_testing_shifted$p <- c(
    result_testing_shifted$p[(shift+1):nrow(result_testing_shifted)],
    rep(0, shift)
  )
  
  return(bind_rows(result_training_shifted %>% mutate(set="training"),
                   result_testing_shifted %>% mutate(set="testing")))
}



# Input a vector containing integers in order
# Collapse chains of many consecutive integers into a single integer
# e.g. 2 4 5 6 8 9 11 12
#  ->  2 4-5-6 8-9 11-12
#  ->  2 4 8 11
eliminate_consecutive_integers <- function(integers) {
  # strategy: shift the entries of the lift by one and compare it against itself
  first <- integers[1:length(integers)-1]
  last <- integers[2:length(integers)]
  last_decrement <- last - 1
  
  
  # the first entry is always valid, but this strategy requires it be indexed separately.
  result <- c(integers[1], 
              last[first != last_decrement])
  return(result)
}





# Input a dataframe with pressure data and the modeled p-confidence of touch occurrence.
# Include the value at which to threshold.
threshold_pressure <- function(result, threshold=0.4) {
  indices <- which(result$p > threshold) %>%
    eliminate_consecutive_integers
  
  occurrence_times <- result %>%
    slice(indices) %>%
    .$time
  
  occurrence_df <- data.frame(type="touch_predict",
                              time=occurrence_times,
                              one=0,
                              two=0,
                              three=0,
                              p=0)
  
  thresholded <- bind_rows(result, occurrence_df) %>%
    arrange(time)
  
  return(thresholded)
}



# data: should contain touch tidbits and sensor tidbits
# touch_confidence: literally just needs columns p and time
scatterplot_pressure_model <- function(data, touch_confidence, a, b, threshold=0.5,
                                       sensor_type="pressure", col="one") {
  touch_range <- data %>% filter(type=="touch") %>% select(time) %>% range()
  touch_span <- touch_range %>% diff()
  first_touch <- touch_range[1]
  open = touch_span*a + first_touch
  close = touch_span*b + first_touch
  
  # TODO remove this plot
  # also, try coloring: testing red, training #d781d5
  scatterplot(touch_confidence, sensor_type, col, a, b) +
    geom_line(data=touch_confidence %>% 
                filter(time > open & time < close),
              aes(y=p*diff(range(one)) + min(one),
                  colour=set))
  
  foo <- scatterplot(data, sensor_type, col, a, b) + ggtitle("")
  bar <- touch_confidence %>%
    filter(time > open & time < close) %>%
    ggplot() +
    geom_line(aes(x=time, y=p, color=set)) +
    scale_x_continuous(limits=c(open, close)) +
    scale_y_continuous(limits=c(0,1)) +
    geom_hline(yintercept=threshold, linetype=2, color="gray")
  
  
  ggarrange(foo, bar,
            ncol=1, nrow=2,
            labels=c("sensor and touch input","model confidence"),
            heights=c(1.5,1))
}