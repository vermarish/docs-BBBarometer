# For labeling snapshots of pressure data with 1 or 0
# input: a dataframe containing pressure data and touch data
#        width: the size of a single window
#        incidences: the number of windows to record following a touch event
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
build_df <- function(pressure_data, width=4, incidences=2) {
  df <- data.frame(matrix(rep(NA,width + 2), nrow=1))
  df <- na.omit(df)
  colnames(df)[ncol(df)-1] = "pressure"
  colnames(df)[ncol(df)] = "label"
  
  i = 1
  quota = 0
  while (i <= nrow(pressure_data) - 2*width) {
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
      quota = incidences
    } else {  # event$type[1] == "pressure"
      # then label as 1 only if we need to meet the quota
      if (quota > 0) {
        label = 1
        quota = quota - 1
      } else {
        label = 0
      }
      # <windowed first derivative of pressure> | <max windowed pressure> | label
      df[nrow(df)+1,] = c(diff(pressure_reading), max(pressure_reading), label)
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
train_pressure_model <- function(data, width=4, incidences=2) {
  buffer = width
  
  train <- data %>% 
    filter(set=="training") %>%
    filter(type %in% c("pressure", "touch")) %>%
    arrange(time)
  
  test <- data %>% 
    filter(set=="testing") %>%
    filter(type %in% c("pressure", "touch")) %>%
    arrange(time)
  
  ## Arrange the data
  train_df <- build_df(train, width=width, incidences=incidences)
  test_df <- build_df(test, width=width, incidences=incidences)
  
  ## Fit the model
  #model <- glm(label ~ ., 
                #data=train_df,
                #family = "binomial")
  link <- function(x) {1/(1+exp(-1*(x)))}
  # Naive model
  train_df_1 <- train_df
  test_df_1 <- test_df
  model1 <- glm(label ~ ., 
                     data=train_df_1,
                     family = "binomial")
  RSS1 <- sum((link(predict(model1, test_df_1)) - test_df_1$label)^2)
  
  # Center pressure, zero-intercept
  train_df_2 <- train_df %>% mutate(pressure = pressure - median(pressure))
  test_df_2 <- test_df %>% mutate(pressure = pressure - median(pressure))
  model2 <- glm(label ~ . + 0,
                data=train_df_2,
                family="binomial")
  RSS2 <- sum((link(predict(model2, test_df_2)) - test_df_2$label)^2)
  
  # Naive, regress on X1 and X2,X3
  train_df_3 <- train_df_1
  test_df_3 <- test_df_1
  model3 <- glm(label ~ X1 + X2 + X3 + pressure, 
                data=train_df_1,
                family = "binomial")
  RSS3 <- sum((link(predict(model3, test_df_3)) - test_df_3$label)^2)
  
  # Naive, regress on X1 and X3+X4
  train_df_4 <- train_df %>% mutate(a=X1, b=X3+X4) %>% select(a,b,pressure,label)
  test_df_4 <- test_df %>% mutate(a=X1, b=X3+X4) %>% select(a,b,pressure,label)
  model4 <- glm(label ~ ., 
                data=train_df_4,
                family = "binomial")
  RSS4 <- sum((link(predict(model4, test_df_4)) - test_df_4$label)^2)
  
  # Stupid
  train_df_5 <- train_df %>% select(-pressure)
  test_df_5 <- test_df %>% select(-pressure)
  model5 <- glm(label ~ ., 
                data=train_df_5,
                family = "binomial")
  RSS5 <- sum((link(predict(model5, test_df_5)) - test_df_5$label)^2)
  
  # log(pressure)
  train_df_6 <- train_df %>% mutate(pressure = log(pressure-min(pressure)+1))
  test_df_6 <- test_df %>% mutate(pressure = log(pressure-min(pressure)+1))
  model6 <- glm(label ~ ., 
                data=train_df_6,
                family = "binomial")
  RSS6 <- sum((link(predict(model6, test_df_6)) - test_df_6$label)^2)
  
  
  train_df <- train_df_6
  test_df <- test_df_6
  touch_model <- model6
  
  

  ## TODO let's try fitting a model with lasso regularization
  touch_model_lasso <- glmnet(x=as.matrix(train_df %>% select(-label)),
                              y=train_df$label,
                              family="binomial")
  
  
  ## Handle the values fitted in training
  # Store the fitted values
  result_training <- bind_cols(train %>% 
                                 filter(type == "pressure") %>% 
                                 slice((buffer):(buffer+nrow(train_df)-1)),
                               p=touch_model$fitted.values)
  
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
                              p=fitted.values)
  
  # Shift the fitted
  result_testing_shifted <- result_testing
  result_testing_shifted$p <- c(
    result_testing_shifted$p[(shift+1):nrow(result_testing_shifted)],
    rep(0, shift)
  )
  
  return(bind_rows(result_training_shifted %>% mutate(set="training"),
                   result_testing_shifted %>% mutate(set="testing")))
}


# Helper function for threshold_pressure()
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
# Inserts rows of type "touch_predict" labeled by set "training" or "testing"
# Assumes all training data happens before testing data.
threshold_pressure <- function(touch_confidence, threshold=0.4) {
  cutoff <- touch_confidence %>%
    filter(set == "training") %>%
    pull(time) %>%
    max()
  
  indices <- which(touch_confidence$p > threshold) %>%
    eliminate_consecutive_integers
  
  occurrence_times <- touch_confidence %>%
    slice(indices) %>%
    .$time
  
  occurrence_df <- tibble(type="touch_predict",
                          time=occurrence_times,
                          one=0,
                          two=0,
                          three=0,
                          p=0,
                          set="") %>%
    mutate(set = sapply(time, function(t) {if (t > cutoff) "testing" else "training"})) %>%
    mutate(set = as.character(set))
  
  thresholded <- bind_rows(touch_confidence, occurrence_df) %>%
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



# data includes the rows:
#   touch
# touch_confidence includes the columns:
#   p: a 0-1 confidence of touch input
#   set: {"training", "testing"}
# 
# returns a dataframe containing:
#   * threshold values (0-1)
#   * PPV and TPR proportions
#   * TP, FP, and FN counts
#   * F0.5, F1, F2
evaluate_thresholds <- function(data, touch_confidence, sets = "testing") {
  dt = 4e7
  margin = 5*dt  # radius of the window for matching a touch_predict with a touch
  
  thresholds = seq(0.05, 0.98, 0.01)
  TP = integer(length(thresholds))
  FP = integer(length(thresholds))
  FN = integer(length(thresholds))
  
  touches <- data %>% filter(type=="touch")
  
  for (k in seq(length(thresholds))) {
    events <- threshold_pressure(touch_confidence, threshold=thresholds[k]) %>%
      filter(type == "touch_predict") %>%
      bind_rows(touches) %>%
      arrange(time) %>%
      # label each event falsely before iterating through and finding true positives
      mutate(class = sapply(type, function(t) {if (t=="touch") "FN" else "FP"}))
    
    # only evaluate with the testing data, or with the training data, or whatever
    events <- events %>%
      filter(set %in% sets)
    
    # two-pointer iteration through touch_data and touch_predict_data  (O(n))
    touch_data <- events %>% filter(type == "touch")
    touch_predict_data <- events %>% filter(type == "touch_predict")
    i = 1
    j = 1
    while (i <= nrow(touch_data) & j <= nrow(touch_predict_data)) {
      d = touch_data[["time"]][i] - touch_predict_data[["time"]][j]
      if (abs(d) < margin) {
        touch_data$class[i] = "TP"
        touch_predict_data$class[j] = "TP"
      }
      if (d < 0)  { i = i + 1 }  else  { j = j + 1 }
    }
    
    classifications <- bind_rows(touch_data, touch_predict_data) %>%
      arrange(time) %>%
      filter(!(type == "touch_predict" & class == "TP"))  %>% # each TP shows up twice.
      # once for touch,
      # once for touch_predict.
      select(class) %>%
      table()
    
    TP[k] <- classifications["TP"]
    FP[k] <- classifications["FP"]
    FN[k] <- classifications["FN"]
  }
  
  performance_by_threshold <- tibble(threshold=thresholds, TP, FN, FP) %>%
    replace_na(list(TP=0, FP=0, FN=0)) %>%
    mutate(PPV = TP/(TP+FP),
           TPR = TP/(TP+FN)) %>%
    replace_na(list(PPV=0, TPR=0)) %>%
    mutate(F1=2*PPV*TPR/(PPV+TPR),
           Fhalf=1.25*(PPV*TPR)/(1/4*PPV+TPR),
           F2=5*PPV*TPR/(4*PPV+TPR)) %>%
    replace_na(list(F1=0, Fhalf=0, F2=0))

  
  # Removing thresholds that are too low. See point 5 and 6.
  min_threshold <- performance_by_threshold %>% 
    filter(TP < FP) %>%  # precision < 0.5 is garbage
    slice_max(threshold) %>% 
    pull(threshold)
  if (length(min_threshold) == 0)  { min_threshold = 0 }
  
  performance_by_threshold <- performance_by_threshold %>%
    filter(threshold > min_threshold)

  # For output, find the best threshold by each F-statistic
  best_thresholds <- performance_by_threshold %>%
    pivot_longer(cols=c("F1","Fhalf","F2"), names_to="metric", values_to="value") %>%
    group_by(metric) %>%
    slice_max(value) %>%
    slice_max(threshold)
  
  value <- best_thresholds$value
  value_names <- best_thresholds$metric
  threshold <- best_thresholds$threshold
  threshold_names <- paste("threshold", value_names, sep="_")
  
  c(list(performance_by_threshold=performance_by_threshold),
    split(value, value_names), 
    split(threshold, threshold_names))
}
