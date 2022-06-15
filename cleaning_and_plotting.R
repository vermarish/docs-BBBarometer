type_map <- function(type) {
  if (type == -27) {
    return("touch")
  } else if (type == 1) {
    return("accelerometer")
  } else if (type == 4) {
    return("gyroscope")
  } else if (type == 6) {
    return("pressure")
  } else {
    return("unknown")
  }
}

clean_tidbits <- function(tidbits) {
  ## Get the clocking tidbits
  clocking <- tidbits %>%
    filter(type == -1 | type == -2)
  
  uptimeMillis <- clocking %>%
    filter(type == -1) %>%
    select(time)
  
  elapsedRealtimeNanos <- clocking %>%
    filter(type == -2) %>%
    select(time)
  
  uptimeMillisStart = min(uptimeMillis)
  uptimeMillisEnd = max(uptimeMillis)
  elapsedRealtimeNanosStart = min(elapsedRealtimeNanos)
  elapsedRealtimeNanosEnd = max(elapsedRealtimeNanos)
  
  ## Select the data tidbits
  tidbits <- tidbits %>% 
    filter(type == -27 | type > 0)
  
  calibrated <- tidbits %>%
    filter(type != -27)
  
  # Calibrate the touch-data tidbits
  m = (elapsedRealtimeNanosEnd - elapsedRealtimeNanosStart) / (uptimeMillisEnd - uptimeMillisStart)
  b = elapsedRealtimeNanosEnd - m*uptimeMillisEnd
  
  uncalibrated <- tidbits %>%
    filter(type == -27) %>%
    mutate(time = m*time + b)
  
  ## Combine results and re-format type as string
  tidbits <- bind_rows(calibrated, uncalibrated) %>%
    mutate(type = sapply(type, type_map)) %>%
    arrange(time)
  
  return(tidbits)
}


data <- clean_tidbits(tidbits)


first_touch = data %>%
  filter(type=="touch") %>%
  select(time) %>%
  min %>%
  as.double

last_touch = data %>% 
  filter(type=="touch") %>%
  select(time) %>%
  max %>%
  as.double

range = last_touch - first_touch

# col = {'one', 'two', 'three'}
scatterplot <- function(data, sensor_type, col, 
                        start=0, end=1,
                        size=1,
                        stem=FALSE, derivs=0) {
  time_series <- data %>%
    filter(type==sensor_type) %>%
    select(time, col)
  
  # apply as many derivs as are specified
  i = 0
  while (i < derivs) {
    values = time_series[which(names(time_series) == col)][[1]]
    values_d = c(0, diff(values))
    time_series[which(names(time_series) == col)][[1]] = values_d
    i = i + 1
  }
  
  
  touch_range <- data %>% filter(type=="touch") %>% select(time) %>% range()
  touch_span <- touch_range %>% diff()
  first_touch <- touch_range[1]
  open = touch_span*start + first_touch
  close = touch_span*end + first_touch
  
  graph <- time_series %>%
    filter(time > open & time < close) %>%
    ggplot(aes_string(x="time", y=col)) + 
    geom_point(colour = "#a1a3a1") + 
    scale_y_continuous() +
    scale_x_continuous(limits=c(open, close))
  
  if (!stem) {
    graph <- graph + 
      geom_line(colour = "#a1a3a1")  # plot signal using line
  } else {
    graph <- graph + 
      geom_segment(aes(xend=time, yend=0), color = "#a1a3a1")
  }
  
  # if the time series has both positive and negative values
  if (sum(sign(range(time_series[which(names(time_series) == col)][[1]]))) == 0) {
    graph <- graph + geom_hline(yintercept=0, linetype=2, color="#666666")
  }
  
  # add title and subtitle
  duration <- (range*(end-start) / 1e9 ) %>% round(2) %>% toString()
  title <- paste(sensor_type, col)
  if (derivs > 0) { title <- paste(title, "--", derivs, "derivatives") }
  graph <- graph + 
    ylab(title) + 
    ggtitle(label=title,
            subtitle=paste("Data captured across", duration, "seconds."))
  
  
  
  # Plot type "touch"
  
  touch_data <- data %>%
    filter(type == "touch" | type == "touch_predict") %>%
    filter(time > open & time < close)
  graph <- graph + 
    geom_vline(aes(xintercept=time, linetype=type, color=type, size=type), data=touch_data) +
    scale_size_discrete(range=c(1,1.5)) +
    labs(color="Events", linetype="Events", size="Events")
  
  cols <- c("touch" = "#F8766D", "touch_predict" = "#619CFF", "training" = "#00BA38", "testing" = "purple")
  
  graph <- graph + scale_color_manual(values=cols)
  
  return(graph)
}
