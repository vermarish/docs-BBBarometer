library(tidyverse)
library(rwt)

data <- read_csv("data/uncalibrated.csv")

gyroscope <- data %>% 
  filter(type == 4) %>% 
  select(time, one, two, three) %>% 
  arrange(desc(time))


smoothen <- function() {
  WINDOW_LENGTH = 128
  chunks = floor(gyroscope %>% count %>% as.integer / WINDOW_LENGTH)
  samples = WINDOW_LENGTH*chunks
  
  gyroscope <- gyroscope %>% slice(1:samples)
  
  smooth_data <- gyroscope %>% 
    select(one) %>% 
    unlist()
  
  
  h <- daubcqf(4)  # must be even
  
  one_smooth <- denoise.dwt(smooth_data, h$h.0)
}

smoothen

one_smooth <- smoothen()

# one_smooth <- bind_cols(gyroscope, one_smooth)

