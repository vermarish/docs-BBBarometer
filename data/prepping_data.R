# this script is for prepping data for large-scale analysis


# 3. Build train_df and test_df separately with build_df(data, width=5, incidences=4)  (cue Jason Bramburger saying you just gotta set some numbers and run with it)
# 4. Build touches from data %>% filter(type=="touch")
# 5. Store data, train_df, test_df, touches as incrementing .rda files for later [^2]
# 6. Combine each thing into one large thing and save that as four big .rda's
# 
# [^1] the threshold is part of the overall model, and its selection requires evaluation on an independent set of labeled data to select
# [^2] e.g. data_1, data_2, ..., data_n; train_df_1, train_df_2, ..., train_df_n; ...

setwd("C:/Users/risha/OneDrive/Desktop/docs-BBBarometer/data")
source("../cleaning_and_plotting.R")
source("../phase_one_functions.R")
source("../phase_two_functions.R")
library(tidyverse)



file_names = list.files("phase_1_dirty")
input_filepaths = paste("phase_1_dirty", file_names, sep="/")
file_names = file_names %>% substr(start=0, stop=nchar(file_names)-4) # remove the .csv suffix

# run to see which files have overlapping time stamps
# temp_time_check = matrix(nrow=length(file_names), ncol=4)
# for (i in seq(length(file_names))) {
#   data <- clean_tidbits(read_csv(input_filepaths[i]))
#   temp_time_check[i,1:2] = range(data$time)
#   temp_time_check[i,3] = data %>% filter(type=="pressure") %>% pull(time) %>% diff %>% max
#   temp_time_check[i,4] = i
# }
# foobar <- as.tibble(temp_time_check) %>% 
#   arrange(V1) %>% mutate(then=lead(V2,1)) %>% mutate(overlap = V2 >= then)
# input_filepaths[foobar %>% filter(overlap) %>% pull(V4)]



for (i in seq(length(file_names))) {
  print(paste(i,length(file_names)),sep="/")
  
  # 1. Clean the tidbits into data
  data <- clean_tidbits(read_csv(input_filepaths[i]))
    
  # 2. Label as 60% as training, 40% as testing
  # cutoff <- data %>% filter(type=="touch") %>% pull(time) %>% quantile(0.6)
  # data <- data %>%
  #   mutate(set = ifelse(time < cutoff,
  #                       "training", "testing"))
  # # We used to do this by partitioning within each individual dataset.
  # # Instead, let's draw our partition between each dataset.
  if (i %% 3 == 0) {  
    data$set = "testing"
  } else {
    data$set = "training"
  }

  # 3. Build train_df and test_df separately with build_df(data, width=5, incidences=4)  (cue Jason Bramburger saying you just gotta set some numbers and run with it)
  train <- data %>% 
    filter(set=="training") %>%
    filter(type %in% c("pressure", "touch")) %>%
    arrange(time)
  test <- data %>% 
    filter(set=="testing") %>%
    filter(type %in% c("pressure", "touch")) %>%
    arrange(time)
  
  if (nrow(train) > 0)  train_df <- build_df(train, width=5, incidences=4)
  if (nrow(test) > 0)   test_df <- build_df(test, width=5, incidences=4)
    
  # 4. Build touches from data %>% filter(type=="touch")
  touches <- data %>% filter(type=="touch")
  
  # 5. Store data, train_df, test_df, touches as incrementing .rda files for later [^2]
  save(data, file=paste("temp/data", file_names[i], ".Rda"))
  if (nrow(train) > 0)  save(train_df, file=paste("temp/train_df", file_names[i], ".Rda"))
  if (nrow(test) > 0)   save(test_df, file=paste("temp/test_df", file_names[i], ".Rda"))
  save(touches, file=paste("temp/touches", file_names[i], ".Rda"))
}



# 6. Combine each thing into one large thing and save that as four big .rda's
prefixes = c("data", "train_df", "test_df", "touches")
load_object <- function(file) {
  tmp <- new.env()
  load(file = file, envir = tmp)
  tmp[[ls(tmp)[1]]]
}


for (prefix in prefixes) {
  # get every file named ./temp/[prefix]*
  temp_files = list.files("temp")
  temp_files = temp_files[grepl(pattern=paste("^",prefix,sep=""),
                                x=temp_files)]
  temp_files = paste("temp", temp_files, sep="/")
  # read each of those files and combine into a single df
  head <- load_object(temp_files[1])
  for (i in seq(2:length(temp_files))) {
    tail <- load_object(temp_files[i])
    head <- rbind(head, tail)
  }
  
  varName = paste("phase", "1", prefix, sep="_")
  assign(varName, head)
}

save(list=c("phase_1_data","phase_1_train_df","phase_1_test_df","phase_1_touches"), 
     file="phase_1_clean/phase_1_data.Rda")
