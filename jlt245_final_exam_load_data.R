setwd("~/final-exam")

library(lubridate)

library(readr)
library(dplyr)

filenames <- c("datatest.txt", "datatest.txt", "datatraining.txt")

data_list <- lapply(filenames, function (x) read.csv(x))

data_frame <- do.call(rbind, data_list)

data_frame

# drop date field as it is useless in analysis. we assume time independence in this analysis.
write.csv(data_frame[, !(names(data_frame) %in% c("date"))], 
          file = "jlt245_final_exam_occupancy_dataset.csv", row.names = FALSE)





