setwd("~/final-exam")


#  lubridate
#  ggplot2
#  grid
#  gridExtra
#  scales
#  Hmisc
#  corrplot
#  randomForest
#  rattle
#  caret
#  e1071
#  RGTk2 (for rattle), it is recommended you install his package before rattle
#                      Refer to the link below for installation
#                      https://gist.github.com/sebkopf/9405675
#  rattle   refer to the link below for rattle installation in case you 
#           run into problems during installation
#           http://rattle.togaware.com/rattle-install-mswindows.html



install.packages(c("e1071", "lubridate", "ggplot2", "grid"), dep = TRUE, repos = "http://cran.us.r-project.org")
install.packages(c("scales", "Hmisc", "corrplot", "randomForest"), dep = TRUE, repos = "http://cran.us.r-project.org")
install.packages(c("caret", "rattle", "RGTk2"), dep = TRUE, repos = "http://cran.us.r-project.org")

library(MASS)
library(lubridate)
library(ggplot2)
library(grid)
library(gridExtra)
library(scales)
library(Hmisc)
library(corrplot)
library(randomForest)
library(rattle)
library(caret)
library(e1071)
library(RGTk2)
