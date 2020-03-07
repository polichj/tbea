library(dplyr)


# importing data
points_tbea <- read.csv("C:/Users/jessyca/Downloads/TBEA_2019_Points - Points.csv", header=FALSE)
points_tbea <- points_tbea[-c(1),]

points_tbea <- cbind(points_tbea,Points = c(""))

#renaming colums
names_ <- c("Email","Name","Age","Horse_Name","TB_half","Member_ID","Show","Class","Level","Placing","Dressage_Score","Points")

colnames(points_tbea) <- names_
