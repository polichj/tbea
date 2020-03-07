## Points Assignments
# 1st - 6pts
# 2nd - 5pts
# 3rd - 4pts
# 4th - 3pts
# 5th - 2pts
# 6th - 1pts
# Participant - 1pts
# Champion - 10pts
# Reserve Champion - 8pts
# TBEA Shows & Recognized Dressage or Horse Trials + 2pts each category

library(dplyr)


# importing data
points_tbea <- read.csv("C:/Users/jessyca/Downloads/TBEA_2019_Points - Points.csv", header=FALSE)
points_tbea <- points_tbea[-c(1),]

points_tbea <- cbind(points_tbea,Points = c(""))

#renaming colums
names_ <- c("Email","Name","Age","Horse_Name","TB_half","Member_ID","Show","Class","Level","Placing","Dressage_Score","Points")

colnames(points_tbea) <- names_

# adding points_ column
# points for TBEA or Recognized Shows
points_tbea$Points <- ifelse(points_tbea$Placing == 1 & points_tbea$Show %in% (c("Recognized","Spring Show", "Fall Show")),8,points_tbea$Points)
points_tbea$Points <- ifelse(points_tbea$Placing == 2 & points_tbea$Show %in% (c("Recognized","Spring Show", "Fall Show")),7,points_tbea$Points)
points_tbea$Points <- ifelse(points_tbea$Placing == 3 & points_tbea$Show %in% (c("Recognized","Spring Show", "Fall Show")),6,points_tbea$Points)
points_tbea$Points <- ifelse(points_tbea$Placing == 4 & points_tbea$Show %in% (c("Recognized","Spring Show", "Fall Show")),5,points_tbea$Points)
points_tbea$Points <- ifelse(points_tbea$Placing == 5 & points_tbea$Show %in% (c("Recognized","Spring Show", "Fall Show")),4,points_tbea$Points)
points_tbea$Points <- ifelse(points_tbea$Placing == 6 & points_tbea$Show %in% (c("Recognized","Spring Show", "Fall Show")),2,points_tbea$Points)
points_tbea$Points <- ifelse(points_tbea$Placing == "Participant" & points_tbea$Show %in% (c("Recognized","Spring Show", "Fall Show")),2,points_tbea$Points)

# points for shows other than tbea or recognized
points_tbea$Show<- ifelse(!points_tbea$Show %in% (c("Recognized","Spring Show", "Fall Show")),"Other",points_tbea$Show)

points_tbea$Points <- ifelse(points_tbea$Placing == 1 & points_tbea$Show == "Other",6,points_tbea$Points)
points_tbea$Points <- ifelse(points_tbea$Placing == 2 & points_tbea$Show == "Other",5,points_tbea$Points)
points_tbea$Points <- ifelse(points_tbea$Placing == 3 & points_tbea$Show == "Other",4,points_tbea$Points)
points_tbea$Points <- ifelse(points_tbea$Placing == 4 & points_tbea$Show == "Other",3,points_tbea$Points)
points_tbea$Points <- ifelse(points_tbea$Placing == 5 & points_tbea$Show == "Other",2,points_tbea$Points)
points_tbea$Points <- ifelse(points_tbea$Placing == 6 & points_tbea$Show == "Other",1,points_tbea$Points)
points_tbea$Points <- ifelse(points_tbea$Placing == "Participant" & points_tbea$Show == "Other",1,points_tbea$Points)

# Points for Champion and Reserve
points_tbea$Points <- ifelse(points_tbea$Placing %in% c("Champion", "Champion (Tie") & points_tbea$Show == "Other",10,points_tbea$Points)
points_tbea$Points <- ifelse(points_tbea$Placing == "Reserve Champion" & points_tbea$Show == "Other",8,points_tbea$Points)
points_tbea$Points <- ifelse(points_tbea$Placing %in% c("Champion", "Champion (Tie") & points_tbea$Show %in% (c("Recognized","Spring Show", "Fall Show")),12,points_tbea$Points)
points_tbea$Points <- ifelse(points_tbea$Placing == "Reserve Champion" & points_tbea$Show %in% (c("Recognized","Spring Show", "Fall Show")),10,points_tbea$Points)

# highpoint calculations
english_performance_tb <- points_tbea %>%
  filter(Class == "English Performance",
         TB_half == "TB")
english_performance_tb <- aggregate(english_performance_tb$Points,by=list(category=english_performance_tb$Name), FUN=sum) %>%
  arrange(desc(x))

english_performance_half <- points_tbea %>%
  filter(Class == "English Performance",
         TB_half == "1/2")
english_performance_half <- aggregate(english_performance_half$Points,by=list(category=english_performance_half$Name), FUN=sum) %>%
  arrange(desc(x))

western_performance_tb <- points_tbea %>%
  filter(Class == "Western Performance",
         TB_half == "TB")
western_performance_tb <- aggregate(western_performance_tb$Points, by=list(category=western_performance_tb$Name), FUN=sum)%>%
  arrange(desc(x))

western_performance_half <- points_tbea %>%
  filter(Class == "Western Performance",
         TB_half == "1/2")
western_performance_half <- aggregate(western_performance_half$Points, by=list(category=western_performance_half$Name), FUN=sum)%>%
  arrange(desc(x))

open_performance_TB <- points_tbea %>%
  filter(TB_half == "TB")
open_performance_TB <- aggregate(open_performance_TB$Points, by=list(category=open_performance_TB$Name), FUN=sum)%>%
  arrange(desc(x))

open_performance_half <- points_tbea %>%
  filter(TB_half == "1/2")
open_performance_half <- aggregate(open_performance_half$Points, by=list(category=open_performance_half$Name), FUN=sum)%>%
  arrange(desc(x))


h_j_tb <- points_tbea %>%
  filter(Class == "H/J",
         TB_half == "TB")
h_j_tb <- aggregate(h_j_tb$Points, by=list(category=h_j_tb$Name), FUN=sum) %>%
  arrange(desc(x))

h_j_half <- points_tbea %>%
  filter(Class == "H/J",
         TB_half == "1/2")
h_j_half <- aggregate(h_j_half$Points, by=list(category=h_j_half$Name), FUN=sum) %>%
  arrange(desc(x))

combined_test_tb <- points_tbea %>%
  filter(Class == "Combined Test",
         TB_half == "TB")
combined_test_tb <- aggregate(combined_test_tb$Points, by=list(category=combined_test_tb$Name), FUN=sum) %>%
  arrange(desc(x))

combined_test_half <- points_tbea %>%
  filter(Class == "Combined Test",
         TB_half == "1/2")
combined_test_half <- aggregate(combined_test_half$Points, by=list(category=combined_test_half$Name), FUN=sum) %>%
  arrange(desc(x))

youth_13_under <- points_tbea %>%
  filter(Age == "13 & under")
youth_13_under <- aggregate(youth_13_under$Points, by=list(category=youth_13_under$Name), FUN=sum) %>%
  arrange(desc(x))

youth_14_18<- points_tbea %>%
  filter(Age == "14-18" |
           Age == "14-17")
youth_14_18 <- aggregate(youth_14_18$Points, by=list(category=youth_14_18$Name), FUN=sum) %>%
  arrange(desc(x))

adult_19_39 <- points_tbea %>%
  filter(Age == "19-39")
adult_19_39 <- aggregate(adult_19_39$Points, by=list(category=adult_19_39$Name), FUN=sum) %>%
  arrange(desc(x))

adult_39_up <- points_tbea %>%
  filter(Age == "39+")
adult_39_up <- aggregate(adult_39_up$Points, by=list(category=adult_39_up$Name), FUN=sum)%>%
  arrange(desc(x))

# highest_dressage_adult <- points_tbea %>%
#   filter(Class == "Dressage",
#          Age == "19-39" | 
#           Age =="39+") %>%
#   arrange(desc(Dressage_Score)) %>%
#   select(Name,
#          Dressage_Score)
# 
# highest_dressage_youth <- points_tbea %>%
#   filter(Class == "Dressage",
#          Age == "14-18" | 
#            Age =="13 & under") %>%
#   arrange(desc(Dressage_Score)) %>%
#   select(Name,
#          Dressage_Score)

highest_dressage_score_tb <- points_tbea %>%
  filter(Class == "Dressage",
         TB_half == "TB") %>%
  arrange(desc(Dressage_Score)) %>%
  select(Name,
         Dressage_Score)

highest_dressage_score_half <- points_tbea %>%
  filter(Class == "Dressage",
         TB_half == "1/2") %>%
  arrange(desc(Dressage_Score)) %>%
  select(Name,
         Dressage_Score)

classical_dressage_tb <- points_tbea %>%
  filter(Class == "Dressage",
         TB_half == "TB")
classical_dressage_tb <- aggregate(classical_dressage_tb$Points, by=list(category=classical_dressage_tb$Name), FUN=sum)%>%
  arrange(desc(x))

classical_dressage_half <- points_tbea %>%
  filter(Class == "Dressage",
         TB_half == "1/2")
classical_dressage_half <- aggregate(classical_dressage_half$Points, by=list(category=classical_dressage_half$Name), FUN=sum)%>%
  arrange(desc(x))

#need to add tb and 1/2
dressage_western <- points_tbea %>%
  filter(Class == "Western Dressage")
dressage_western <- aggregate(dressage_western$Points, by=list(category=dressage_western$Name), FUN=sum)%>%
  arrange(desc(x))

##NEED TO ADD TB AND HALF
gymkhana <- points_tbea %>%
  filter(Class == "Gymkhana")
gymkhana <- aggregate(gymkhana$Points, by=list(category=gymkhana$Name), FUN=sum)%>%
  arrange(desc(x))

halter_tb_geldings  <- points_tbea %>%
  filter(Class == "Halter - Geldings"|
                    Class == "Halter - Gelding",
                    TB_half == "TB")
halter_tb_geldings <- aggregate(halter_tb_geldings$Points, by=list(category=halter_tb_geldings$Name), FUN=sum)%>%
  arrange(desc(x))


halter_tb_mares <- points_tbea %>%
  filter(Class == "Halter - Mares",
         TB_half == "TB")
halter_tb_mares <- aggregate(halter_tb_mares$Points, by=list(category=halter_tb_mares$Name), FUN=sum)%>%
  arrange(desc(x))


halter_half_geldings  <- points_tbea %>%
  filter(Class == "Halter - Geldings"|
           Class == "Halter - Gelding",
         TB_half == "1/2")
halter_half_geldings <- aggregate(halter_half_geldings$Points, by=list(category=halter_half_geldings$Name), FUN=sum)%>%
  arrange(desc(x))


halter_half_mares <- points_tbea %>%
  filter(Class == "Halter - Mares",
         TB_half == "1/2")
halter_half_mares <- aggregate(halter_half_mares$Points, by=list(category=halter_half_mares$Name), FUN=sum)%>%
  arrange(desc(x))

