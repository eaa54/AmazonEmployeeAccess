## This creates the dataset that will be used for analytics
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyr)


# Hand-collected float data - Will need to change sheet to all data
Float <- read_excel("/Users/eaa54/Documents/School/Thesis/FloatData.xlsx")
Float <- Float %>%
  mutate(row_id=row_number()) %>% #add row number as col to join
  select(Float, 'Server Location', 'Passer Location', Missed, row_id)

# Top teams conference only games - Will need to change this
Match <- read.csv("/Users/eaa54/.ssh/top1teams_data.csv", header = TRUE)

Match <- Match %>%
  filter(hteam == 365 | ateam == 365) %>%
  select(teamid, sk, skty, skgrd, x1, y1, x3, y3, hh.mm.ss, hrot, arot, wonlost, hms2, pind4) %>%
  mutate(speed = hms2 - hh.mm.ss, skgrd = as.integer(skgrd))

Match_Receive <- Match %>%
  filter(teamid == 365 & sk == 2 | teamid != 365 & sk == 1 & skgrd == 1) %>%
  select(pind4) %>%
  mutate(row_id = row_number())

Match_Set <- Match %>%
  filter(teamid == 365 & sk == 3) %>%
  select(x1, y1)

Match_Set

# Replace NA values in pind4 (missed serves) with something so can filter out later
Match_Receive[is.na(Match_Receive)] <- "100"

Float_Receive <- full_join(Float, Match_Receive, by = "row_id")

# Will need to figure out how to only get first ball sets in this
#Set_Location <- Match %>%
  #filter(teamid == 365 & sk == 3 | teamid != 365 & sk == 1 & skgrd == 1) %>%
  #select(teamid, sk, x1, y1, strzn)
#Set_Location

Match <- Match %>%
  filter(teamid != 365 & sk == 1) %>%
  mutate(row_id=row_number())

Data <- full_join(Float_Receive, Match, "row_id")
Data <- Data %>%
  filter(Missed == 0) %>%
  select(Float, "Server Location", "Passer Location", Missed, row_id, pind4.x, 
         teamid, sk, skty, skgrd, x3, y3, hrot, arot, wonlost, speed)
Data <- Data %>% 
  rename("sloc" = "Server Location",
         "ploc" = "Passer Location",
         "pind4" = "pind4.x") %>%
  subset(pind4 != 99 & pind4 != 100)


# ANOVA looking at Float vs. No Float with 4pt pass grade as response
Float_NoFloat <- lm(pind4 ~ -1 + Float, Data)
summary(Float_NoFloat)

# Categorize cross court vs. straight on (sharp cross, shallow cross, and straight)
Data <- Data %>%
  mutate(direction = 1) #create direction col

#simplify start zones
Data$sloc[Data$sloc == 7] <- 5
Data$sloc[Data$sloc == 9] <- 1

#true cross
Data$direction[Data$sloc == 5 & Data$ploc == 5 |
              Data$sloc == 1 & Data$ploc == 1] <- "sharp"

#straight on
Data$direction[Data$sloc == 6 & Data$ploc == 6 |
              Data$sloc == 5 & Data$ploc == 1 |
              Data$sloc == 1 & Data$ploc == 5] <- "straight"

#shallow cross
Data$direction[Data$sloc == 5 & Data$ploc == 6 |
              Data$sloc == 1 & Data$ploc == 6 |
              Data$sloc == 6 & Data$ploc == 1 |
              Data$sloc == 6 & Data$ploc == 5] <- "shallow"

#ANOVA looking at direction with pass grade as response
dirmn <- lm(pind4 ~ -1 + direction, Data)
summary(dirmn)

dira <- lm(pind4 ~ direction, Data)
anova(dira)

#Speed?
speed1 <- lm(pind4 ~ speed, Data)
summary(speed1)

plot(Data$speed, Data$pind4)

#Interactions
spdir <- lm(pind4 ~ speed*direction, Data)
spfl <- lm(pind4 ~ speed*Float, Data)
fldir <- lm(pind4 ~ Float*direction, Data)

summary(spdir)
summary(spfl)
summary(fldir)

anova(fldir)

#NEXT STEP: make seams for location variable
#create seam column
Data <- Data %>%
  mutate(seam = 1)
Data$seam[Data$x3 >=0 & Data$x3 <= 1.29] <- 4 
Data$seam[Data$x3 >1.29 & Data$x3 <= 2.58] <- "PL"
Data$seam[Data$x3 >2.58 & Data$x3 <= 3.87] <- 3
Data$seam[Data$x3 >3.87 & Data$x3 <= 5.16] <- "PM"
Data$seam[Data$x3 >5.16 & Data$x3 <= 6.45] <- 2
Data$seam[Data$x3 >6.45 & Data$x3 <= 7.74] <- "PR"
Data$seam[Data$x3 >7.74 & Data$x3 <= 9] <- 1
Data$seam[Data$y3 >= 6 & Data$y3 <= 12] <- "mid" #in front of 10ft line

#seam?
seama <- lm(pind4 ~ -1 + seam, Data)
seamdir <- lm(pind4 ~ seam*direction, Data)
seamfl <- lm(pind4 ~ Float*seam, Data)
seamsp <- lm(pind4 ~ Float*speed, Data)

summary(seama)
summary(seamdir)
summary(seamfl)
summary(seamsp)
