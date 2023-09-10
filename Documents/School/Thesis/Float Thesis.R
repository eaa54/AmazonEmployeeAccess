## This creates the dataset that will be used for analytics
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyr)


# Hand-collected float data - Will need to change sheet to all data
Float <- read_excel("/Users/eaa54/Documents/FloatData.xlsx")
Float <- Float %>%
  mutate(row_id=row_number()) %>% #add row number as col to join
  select(Float, 'Server Location', 'Passer Location', Missed, row_id)

# Top teams conference only games - Will need to change this
Match <- read.csv("/Users/eaa54/.ssh/top1teams_data.csv", header = TRUE)

Match <- Match %>%
  filter(hteam == 365 | ateam == 365) %>%
  select(teamid, sk, skty, skgrd, x1, y1, hh.mm.ss, hrot, arot, wonlost, hms2, pind4) %>%
  mutate(speed = hms2 - hh.mm.ss, skgrd = as.integer(skgrd))

Match_Receive <- Match %>%
  filter(teamid == 365 & sk == 2 | teamid != 365 & sk == 1 & skgrd == 1) %>%
  select(pind4) %>%
  mutate(row_id = row_number())
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
         teamid, sk, skty, skgrd, x1, y1, hrot, arot, wonlost, speed)
Data <- Data %>% 
  rename("sloc" = "Server Location",
         "ploc" = "Passer Location",
         "pind4" = "pind4.x")


# ANOVA looking at Float vs. No Float with 4pt pass grade as response
Float_NoFloat <- aov(pind4 ~ -1 + Float, Data)
summary(Float_NoFloat)

# Prelim to see if serve location is significant
DoesZoneMatter <- aov(pind4 ~ -1 + sloc, Data)

# Categorize cross court vs. straight on (sharp cross, shallow cross, and straight)
Data <- Data %>%
  mutate(direction = 1) #create direction col

# simiplify start zones
Data$direction[Data$sloc == 7] <- 5
Data$direction[Data$sloc == 9] <- 1

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
summary(aov(pind4 ~ -1 + direction, Data))

#Speed?
summary(aov(pind4 ~ -1 + speed, Data))

#NEXT STEP: make seams for location variable
