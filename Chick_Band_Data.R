library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(tidyverse)

#Excel files:
# Brood_Info_2000_2017
# ChickcaptureData_No_dates
# BroodCaptures2018-2023


ChickCaptureData <- ChickCaptureData_No_Dates

#pivot the data so that each capture even is its own row
ChickCaptureData <- ChickCaptureData %>%
  pivot_longer(
    cols = starts_with('1st_')| starts_with("2nd_"),
    names_to = c("CaptureID", '.value'),
    names_sep = '_',
    values_drop_na = TRUE
  )

#create new column 'DATE" that pulls the data information from BroodInfo based on the BroodCapID number
ChickCaptureData$Date <- Brood_Info_2000_2017$Date[match(ChickCaptureData$` Brood ID`, Brood_Info_2000_2017$BroodCaptureID)] 

ChickCaptureData$Date <- as.Date(ChickCaptureData$Date) #convert data to a data

ChickCaptureData <- ChickCaptureData %>% relocate(Date,.after = CaptureID) #move column 

#remove chin marks data
ChickCaptureData <- ChickCaptureData %>%
  filter(` Bird ID type` != "Chin Marks")
#remove chin marks data
ChickCaptureData <- ChickCaptureData %>%
  filter(` Bird ID type` != "GB")

#removed dead and released unmarked chicks from incubator
ChickCaptureData <- ChickCaptureData %>%
  filter(` Bird ID type` != "Other")
ChickCaptureData <- ChickCaptureData %>%
  filter(` BirdCondition` != "Dead")

#remove the 2018 chicks
ChickCaptureData <- ChickCaptureData %>%
  filter(` Brood ID` != "640")

#replace 14.25 with wing band for chick 102022
ChickCaptureData$` Bird ID type`[ChickCaptureData$` Bird ID type`== '14.25'] <- 'Wing Band'

#rename column
colnames(ChickCaptureData)[4] <- "Chick ID" 
colnames(ChickCaptureData)[2] <- "Capture Date"
colnames(ChickCaptureData)[9] <- "Condition"

# remove the T in front of the chickID --> extract starting as second character 
BroodCaptures2018_2023$`Chick ID` <- substring(BroodCaptures2018_2023$`Chick ID`, 2)

BroodCaptures2018_2023$`Chick ID` <- as.numeric(BroodCaptures2018_2023$`Chick ID`) #change str

BroodCaptures2018_2023 <- BroodCaptures2018_2023%>%
  filter(`Cond` != "Dead")

colnames(BroodCaptures2018_2023)[12] <- "Condition"

##########################################################################################
#merge the 2 df into a single df that contains all chicks captured
tagged_chicks <- merge(ChickCaptureData, BroodCaptures2018_2023, 
                       by = c("Capture Date", "Chick ID"), all = TRUE)

colnames(tagged_chicks)[8] <- "Weight.x"
colnames(tagged_chicks)[18] <- "Weight.y"
colnames(tagged_chicks)[11] <- "Comment.x"
colnames(tagged_chicks)[22] <- "Comment.y"

#mutate = create new column
tagged_chicks <- tagged_chicks %>%
  mutate(Weight = coalesce(Weight.x, Weight.y), #combine weight columns 
         Condition = ifelse(is.na(Condition.x), Condition.y, Condition.x), #combines Condition columns
         Comments = coalesce(Comment.x, Comment.y))

tagged_chicks <- select(tagged_chicks,-starts_with("Weight."), 
                        -starts_with("Condition."), 
                        -starts_with("Comment.")) #remove the excess columns created in the merge

#remove columns so its just capture date, chick ID, brood ID, morphomerics, and comments
tagged_chicks <- tagged_chicks[,-c(3,5,6,7,9,10,11,12,13,17,18,19,20,21)]
                                   
tagged_chicks <- tagged_chicks %>% relocate("Chick Radio", .before = "Comments")

#data not recorded on broodcapture_data BUT found date with band number from capture_history
tagged_chicks[c(4019, 4020, 4021), "Capture Date"] <- '2015-07-28'


