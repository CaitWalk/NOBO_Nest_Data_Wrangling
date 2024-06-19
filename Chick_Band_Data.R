library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(tidyverse)

#Excel files:
# Brood_Info_2000_2017
# ChickcaptureData_No_dates

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


 


