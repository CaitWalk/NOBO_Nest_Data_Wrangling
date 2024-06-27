library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(tidyverse)
library(readxl)

##############################################################################################
##############################################################################################
#                           cHICKS Tagged DATA

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


##############################################################################################
##############################################################################################
#                           cHICK RECAPTURE DATA
#EXCEL FILES
# Capture History 2000-2017 Wing Only
# TT Data 2017-2023 (has just fall trap data)

################ 2000-2016 data
Wing_Data <- Capture_History_2000_2017_Wing_Only[,-c(1,6,8,9,13,14,16,17,21,22,25,26,30,31,34,35,38,39,41,42,45,46,48,49)]
Wing_Data <- Wing_Data[,-c(2,3,4,13,18,28,29,31,32,35,36,37,38,39,40,41,42,43)]

Wing_Data <- Wing_Data %>% #change date format
  mutate(`1st Capture Date`= as.Date(`1st Capture Date`, format="%Y/%m/%d")) %>%
  mutate(`2nd Capture Date`= as.Date(`2nd Capture Date`, format="%Y/%m/%d")) %>%
  mutate(`3rd Capture Date`= as.Date(`3rd Capture Date`, format="%Y/%m/%d")) %>%
  mutate(`4th Capture Date`= as.Date(`4th Capture Date`, format="%Y/%m/%d")) %>%
  mutate(`5th Capture Date`= as.Date(`5th Capture Date`, format="%Y/%m/%d")) %>%
  mutate(`6th Capture Date`= as.Date(`6th Capture Date`, format="%Y/%m/%d")) %>%
  mutate(`7th Capture Date`= as.Date(`7th Capture Date`, format="%Y/%m/%d")) %>%
  filter(year(`1st Capture Date`) !=2017) #remove 2017 data since incomplete frm this df

recaptures <- Wing_Data %>% drop_na(`2nd Capture Date`) #only chicks that were recaptured at least once

recaptures <- recaptures %>% 
  pivot_longer(
    cols = starts_with(c('1st ', '2nd ', '3rd ', '4th ', '5th ', '6th ', '7th ')),
    names_to = c("capture", '.value'),
    names_sep = ' ',
    values_drop_na = TRUE
  )
# Warning message:
# Expected 2 pieces. Additional pieces discarded in 10 rows [1, 4, 5, 8, 9, 12, 13, 16, 19, 22].

recaptures$Month <- lubridate::month(recaptures$Capture)
recaptures <- recaptures%>% relocate(Month, .before = Capture)

recaptures <- recaptures %>% filter(capture != "1st") #remove brood captures


################# 2017-2023 data
fall_recap <- TT_Data_2017_2023 %>% drop_na(Additional_BandType) #2017-2023 wing recaptures
fall_recap <- fall_recap %>% mutate(CaptureDate = as.Date(CaptureDate, format="%Y/%m/%d"))

fall_recap$Year <- lubridate::year(fall_recap$CaptureDate)
fall_recap <- fall_recap %>% relocate('Year', .after = `New Recap`)

fall_recap <- fall_recap%>% #removed bird that were caught multiple times 
  distinct(Band_ID, .keep_all = TRUE)
fall_recap <- fall_recap%>% #removed bird that were caught multiple times in the same year
  distinct(Band_ID, Year, .keep_all = TRUE)



############################################################################################
############################################################################################
################  Fall TRAP DAYS

#EXCEL FILES
# Capture History 2000-2017 Fall 
# TT Data 2017-2023 (has just fall trap data)

Capture_History_2000_2017_Fall <-  Capture_History_2000_2017_Fall %>%
  mutate(`1st_CaptureDate` = as.Date(`1st_CaptureDate`, format="%Y/%m/%d"))

TT_A <- select(Capture_History_2000_2017_Fall, c('Bird_ID', '1st_CaptureDate', '1st_Age'))
colnames(TT_A)[2] <- "CaptureDate"
colnames(TT_A)[3] <- "Age"
TT_A <- TT_A %>% filter(year(`CaptureDate`) != 2017) ## remove data from year 2017

TT_B <- select(TT_Data_2017_2023, c('Band_ID', 'CaptureDate', 'Age'))
colnames(TT_B)[1] <- "Bird_ID"

#create dataframe of all captured fall birds and remove summer months captures
fall_trap_data <- rbind(TT_A, TT_B)
fall_trap_data <- fall_trap_data %>% 
  mutate(CaptureDate = as.Date(CaptureDate, format = "%Y/%m/%d")) %>%
  filter(month(CaptureDate) != 06)%>%
  filter(month(CaptureDate) != 07)%>%
  filter(month(CaptureDate) != 08)%>%
  filter(month(CaptureDate) != 09)
fall_trap_data$Year <- lubridate::year(fall_trap_data$CaptureDate)

fall_trap_data$Age[fall_trap_data$Age == 'J'] <- 'juvenile'
fall_trap_data$Age[fall_trap_data$Age == 'A'] <- 'Adult'
fall_trap_data$Age[fall_trap_data$Age == 'adult'] <- 'Adult'
fall_trap_data <- fall_trap_data %>% filter(Age != 'N') #remove dead trapped birds

fall_trap_nights <- fall_trap_data %>% #number of trap nights 
  distinct(CaptureDate, .keep_all = TRUE)
fall_trap_indiv <- fall_trap_data %>% #number of individuals trapped
  distinct(Bird_ID, Year, .keep_all = TRUE)

age_ratio <- fall_trap_data %>%
  group_by(Year, Age) %>%
  summarize(Count = n())


ggplot(age_ratio, aes(x = Year, y = Count, fill = Age))+
  geom_col(position = position_dodge(0.5)) +
  scale_x_continuous(breaks = seq(min(2000), max(2023), by = 2),
                     labels = seq(min(2000), max(2023), by= 2)) +
  geom_text(aes(label = Count, group = Age), 
            position = position_dodge(0.5),
            vjust = -0.3, size = 2.5)
  

############################################################################################
############################################################################################
################  NEST DATA

#EXCEL FILES
# NestData_2000_2013
# Nesting_Date_TTRS_2014_2023

NestData_2000_2013 <- NestData_2000_2013 %>%
  mutate(`Incubation date` = as.Date(`Incubation date`, format="%m/%d/%Y"))
NestData_2000_2013 <- NestData_2000_2013 %>%
  mutate(`Fate date` = as.Date(`Fate date`, format="%m/%d/%Y"))

colnames(NestData_2000_2013) [2] <- "Age"
colnames(NestData_2000_2013) [7] <- "Clutch Size" 
colnames(NestData_2000_2013) [9] <- "Fate"
colnames(NestData_2000_2013) [10] <- "Hatched"
colnames(NestData_2000_2013) [18] <- "Sex"
Nest_Data_2000_2023 <- select(NestData_2000_2013, c("Bird ID", "RadioFreq", "Sex", "Age", "Incubation date", "Fate date", "Fate", "Clutch Size", "Hatched", "Comments"))

Nesting_Data_TTRS_2014_2023 <- Nesting_Data_TTRS_2014_2023 %>%
  mutate(Date_Found= as.Date(Date_Found, format="%m/%d/%Y"))
Nesting_Data_TTRS_2014_2023 <- Nesting_Data_TTRS_2014_2023 %>%
  mutate(Fate_Date= as.Date(Fate_Date, format="%m/%d/%Y"))

colnames(Nesting_Data_TTRS_2014_2023)[1] <- "Bird ID"
colnames(Nesting_Data_TTRS_2014_2023)[2] <- "RadioFreq"
colnames(Nesting_Data_TTRS_2014_2023)[5] <- "Incubation date"
colnames(Nesting_Data_TTRS_2014_2023)[6] <- "Fate date"
colnames(Nesting_Data_TTRS_2014_2023)[8] <- "Clutch Size"

Nest_Data_2000_2023 <- rbind(Nest_Data_2000_2023, Nesting_Data_TTRS_2014_2023)

Nest_Data_2000_2023$Sex[Nest_Data_2000_2023$Sex == 'female'] <- 'F'
Nest_Data_2000_2023$Sex[Nest_Data_2000_2023$Sex == 'male'] <- 'M'
Nest_Data_2000_2023$Sex[Nest_Data_2000_2023$Sex == 'f'] <- 'F'
Nest_Data_2000_2023$Age[Nest_Data_2000_2023$Age == 'adult'] <- 'A'
Nest_Data_2000_2023$Age[Nest_Data_2000_2023$Age == 'juvenile'] <- 'J'
Nest_Data_2000_2023$Fate[Nest_Data_2000_2023$Fate == 'Yes'] <- 'H'
Nest_Data_2000_2023$Fate[Nest_Data_2000_2023$Fate == 'No'] <- 'D'

Nest_Data_2000_2023$Year  <- lubridate::year(Nest_Data_2000_2023$`Incubation date`)
NEST_FATE <- Nest_Data_2000_2023 %>%
  group_by(Year,Fate) %>% summarize(Count = n())
  
ggplot(NEST_FATE, aes(x = Year, y = Count, fill = Fate))+
  geom_col(position = position_dodge(0.5)) +
  scale_x_continuous(breaks = seq(min(2000), max(2023), by = 2),
                     labels = seq(min(2000), max(2023), by= 2)) +
  geom_text(aes(label = Count, group = Fate), 
            position = position_dodge(0.5),
            vjust = -0.3, size = 2.5)




