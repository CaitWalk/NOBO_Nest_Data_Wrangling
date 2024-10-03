library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(tidyverse)

#Excel Files
# Capture History 2000-2017 Fall
# TT Data 2017-2023

Capture_History_2000_2017_Fall$`1st_Age`[Capture_History_2000_2017_Fall$`1st_Age`== 'immature'] <- 'juvenile' ##replace immature with juvenile

############################################################################################

juv_cap <-Capture_History_2000_2017_Fall %>% filter(`1st_Age`=='juvenile') ## subset to just juvenile captures in fall

juv_cap <- juv_cap %>%
  mutate(`1st_CaptureDate` = as.Date(`1st_CaptureDate`, format="%Y/%m/%d")) %>% ## remove data from year 2000
  filter(year(`1st_CaptureDate`) != 2000)  

juv_cap <- juv_cap %>%
  mutate(`1st_CaptureDate` = as.Date(`1st_CaptureDate`, format="%Y/%m/%d")) %>%
  filter(year(`1st_CaptureDate`) != 2017) ## remove data from year 2017

juv_cap$`1st_ImmAge`[is.na(juv_cap$`1st_ImmAge`)] <-150 ## change an blank/NA to 150

str(juv_cap)
#################################################################################################

juv_cap$'1st_CaptureDate' <- as.Date(juv_cap$'1st_CaptureDate')  ##convert to date structure
juv_cap$'2nd_CaptureDate' <- as.Date(juv_cap$'2nd_CaptureDate')
juv_cap$'3rd_CaptureDate' <- as.Date(juv_cap$'3rd_CaptureDate')
juv_cap$'4th_CaptureDate' <- as.Date(juv_cap$'4th_CaptureDate')
juv_cap$'5th_CaptureDate' <- as.Date(juv_cap$'5th_CaptureDate')
juv_cap$'6th_CaptureDate' <- as.Date(juv_cap$'6th_CaptureDate')

View(juv_cap)
#############################################################################################################

##Convert data from wide to long format i.e. only one capture per row
long_test <- juv_cap %>%
  pivot_longer(
    cols = starts_with(c('1st_', '2nd_', '3rd_', '4th_', '5th_', '6th_')),
    names_to = c('Capture','.value'),
    names_sep = '_',
    values_drop_na = TRUE
  )
       View(long_test)                          
##########################################################################################


####have to figure out how to make it only apply to 1st capture

long_test$Hatch <-((long_test$CaptureDate)-(long_test$ImmAge)) ## create new column Hatch and calculate est hatch data -> puts at end

long_test$Hatch <- ifelse(long_test$Capture == "1st", 
                          format(as.Date(long_test$CaptureDate - long_test$ImmAge), "%Y-%m-%d"), # just those that were on thei 1st capture
                          NA)

long_test <- long_test %>% relocate('Hatch', .after = 'ImmAge') ## hatch column moved from end next to imm age

long_test$Month <- format(as.Date(long_test$Hatch, format ="%Y-%m-%d"), "%m") ## create new column month that just pulls out the hatch month

long_test <- long_test %>% relocate(Month, .after = Hatch) ## move month column

long_test$Year <- format(as.Date(long_test$Hatch, format ="%Y-%m-%d"), "%Y") ## create new column month that just pulls out the hatch month

long_test <- long_test %>% relocate(Year, .after = Month) ## move month column

long_test <- long_test[,-6] #remove the 6th column BirdIDrecap

                          
#######################################################################################

##scatterplot with connecting lines
# Pre-calculate counts
count_data <- long_test %>%
  group_by(Year, Month) %>%
  summarize(Count = n())

ggplot(count_data, aes(x = as.numeric(Year), y = Count, color = Month, group = Month)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_line(position = position_dodge(width = 0.5)) +
  labs(title = "Hatched per Month based on Fall captures",
       x = "Year",
       y = "Count") +
  scale_color_discrete(name = "Hatch Month")  +
  coord_cartesian(ylim = c(0, 200)) +
  scale_x_continuous(breaks = seq(min(2000), max(2024), by = 2),
                     labels = seq(min(2000), max(2024), by= 2)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
############################################################################
Juv_2017_2023 <- subset(TT_Data_2017_2023, Age == 'J') # new dataframe of just juvenile captures

Juv_2017_2023$CaptureDate <- as.Date(Juv_2017_2023$CaptureDate) #change structure to date
Juv_2017_2023$ImmAge <- as.numeric(Juv_2017_2023$ImmAge) #change structure to numeric

##change immage from molt to days
Juv_2017_2023['ImmAge'][Juv_2017_2023['ImmAge'] == 2.75] <- 52
Juv_2017_2023['ImmAge'][Juv_2017_2023['ImmAge'] == 3.25] <- 47
Juv_2017_2023['ImmAge'][Juv_2017_2023['ImmAge'] == 3.75] <- 56
Juv_2017_2023['ImmAge'][Juv_2017_2023['ImmAge'] == 4.25] <- 53
Juv_2017_2023['ImmAge'][Juv_2017_2023['ImmAge'] == 4.50] <- 57
Juv_2017_2023['ImmAge'][Juv_2017_2023['ImmAge'] == 5.25] <- 60
Juv_2017_2023['ImmAge'][Juv_2017_2023['ImmAge'] == 5.50] <- 65
Juv_2017_2023['ImmAge'][Juv_2017_2023['ImmAge'] == 5.75] <- 73
Juv_2017_2023['ImmAge'][Juv_2017_2023['ImmAge'] == 6.25] <- 68
Juv_2017_2023['ImmAge'][Juv_2017_2023['ImmAge'] == 6.50] <- 74
Juv_2017_2023['ImmAge'][Juv_2017_2023['ImmAge'] == 6.75] <- 82
Juv_2017_2023['ImmAge'][Juv_2017_2023['ImmAge'] == 7.00] <- 74
Juv_2017_2023['ImmAge'][Juv_2017_2023['ImmAge'] == 7.25] <- 83
Juv_2017_2023['ImmAge'][Juv_2017_2023['ImmAge'] == 7.50] <- 93
Juv_2017_2023['ImmAge'][Juv_2017_2023['ImmAge'] == 7.75] <- 105
Juv_2017_2023['ImmAge'][Juv_2017_2023['ImmAge'] == 8.00] <- 101
Juv_2017_2023['ImmAge'][Juv_2017_2023['ImmAge'] == 8.25] <- 111
Juv_2017_2023['ImmAge'][Juv_2017_2023['ImmAge'] == 8.50] <- 119
Juv_2017_2023['ImmAge'][Juv_2017_2023['ImmAge'] == 8.75] <- 127
Juv_2017_2023['ImmAge'][Juv_2017_2023['ImmAge'] == 9.25] <- 150
Juv_2017_2023['ImmAge'][Juv_2017_2023['ImmAge'] == 9.50] <- 150
Juv_2017_2023['ImmAge'][Juv_2017_2023['ImmAge'] == 9.75] <- 150
Juv_2017_2023['ImmAge'][Juv_2017_2023['ImmAge'] == 10.25] <- 150

Juv_2017_2023$Hatch <- ((Juv_2017_2023$CaptureDate)-(Juv_2017_2023$ImmAge)) #create a new column of hatch 

Juv_2017_2023 <- Juv_2017_2023 %>% relocate('Hatch', .after = 'ImmAge') ## hatch column moved from end next to imm age

Juv_2017_2023$Month <- format(as.Date(Juv_2017_2023$Hatch, format ="%Y-%m-%d"), "%m") ## create new column month that just pulls out the hatch month

Juv_2017_2023 <- Juv_2017_2023 %>% relocate(Month, .after = Hatch) ## move month column

Juv_2017_2023$Year <- format(as.Date(Juv_2017_2023$Hatch, format ="%Y-%m-%d"), "%Y") ## create new column month that just pulls out the hatch year

Juv_2017_2023 <- Juv_2017_2023 %>% relocate(Year, .after = Month) ## move month column

###############################################################################################

Juv_2017_2023_test <- Juv_2017_2023[!duplicated(Juv_2017_2023$Band_ID),] #remove duplicate bandID values

Juv_2017_2023_test <- Juv_2017_2023[,-5:-6] #remove old/new recap column

Juv_2017_2023_test$Capture <- '1st' #create a new column capture and place NA in it
   
Juv_2017_2023_test <-Juv_2017_2023_test %>% relocate('Capture', .after = 'Additional_BandType')

colnames(Juv_2017_2023_test)[1] <- "Bird_ID" #change column from band_id to bird_ID

Juv_2017_2023_test$ImmAge[is.na(Juv_2017_2023_test$ImmAge)] <- 150 ## change an blank/NA to 150

###############################################################################################
all_data <- rbind(long_test, Juv_2017_2023_test) # new database combining to two sets of data

all_data$Age[all_data$Age=='J'] <- 'juvenile'  #change age from J to juvenile
all_data$Age[all_data$Age=='Juvenile'] <- 'juvenile'

all_data$Hatch <- ((all_data$CaptureDate)-(all_data$ImmAge))

all_juv_fall <- all_data %>% filter(Age == 'juvenile')  ## subset to just juvenile captures 


all_juv_fall <- all_juv_fall %>% #remove row that were captured a different time (i.e. dont have a hatch date)
  drop_na(Hatch)


all_juv_fall$CaptureMonth <- format(as.Date(all_juv_fall$CaptureDate, format ="%Y-%m-%d"), "%m") ## create new column month that just pulls out the capture month

all_juv_fall <- all_juv_fall %>% relocate(CaptureMonth, .after = CaptureDate) ## move month column

fall_only_juv <- all_juv_fall[all_juv_fall$CaptureMonth %in% c("10", "11", "12"),]

fall_only_juv$Month <- format(as.Date(fall_only_juv$Hatch, format ="%Y-%m-%d"), "%m") ## create new column month that just pulls out the hatch month

fall_only_juv$Year <- format(as.Date(fall_only_juv$Hatch, format ="%Y-%m-%d"), "%Y") ## create new column month that just pulls out the hatch year

hatch_prop <- fall_only_juv %>%
  group_by(Year, Month) %>% #group by year
  summarise(cnt=n()) %>% #counting how many by group
  mutate(props = round(cnt/sum(cnt),2)) #getting frequency
  
hatch_prop
################################################################################################
##scatterplot with connecting lines
# Pre-calculate counts
all_count_data <- all_juv_fall %>%
  group_by(Year, Month) %>%
  summarize(Count = n())

#all years number of hatched chicks/month
ggplot(all_count_data, aes(x = as.numeric(Year), y = Count, color = Month, group = Month)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_line(position = position_dodge(width = 0.5)) +
  labs(title = "Hatched per Month based on Fall captures",
       x = "Year",
       y = "Count") +
  scale_color_discrete(name = "Hatch Month")  +
  coord_cartesian(ylim = c(0, 200)) +
  scale_x_continuous(breaks = seq(min(2000), max(2024), by = 2),
                     labels = seq(min(2000), max(2024), by= 2)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_minimal()

#proportion of chicks hatched/month based on total number
ggplot(hatch_prop, aes(x = Month, y = props, group = Year, color = factor(Year))) +
  geom_line() +
  geom_point() +
  labs(title = "Proportion of Hatches Over Months (Separate by Year)",
       x = "Month",
       y = "Proportion of Hatch") +
  theme_minimal() + facet_wrap(~ Year)


csum <- hatch_prop %>% 
        group_by(Year) %>% 
        mutate(cumsum = cumsum(cnt), year_tot = sum(cnt), 
               cumprop = cumsum/year_tot)

#cumulative proportion of hatches/month 
ggplot(csum, aes(x = Month, y = cumprop, group = Year, color = factor(Year))) +
  geom_line() +
  geom_point() +
  labs(title = "Cumulative Proportion of Hatches Over Months (Separate by Year)",
       x = "Month",
       y = "Cumulative Proportion of Hatch") +
  theme_minimal() + facet_wrap(~ Year)

######################
#for capture histry martix if i lose the ID column
id_column <- capture_history$id

# Get the other column names excluding 'ID'
col_names <- colnames(capture_history)[-1]

# Extract the year part from the column names using a regular expression
sorted_col_names <- col_names[order(as.numeric(sub("^(\\d+).*", "\\1", col_names)))]

# Recreate the data frame with 'ID' as the first column, followed by the sorted columns
capture_history_ordered <- capture_history[, c("id", sorted_col_names)]

# View the reordered capture history matrix
print(capture_history_ordered)



