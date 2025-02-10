library(stringr)
library(writexl)

#combined the following files:
  # NestData_2000_2013
  # Nesting_Data_TTRS_2014-2024
  # BirdID_Sex_2000_2017

#change output
Nesting_Data_TTRS_2014_2024$Successful[Nesting_Data_TTRS_2014_2024$Successful == 'D'] <- 'No'
Nesting_Data_TTRS_2014_2024$Successful[Nesting_Data_TTRS_2014_2024$Successful == 'U'] <- 'No'
Nesting_Data_TTRS_2014_2024$Successful[Nesting_Data_TTRS_2014_2024$Successful == 'A'] <- 'No'
Nesting_Data_TTRS_2014_2024$Successful[Nesting_Data_TTRS_2014_2024$Successful == 'H'] <- 'Yes'
Nesting_Data_TTRS_2014_2024$Successful[Nesting_Data_TTRS_2014_2024$Successful == 'S'] <- 'Yes'

NestData_2000_2013$`Incubation date` <- as.Date(NestData_2000_2013$`Incubation date`, format = "%m/%d/%Y")
NestData_2000_2013$`Fate date` <- as.Date(NestData_2000_2013$`Fate date`, format = "%m/%d/%Y")


NestData_2000_2013$Successful[NestData_2000_2013$Successful == 'A'] <- 'No'
######################################################################################################################################  
#go thru the sex data and only pull out one
BirdID_Sex_2000_2017 <- BirdID_Sex_2000_2017 %>%
  rowwise() %>%  # Process each row independently
  mutate(Sex = {
    values <- c_across(`1st Sex`:`3rd Sex`)  # Collect values from columns
    first_valid <- values[!(values %in% c("immature", "unknown"))]  # Remove "Immature" and "Unknown"
    if (length(first_valid) > 0) first_valid[1] else tail(values, 1)  # Pick first valid or last fallback
  }) %>%
  ungroup() %>%
  select(`Bird ID`, Sex)  # Keep only relevant columns

### making sure there are leading 0s and 6 total numbers for Bird ID
NestData_2000_2013$`Bird ID` <- str_pad(NestData_2000_2013$`Bird ID`, width = 6, side = "left", pad = "0")
BirdID_Sex_2000_2017$`Bird ID` <- str_pad(BirdID_Sex_2000_2017$`Bird ID`, width = 6, side = "left", pad = "0")

#### Merge the two datasets 
NestData_2000_2013 <- NestData_2000_2013 %>%
  left_join(BirdID_Sex_2000_2017 %>% select(`Bird ID`, Sex), by = "Bird ID", relationship = "many-to-many")

### both of theses codes are able to join the two columns 
Nests_2000_2013 <- NestData_2000_2013 %>%
    mutate(Sex = coalesce(SEX, Sex))%>%
   select(-SEX)

NestData_2000_2013$Sex <-ifelse(is.na(NestData_2000_2013$Sex), NestData_2000_2013$SEX, NestData_2000_2013$Sex)
NestData_2000_2013$SEX <- NULL  # Remove 'SEX' column

Nests_2000_2013 <- Nests_2000_2013[, -c(8,9)]


Nests_2000_2013 <- Nests_2000_2013 %>% relocate('Sex', .after = 'Frequency')
######################################################################################################################################  
Nesting_Data_TTRS_2014_2024 <- Nesting_Data_TTRS_2014_2024 %>%
  rename('Bird ID' = 'Band ID')

Nesting_Data_TTRS_2014_2024$`Incubation date` <- as.Date(Nesting_Data_TTRS_2014_2024$`Incubation date`, format = "%m/%d/%Y")
Nesting_Data_TTRS_2014_2024$`Fate date` <- as.Date(Nesting_Data_TTRS_2014_2024$`Fate date`, format = "%m/%d/%Y")

Nesting_Data_TTRS_2014_2024$Sex[Nesting_Data_TTRS_2014_2024$Sex == 'F'] <- 'female'
Nesting_Data_TTRS_2014_2024$Sex[Nesting_Data_TTRS_2014_2024$Sex == 'f'] <- 'female'
Nesting_Data_TTRS_2014_2024$Sex[Nesting_Data_TTRS_2014_2024$Sex == 'Female'] <- 'female'
Nesting_Data_TTRS_2014_2024$Sex[Nesting_Data_TTRS_2014_2024$Sex == 'M'] <- 'male'
Nesting_Data_TTRS_2014_2024$Sex[Nesting_Data_TTRS_2014_2024$Sex == 'm'] <- 'male'
Nesting_Data_TTRS_2014_2024$Sex[Nesting_Data_TTRS_2014_2024$Sex == 'Male'] <- 'male'

#change output
Nesting_Data_TTRS_2014_2024$Successful[Nesting_Data_TTRS_2014_2024$Successful == 'D'] <- 'No'
Nesting_Data_TTRS_2014_2024$Successful[Nesting_Data_TTRS_2014_2024$Successful == 'U'] <- 'No'
Nesting_Data_TTRS_2014_2024$Successful[Nesting_Data_TTRS_2014_2024$Successful == 'A'] <- 'No'
Nesting_Data_TTRS_2014_2024$Successful[Nesting_Data_TTRS_2014_2024$Successful == 'H'] <- 'Yes'
Nesting_Data_TTRS_2014_2024$Successful[Nesting_Data_TTRS_2014_2024$Successful == 'S'] <- 'Yes'

#merge the two datasets 
Nests_2000_2024 <- rbind(Nests_2000_2013, Nesting_Data_TTRS_2014_2024)

write_xlsx(Nests_2000_2024, "/Volumes/Elements/Long_Term_Data_Tall_Timbers/Nest Data/Nests_2000_2024.xlsx")
