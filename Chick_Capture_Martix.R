library(dplyr)

str(Chick_Master_2000_2023_working)

Chick_Master_2000_2023_working$Month <- format(as.Date(Chick_Master_2000_2023_working$date, format="%Y-%m-%d"), "%m") 

chick_data <- Chick_Master_2000_2023_working %>%
    mutate(Season = case_when(
      Month %in% c("05", "06", "07", "08", "09", "10") & status == "capture" ~ "Brood",
      Month %in% c("11", "12") & status == "capture" ~ "Nov",
      year == 2000 & Month == "10" & status == "capture" ~ "BROOD",
      year == 2011 & Month == "10" & status == "capture" ~ "BROOD",
      Month == "01" & status == "capture" ~ "Jan",
      Month == "03" & status == "capture" ~ "March",
      Month %in% c("12", "01", "02") & status == "HUNT" ~ "Hunt",
      status == "Trap" ~ "TrapDEAD",
      status == "HuntTrap" ~ "HUNTTRAP",
      TRUE ~ NA_character_  # Keeps other months or statuses as NA
    ),
    StatusValue = case_when(  # Assign numerical values based on the status
      status == "Trap" ~ 2,
      status == "Hunt" ~ 2,
      status == "HuntTrap" ~ 2,
      status == "capture" ~ 1,
      status == "Brood" ~ 1,
      TRUE ~ 0  # If status is NA or irrelevant, assign 0
    )
  )

#Filter out rows that don't match any of the above conditions
chick_data_filtered<- chick_data %>%
  filter(!is.na(Season))

#Summarize the data to ensure there's only one value per ID, year, and CapturePeriod
chick_data_summarized <- chick_data_filtered %>%
  group_by(id, year, Season) %>%
  summarize(StatusValue = max(StatusValue), .groups = 'drop')  # Take max to prioritize Trap/Hunt over capture

# Step 4: Pivot the data into a capture history matrix
capture_history <- chick_data_summarized %>%
  pivot_wider(
    names_from = c(year, Season),  # Create columns for year and capture periods
    values_from = StatusValue,            # Use the summarized status values
    values_fill = list(StatusValue = 0)   # Fill missing values with 0
  )

# View the capture history matrix
print(capture_history)


  
