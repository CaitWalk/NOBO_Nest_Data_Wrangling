Capture_History_2000_2017_Fall$`1st_Age`[Capture_History_2000_2017_Fall$`1st_Age`== 'immature'] <- 'juvenile' ##replace immature with juvenile

Capture_History_2000_2017_Fall %>% <- Capture_History_2000_2017_Fall %>%
  mutate(`1st_CaptureDate` = as.Date(`1st_CaptureDate`, format="%Y/%m/%d")) %>%
  filter(year(`1st_CaptureDate`) != 2017) ## remove data from year 2017

Capture_History_2000_2017_Fall$`1st_ImmAge`[is.na(Capture_History_2000_2017_Fall$`1st_ImmAge`)] <-150 ## change an blank/NA to 150

trap_00_16 <-Capture_History_2000_2017_Fall
trap_00_16$'1st_CaptureDate' <- as.Date(trap_00_16$'1st_CaptureDate')  ##convert to date structure
trap_00_16$'2nd_CaptureDate' <- as.Date(trap_00_16$'2nd_CaptureDate')
trap_00_16$'3rd_CaptureDate' <- as.Date(trap_00_16$'3rd_CaptureDate')
trap_00_16$'4th_CaptureDate' <- as.Date(trap_00_16$'4th_CaptureDate')
trap_00_16$'5th_CaptureDate' <- as.Date(trap_00_16$'5th_CaptureDate')
trap_00_16$'6th_CaptureDate' <- as.Date(trap_00_16$'6th_CaptureDate')

##Convert data from wide to long format i.e. only one capture per row
long_trap_00_16<- trap_00_16 %>%
  pivot_longer(
    cols = starts_with(c('1st_', '2nd_', '3rd_', '4th_', '5th_', '6th_')),
    names_to = c('Capture','.value'),
    names_sep = '_',
    values_drop_na = TRUE
  )

long_trap_00_16$CaptureYear<- format(as.Date(long_trap_00_16$CaptureDate, format ="%Y-%m-%d"), "%Y") ## create new column month that just pulls out the capture month
long_trap_00_16$CaptureMonth<- format(as.Date(long_trap_00_16$CaptureDate, format ="%Y-%m-%d"), "%m")
long_trap_00_16$CaptureDay<- format(as.Date(long_trap_00_16$CaptureDate, format ="%Y-%m-%d"), "%d")

fall_trap_00_16 <- long_trap_00_16[long_trap_00_16$CaptureMonth %in% c("10", "11", "12"),]

fall_days<- as.data.frame(table(fall_trap_00_16$CaptureDate))

fall_days$CaptureYear<- format(as.Date(fall_days$Var1, format ="%Y-%m-%d"), "%Y") 
fall_days$CaptureDay<- format(as.Date(fall_days$Var1, format ="%Y-%m-%d"), "%d")

unique_dates_per_year <- fall_days %>%
  group_by(CaptureYear) %>%
  summarise(unique_dates = n_distinct(Var1))

print(fall_days)

