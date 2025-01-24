library(lubridate) #for working with dates
library(tidyr)
library(dplyr)

#Excel Files
  #NestingTBL
  #NestData_2000_2023

NestingTBL$`Incubation date` <- as.Date(NestingTBL$`Incubation date`, format = "%m/%d/%Y")
NestingTBL$`Fate date` <- as.Date(NestingTBL$`Fate date`, format = "%m/%d/%Y")

NestingTBL <- NestingTBL %>%
  filter(year(`Incubation date`) !=1999) #removed year 1999 data

NestingTBL <- NestingTBL[, -c(3,5,6,8,11,15,16,17,18,19,20,21)] #removed unwanted columns

NestingTBL <- NestingTBL %>%
  mutate(Year = year(ymd(`Incubation date`)),
  Hatch_Rate =`Number hatched` / `Initial clutch size`)

summary_data <- NestingTBL %>%
  group_by(Year) %>%
  summarize(
    Successful_Nests = sum(Successful == "Yes", na.rm = TRUE),
    Avg_Clutch_Size = mean(`Initial clutch size`, na.rm = TRUE),
    Total_Laid = sum(`Initial clutch size`, na.rm = TRUE),
    Total_Hatched = sum(`Number hatched`, na.rm = TRUE),
    Hatchability = sum(`Number hatched`, na.rm = TRUE) / sum(`Initial clutch size`, na.rm = TRUE),
    Avg_Hatchability = mean(Hatch_Rate, na.rm = TRUE)
  )

success_prob <- NestingTBL %>%
  group_by(Year) %>%
  summarize(
    Total_Nests = n(),
    Successful_Nests = sum(Successful == "Yes", na.rm = TRUE),
    Probability_Nest_Success = Successful_Nests / Total_Nests,
    Lower_CI = binom.test(Successful_Nests, Total_Nests, conf.level = 0.95)$conf.int[1],
    Upper_CI = binom.test(Successful_Nests, Total_Nests, conf.level = 0.95)$conf.int[2]
  )

ggplot(success_prob, aes(x = Year, y = Probability_Nest_Success)) +
  geom_point(size = 3, color = "blue") +  # Plot points for probabilities
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "blue") +  # Add error bars
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +  # Show y-axis in percentage
  labs(
    title = "Probability of Nest Success with 95% CI",
    x = "Year",
    y = "Probability of Nest Success"
  ) +
  theme_minimal(base_size = 14)
######################################
summary_hatchability <- NestingTBL %>%
  group_by(Year) %>%
  summarize(
    Mean_Hatchability = mean(Hatch_Rate, na.rm = TRUE), #avg hatchability for each year
    SD_Hatchability = sd(Hatch_Rate, na.rm = TRUE), #srandard deviation for hatchability 
    N = n(), #sample size (number of nest/year)
    SE = SD_Hatchability / sqrt(N), # Standard error of the mean
    Lower_CI = Mean_Hatchability - qt(0.975, df = N - 1) * SE, # Lower 95% CI
    Upper_CI = Mean_Hatchability + qt(0.975, df = N - 1) * SE  # Upper 95% CI
  )

ggplot(summary_hatchability, aes(x = Year, y = Mean_Hatchability)) +
  geom_point(size = 3, color = "darkgreen") +  # Mean hatchability points
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "darkgreen") +  # Error bars for CI
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +  # y-axis in percentage
  labs(
    title = "Mean Hatchability with 95% CI",
    x = "Year",
    y = "Mean Hatchability"
  ) +
  theme_minimal(base_size = 14)

#####################################
#added the sex of each nest to a new dataframe 

NEST_SEX <- NestingTBL%>%
  left_join(NestData_2000_2023 %>% select(`Bird ID`, SEX), by = "Bird ID", relationship = "many-to-many")
NEST_SEX <- NEST_SEX %>% relocate('SEX', .after = 'Bird ID')

#need to figure out birds with sex listed as NA
nest_proportions <- dataset %>%
  group_by(Year, SEX) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Year) %>%
  mutate(proportion = count / sum(count))  # Proportion of each sex per year


# Function to calculate binomial confidence intervals
calculate_ci <- function(count, total, conf_level = 0.95) {
  binom_ci <- binom.test(count, total, conf.level = conf_level)
  return(c(lower = binom_ci$conf.int[1], upper = binom_ci$conf.int[2]))
}
sex_dist <- NEST_SEX %>%
  group_by(Year)%>%
  summarize(
    Total_Nests = n(),
    proprt_female = sum(SEX == "female", na.rm = TRUE) / Total_Nests, #proportion of female nests
    proprt_male = sum(SEX == "male", na.rm = TRUE) / Total_Nests, #proportion of male nests 
    # Calculate CI for female nests
    female_ci = list(calculate_ci(sum(SEX == "female", na.rm = TRUE), Total_Nests)),
    # Calculate CI for male nests
    male_ci = list(calculate_ci(sum(SEX == "male", na.rm = TRUE), Total_Nests))
  ) %>%
  # Separate the lower and upper bounds for each CI
  mutate(
    female_ci_lower = sapply(female_ci, function(x) x[1]),
    female_ci_upper = sapply(female_ci, function(x) x[2]),
    male_ci_lower = sapply(male_ci, function(x) x[1]),
    male_ci_upper = sapply(male_ci, function(x) x[2])
  ) %>%
  select(-female_ci, -male_ci)  # Remove the list columns

ggplot(sex_dist, aes(x = Year)) +
  # Plot proportion of female nests with CI
  geom_point(aes(y = proprt_female), color = "pink", size = 3) +
  geom_errorbar(aes(ymin = female_ci_lower, ymax = female_ci_upper), color = "pink", width = 0.2) +
  
  # Plot proportion of male nests with CI
  geom_point(aes(y = proprt_male), color = "blue", size = 3) +
  geom_errorbar(aes(ymin = male_ci_lower, ymax = male_ci_upper), color = "blue", width = 0.2) +
  
  # Labels and theme adjustments
  labs(
    title = "Proportion of Female and Male Nests with Confidence Intervals",
    x = "Year",
    y = "Proportion",
    color = "Sex"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("pink", "blue")) +
  theme(
    legend.position = "none",  # Optional: Hide legend
    axis.text.x = element_text(angle = 45, hjust = 1)  # Angle the x-axis labels
  )

