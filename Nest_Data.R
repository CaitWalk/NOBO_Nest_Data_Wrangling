library(lubridate) #for working with dates
library(tidyr)
library(dplyr)
library(ggplot2)

#Excel Files
  #NestingTBL
  #NestData_2000_2023
  #Capture_history_2000_2017

  # Nests_2000_2024

######################################################################################################################################  

Nests_2000_2024 <- Nests_2000_2024 %>%
  mutate(Year = year(ymd(`Incubation date`)))

Nests_2000_2024 <- Nests_2000_2024 %>%
  mutate(Hatch_Rate = case_when(
    Successful == "Yes" & `Number hatched` >`Initial clutch size` ~ 1,  #hatch rate 1 if eggs added
    Successful == "Yes" ~ `Number hatched` / `Initial clutch size`,     #else, calc hatch rate 
    TRUE ~ NA_real_                                                     #set as NA if not successful
    ))

summary_data <- Nests_2000_2024 %>%
  group_by(Year) %>%
  summarize(
    Successful_Nests = sum(Successful == "Yes", na.rm = TRUE),
    Avg_Clutch_Size = mean(`Initial clutch size`, na.rm = TRUE),
    Total_Laid = sum(`Initial clutch size`, na.rm = TRUE),
    Total_Hatched = sum(`Number hatched`, na.rm = TRUE),
    Hatchability = sum(`Number hatched`, na.rm = TRUE) / sum(`Initial clutch size`, na.rm = TRUE), #which is the correct way to cal this 
    Avg_Hatchability = mean(Hatch_Rate, na.rm = TRUE) #which is the correct way to cal this
  )

######################################################################################################################################  
###################################################
#dataset for nest success probabiliy with CI
success_prob <- Nests_2000_2024 %>%
  group_by(Year) %>%
  summarize(
    Total_Nests = n(),
    Successful_Nests = sum(Successful == "Yes", na.rm = TRUE),
    Probability_Nest_Success = as.numeric(Successful_Nests / Total_Nests),
    SD_Successful = as.numeric(sqrt((Probability_Nest_Success * (1 - Probability_Nest_Success)) / Total_Nests)),
    Lower_CI = as.numeric(binom.test(Successful_Nests, Total_Nests, conf.level = 0.95)$conf.int[1]),
    Upper_CI = as.numeric(binom.test(Successful_Nests, Total_Nests, conf.level = 0.95)$conf.int[2])
  )
#adding average row at the end of the table
success_avg <- colMeans(success_prob[,-1])
success_w_average <- rbind(success_prob, c("Average", success_avg))

####nest success probability with CI
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

###nest success probability with SD
ggplot(success_prob, aes(x = Year, y = Probability_Nest_Success)) +
  geom_point(color = "cornflowerblue", size = 3) +
  geom_errorbar(aes(ymin = Probability_Nest_Success - SD_Successful,
                    ymax = Probability_Nest_Success + SD_Successful),
                width = 0.3, color = "black") +
  labs(
    title = "Probability of Nest Success with SD",
    x = "Year",
    y = "Probability of Nest Success"
  ) +
  theme_minimal()

######################################################################################################################################  
###################################### hatchability  #NEED HELP!!!!!!!!!!!!!
summary_hatchability <- Nests_2000_2024 %>%
  filter(!is.na(Hatch_Rate)) %>%
  group_by(Year) %>%
  summarize(
    Mean_Hatchability = mean(Hatch_Rate, na.rm = TRUE), #avg hatchability for each year
    SD_Hatchability = as.numeric(sqrt(Mean_Hatchability * (1 - Mean_Hatchability))), 
    N = n(),
    SE = SD_Hatchability / sqrt(N), # Standard error of the mean
    Lower_CI = Mean_Hatchability - qt(0.975, df = N - 1) * SE, # Lower 95% CI
    Upper_CI = Mean_Hatchability + qt(0.975, df = N - 1) * SE  # Upper 95% CI
  )

###plot with CI as the error bars 
ggplot(summary_hatchability, aes(x = Year, y = Mean_Hatchability)) +
  geom_point(size = 3, color = "darkgreen") +  # Mean hatchability points
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "black") +  # Error bars for CI
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +  # y-axis in percentage
  labs(
    title = "Mean Hatchability with 95% CI",
    x = "Year",
    y = "Mean Hatchability"
  ) +
  theme_minimal(base_size = 14)

### plot with SE as the error bars 
ggplot(summary_hatchability, aes(x = Year, y = Mean_Hatchability)) +
  geom_point(size = 3, color = "darkgreen") +  # Mean hatchability points
  geom_errorbar(aes(ymin = Mean_Hatchability - SD_Hatchability , 
                    ymax = Mean_Hatchability + SD_Hatchability ), 
                width = 0.2, color = "black") +  # Error bars
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +  # y-axis in percentage
  labs(
    title = "Mean Hatchability with SD",
    x = "Year",
    y = "Mean Hatchability"
  ) +
  theme_minimal(base_size = 14)

######################################################################################################################################  
##################################### nest sex information
nest_proportions <- Nests_2000_2024 %>%
  group_by(Year, Sex) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Year) %>%
  mutate(proportion = count / sum(count))  # Proportion of each sex per year

# Function to calculate binomial confidence intervals
calculate_ci <- function(count, total, conf_level = 0.95) {
  binom_ci <- binom.test(count, total, conf.level = conf_level)
  return(c(lower = binom_ci$conf.int[1], upper = binom_ci$conf.int[2]))
}
sex_dist <- Nests_2000_2024 %>%
  group_by(Year)%>%
  summarize(
    Total_Nests = n(),
    proprt_female = sum(Sex == "female", na.rm = TRUE) / Total_Nests, #proportion of female nests
    proprt_male = sum(Sex == "male", na.rm = TRUE) / Total_Nests, #proportion of male nests 
    # Calculate CI for female nests
    female_ci = list(calculate_ci(sum(Sex == "female", na.rm = TRUE), Total_Nests)),
    # Calculate CI for male nests
    male_ci = list(calculate_ci(sum(Sex == "male", na.rm = TRUE), Total_Nests))
  ) %>%
  # Separate the lower and upper bounds for each CI
  mutate(
    female_ci_lower = sapply(female_ci, function(x) x[1]),
    female_ci_upper = sapply(female_ci, function(x) x[2]),
    male_ci_lower = sapply(male_ci, function(x) x[1]),
    male_ci_upper = sapply(male_ci, function(x) x[2])
  ) %>%
  select(-female_ci, -male_ci)  # Remove the list columns

#ggplot for proportion of female and male nests each year 
ggplot(sex_dist, aes(x = Year)) +
  # Plot proportion of female nests with CI
  geom_point(aes(y = proprt_female), color = "pink", size = 3) +
  geom_errorbar(aes(ymin = female_ci_lower, ymax = female_ci_upper), color = "pink", width = 0.2) +
  
  # Plot proportion of male nests with CI
  geom_point(aes(y = proprt_male), color = "blue", size = 3) +
    geom_errorbar(aes(ymin = male_ci_lower, ymax = male_ci_upper), color = "blue", width = 0.2) +
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
