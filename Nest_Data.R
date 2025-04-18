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
  mutate(`Incubation date` = as.Date(`Incubation date`, format="%Y/%m/%d")) %>%
  mutate(`Fate date` = as.Date(`Fate date`, format="%Y/%m/%d"))

Nests_2000_2024 <-Nests_2000_2024 %>%
  distinct(`Bird ID`, `Incubation date`, .keep_all = TRUE) #removed any diplicate rows that have the same bird ID and incubation date

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
avg_success <- mean(success_prob$Probability_Nest_Success)

range(success_prob$Probability_Nest_Success)
succ_results <- t.test(success_prob$Probability_Nest_Success)
succ_SD <- sd(x = success_prob$Probability_Nest_Success, na.rm = FALSE)

####nest success probability with CI
ggplot(success_prob, aes(x = Year, y = Probability_Nest_Success)) +
  geom_point(size = 3, color = "blue") +  # Plot points for probabilities
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "blue") +  # Add error bars
  geom_hline(yintercept = avg_success, color = 'red', linetype = "dashed", size = 1) +
  scale_y_continuous(limits = c(0, 1)) +  # Show y-axis in percentage
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
  geom_hline(yintercept = avg_success, color = 'red', linetype = "dashed", size = 1) +
  scale_y_continuous( limits = c(0, 1)) +  # Show y-axis in percentage
  labs(
    title = "Probability of Nest Success with SD",
    x = "Year",
    y = "Probability of Nest Success"
  ) +
  theme_minimal(base_size = 14)

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

hatch_avg <- colMeans(summary_hatchability[-1])
hatchability_w_avg <- rbind(summary_hatchability, c("Average", hatch_avg))

avg_hatch <- mean(summary_hatchability$Mean_Hatchability)


###plot with CI as the error bars 
ggplot(summary_hatchability, aes(x = Year, y = Mean_Hatchability)) +
  geom_point(size = 3, color = "darkgreen") +  # Mean hatchability points
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "black") +  # Error bars for CI
  geom_hline(yintercept = avg_hatch, color = 'red', linetype = "dashed", size = 1) +
  scale_y_continuous(limits = c(0, 1)) +  # y-axis in percentage
  labs(
    title = "Mean Hatchability with 95% CI",
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

ggplot(TTRS_April15_BreedingNumbers, aes(x = Year, y = N.breed, color = Sex, group = Sex)) +
  geom_line(size = 1) +      # Add lines
  geom_point(size = 2) +     # Add points
  scale_color_manual(values = c("F" = "pink", "M" = "blue")) +  # Custom colors
  scale_x_continuous(name = "Year", breaks = unique(TTRS_April15_BreedingNumbers$Year)) +
  labs(title = "Radioed Birds as of April 15",
       x = "Year",
       y = "Number of Individuals",
       color = "Sex") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) 

######################################################################################################################################  
##################################### multiple nests attempts 
multiple_nests <- Nests_2000_2024 %>%
  group_by(`Bird ID`, Year) %>%
  summarise(nest_count = n(), .groups = 'drop') %>%
  filter(nest_count > 1)

summ_multi_nests <- multiple_nests %>%
  group_by(Year) %>%
  summarise(

    Second_Nest = sum(nest_count == 2),
    Third_Nest = sum(nest_count == 3)
  )

# Convert data to long format for ggplot
multi_nest_summary_long <- tidyr::pivot_longer(summ_multi_nests, 
                                         cols = c(Second_Nest, Third_Nest), 
                                         names_to = "MultiNests", 
                                         values_to = "count")

ggplot(multi_nest_summary_long, aes(x = as.factor(Year), y = count, fill = MultiNests)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Second_Nest" = "cornflowerblue", "Third_Nest" = "lightcoral")) + 
  labs(title = "Number of Birds Nesting Multiple Times per Year",
       x = "Year",
       y = "Total Number of Birds",
       fill = "MultiNests") +
  theme_minimal()

######################################################################################################################################  
##################################### nest initiation likelihood
nest_proportions$Year <- as.numeric(as.character(nest_proportions$Year))
TTRS_April15_BreedingNumbers$Year <- as.numeric(as.character(TTRS_April15_BreedingNumbers$Year))

TTRS_April15_BreedingNumbers$Sex[TTRS_April15_BreedingNumbers$Sex =='F'] <- 'female'
TTRS_April15_BreedingNumbers$Sex[TTRS_April15_BreedingNumbers$Sex =='M'] <- 'male'

Nest_initiate <- merge(nest_proportions, TTRS_April15_BreedingNumbers, 
                       by = c("Year", "Sex"), all.x = TRUE)
Nest_initiate <- Nest_initiate %>% relocate('N.breed', .after = 'Sex')
Nest_initiate <- Nest_initiate[,-6]

Nest_initiate$likelihood_init <- Nest_initiate$count / Nest_initiate$N.breed

nest__init_results <- t.test(Nest_initiate$likelihood_init) #all values 

#female only 
female_initiate<- Nest_initiate %>%
  filter(Sex == "female")

f_results <- t.test(female_initiate$likelihood_init)
f_avg_int <- f_results$estimate
f_ci_init <- f_results$conf.int

#male only
male_initiate<- Nest_initiate %>%
  filter(Sex == "male")

m_results <- t.test(male_initiate$likelihood_init)
m_avg_int <- m_results$estimate
m_ci_init <- m_results$conf.int

ggplot(Nest_initiate, aes(x = Year, y = likelihood_init, color = Sex)) + 
  geom_point() + 
  geom_line(aes(group = Sex)) + 
  labs(title = "Likelihood by Year for Each Sex", x = "Year", y = "Likelihood Initial") + 
  theme_minimal() + 
  
  # Add means and CI as horizontal lines for M and F
  geom_hline(yintercept = m_avg_int, linetype = "dashed", color = "blue", size = 1) + 
  geom_errorbar(
    aes(x = 2014, ymin = m_ci_init[1], ymax = m_ci_init[2]),
    width = 0.2, color = "blue", size = 1
  ) + 
  
  geom_hline(yintercept = f_avg_int, linetype = "dashed", color = "pink", size = 1) + 
  geom_errorbar(
    aes(x = 2014, ymin = f_ci_init[1], ymax = f_ci_init[2]),
    width = 0.2, color = "pink", size = 1
  ) + 
  
  scale_color_manual(values = c("M" = "blue", "F" = "pink"))

###

ggplot(Nest_initiate, aes(x = Year, y = likelihood_init, color = Sex)) + 
  geom_point() + 
  geom_line(aes(group = Sex)) + 
  labs(title = "Likelihood by Year for Each Sex", x = "Year", y = "Likelihood Initial") + 
  theme_minimal() + 
  
  # Add mean line and confidence interval as shaded ribbon for Males
  geom_line(aes(y = m_avg_int), color = "blue", linetype = "dashed", size = 1) +
  geom_ribbon(aes(ymin = m_ci_init[1], ymax = m_ci_init[2]), fill = "grey", alpha = 0.2) +
  
  # Add mean line and confidence interval as shaded ribbon for Females
  geom_line(aes(y = f_avg_int), color = "red", linetype = "dashed", size = 1) +
  geom_ribbon(aes(ymin = f_ci_init[1], ymax = f_ci_init[2]), fill = "grey", alpha = 0.2) +
  
  scale_color_manual(values = c("M" = "blue", "F" = "red"))

######################################################################################################################################  
##################################### individual nest initiate 

#a df that shows each indiviudal per year, with sex, and the number of nests they laid
individual_nests <- Nests_2000_2024 %>%
  group_by(Year, `Bird ID`, Sex) %>%
  summarise(nest_count = n(), .groups = 'drop')

#df breakdown of each year, that shows how many individual laid 1,2,3, or 4 nests 
nests_per_year <- individual_nests %>%
  group_by(Year, nest_count) %>%
  summarise(num_birds = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = nest_count,
    names_prefix = "nests_",
    values_from = num_birds,
    values_fill = 0
  )

#total number of individuals radioed April 15th
total_alive_by_year <- TTRS_April15_BreedingNumbers %>%
  group_by(Year) %>%
  summarise(total_alive = sum(N.breed), .groups = "drop")

#add total number of birds alive 
nests_breakdown <- merge(nests_per_year, total_alive_by_year,
                         by = "Year", all.x = TRUE)
nests_breakdown <- nests_breakdown %>% relocate('total_alive', .after = 'Year')

nest_rates<- nests_breakdown %>%
  mutate(
    nests_0 = total_alive - (nests_1 + nests_2 + nests_3 + nests_4),
    rate_0 = nests_0 / total_alive,
    rate_1 = nests_1 / total_alive,
    rate_2 = nests_2 / total_alive,
    rate_3 = nests_3 / total_alive,
    rate_4 = nests_4 / total_alive
  ) %>%
  select(Year, starts_with("rate_"))

nest_rates_long <- nest_rates %>%
  pivot_longer(
    cols = starts_with("rate_"),
    names_to = "nest_count",
    names_prefix = "rate_",
    values_to = "rate"
  )

ggplot(nest_rates_long, aes(x = factor(Year), y = rate, fill = nest_count)) +
  geom_col(position = "dodge") +
  labs(
    title = "Nest Count Rates by Year",
    x = "Year",
    y = "Proportion of Birds",
    fill = "Number of Nests"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal()

ggplot(nest_rates_long, aes(x = Year, y = rate, color = nest_count)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  labs(
    title = "Nest Rate Trends Over Time",
    x = "Year",
    y = "Proportion of Birds",
    color = "Number of Nests"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal()

#############################################   SAME DATA, JUST BROKEN DOWN BY SEX  #############################################

#df breakdown of each year, by sex, that shows how many indiviudal laid 1,2,3, or 4 nests 
nests_per_yr_per_sex <- individual_nests %>%
  group_by(Year, Sex, nest_count) %>%
  summarise(num_birds = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = nest_count,
    names_prefix = "nests_",
    values_from = num_birds,
    values_fill = 0
  )

#adds the total number of individuals. by sex, alive April 15
nests_breakdown_by_sex <- merge(nests_per_yr_per_sex, TTRS_April15_BreedingNumbers,
                         by = c("Year", "Sex"), all.x = TRUE)
nests_breakdown_by_sex <- nests_breakdown_by_sex %>% relocate('N.breed', .after = 'Sex')
nests_breakdown_by_sex <- nests_breakdown_by_sex[,-8] #remove location as TTRS

#per year, per sex: the proportion of all individuals alive April 15 that lay 0,1,2,3,or4 nests
nest_rates_by_sex <- nests_breakdown_by_sex %>%
  mutate(
    nests_0 = N.breed - (nests_1 + nests_2 + nests_3 + nests_4),
    rate_0 = nests_0 / N.breed,
    rate_1 = nests_1 / N.breed,
    rate_2 = nests_2 / N.breed,
    rate_3 = nests_3 / N.breed,
    rate_4 = nests_4 / N.breed
  ) %>%
  select(Year, Sex, starts_with("rate_"))

nest_rates_long_sex <- nest_rates_by_sex %>%
  pivot_longer(
    cols = starts_with("rate_"),
    names_to = "nest_count",
    names_prefix = "rate_",
    values_to = "rate"
  )

# Filter for females only
nest_rates_female <- nest_rates_long_sex %>%
  filter(Sex == "female")

ggplot(nest_rates_female, aes(x = factor(Year), y = rate, fill = nest_count)) +
  geom_col(position = "dodge") +
  labs(
    title = "Nest Count Rates by Year (Females)",
    x = "Year",
    y = "Proportion of Birds",
    fill = "Number of Nests"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal()

ggplot(nest_rates_female, aes(x = Year, y = rate, color = nest_count, group = nest_count)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  labs(
    title = "Nest Count Rates Over Time (Females)",
    x = "Year",
    y = "Proportion of Birds",
    color = "Number of Nests"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal()

# Filter for males only
nest_rates_male <- nest_rates_long_sex %>%
  filter(Sex == "male")

ggplot(nest_rates_male, aes(x = factor(Year), y = rate, fill = nest_count)) +
  geom_col(position = "dodge") +
  labs(
    title = "Nest Count Rates by Year (Males)",
    x = "Year",
    y = "Proportion of Birds",
    fill = "Number of Nests"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal()

# Plot
ggplot(nest_rates_male, aes(x = Year, y = rate, color = nest_count, group = nest_count)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  labs(
    title = "Nest Count Rates Over Time (Males)",
    x = "Year",
    y = "Proportion of Birds",
    color = "Number of Nests"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal()

############################### NEST INITIATION AVERAGES AND SD
##females 
nest_init_female <- nest_rates_by_sex %>%
  filter(Sex == "female")
nest_init_female <- nest_init_female[,-2]

init_avg_f <- colMeans(nest_init_female[-1])
init_SD_f <- apply(nest_init_female[-1], 2, sd, na.rm = TRUE)
init_f<- rbind(nest_init_female, c("Average", init_avg_f))
init_f<- rbind(init_f, c("SD", init_SD_f))     

init_f <- init_f %>%
  mutate(
    rate_sum = rowSums(across(c(rate_0, rate_1, rate_2, rate_3, rate_4), ~as.numeric(.)), na.rm = TRUE)
  )
#####ggplot data
female_data_long <- nest_init_female %>%
  pivot_longer(cols = starts_with("rate_"), names_to = "nest_count", values_to = "rate")

#Turn named mean and SD vectors into data frames
female_mean_df <- tibble(
  nest_count = names(init_avg_f),
  mean = as.numeric(init_avg_f)
)

female_sd_df <- tibble(
  nest_count = names(init_SD_f),
  sd = as.numeric(init_SD_f)
)

# Join mean and SD info to the main plot data
female_data_long <- female_data_long %>%
  left_join(female_mean_df, by = "nest_count") %>%
  left_join(female_sd_df, by = "nest_count")

ggplot(female_data_long, aes(x = as.numeric(Year), y = rate, color = nest_count, group = nest_count)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  geom_hline(aes(yintercept = mean, color = nest_count), linetype = "dashed") +
  geom_ribbon(
    aes(ymin = mean - sd, ymax = mean + sd, fill = nest_count),
    alpha = 0.2,
    color = NA
  ) +
  labs(
    title = "Female Nest Initiation Rates Over Time",
    x = "Year",
    y = "Proportion of Males",
    color = "Nest Count",
    fill = "Nest Count (SD Range)"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal()

##males 
nest_init_male <- nest_rates_by_sex %>%
  filter(Sex == "male")
nest_init_male <- nest_init_male[,-2]

init_avg_m <- colMeans(nest_init_male[-1])
init_SD_m <- apply(nest_init_male[-1], 2, sd, na.rm = TRUE)
init_m<- rbind(nest_init_male, c("Average", init_avg_m))
init_m<- rbind(init_m, c("SD", init_SD_m)) 

init_m <- init_m %>%
  mutate(
    rate_sum = rowSums(across(c(rate_0, rate_1, rate_2, rate_3, rate_4), ~as.numeric(.)), na.rm = TRUE)
  )
###
male_data_long <- nest_init_male %>%
  pivot_longer(cols = starts_with("rate_"), names_to = "nest_count", values_to = "rate")

#Turn named mean and SD vectors into data frames
male_mean_df <- tibble(
  nest_count = names(init_avg_m),
  mean = as.numeric(init_avg_m)
)

male_sd_df <- tibble(
  nest_count = names(init_SD_m),
  sd = as.numeric(init_SD_m)
)

#Join mean and SD info to the main plot data
male_data_long <- male_data_long %>%
  left_join(male_mean_df, by = "nest_count") %>%
  left_join(male_sd_df, by = "nest_count")

# Plot
ggplot(male_data_long, aes(x = as.numeric(Year), y = rate, color = nest_count, group = nest_count)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  geom_hline(aes(yintercept = mean, color = nest_count), linetype = "dashed") +
  geom_ribbon(
    aes(ymin = mean - sd, ymax = mean + sd, fill = nest_count),
    alpha = 0.2,
    color = NA
  ) +
  labs(
    title = "Male Nest Initiation Rates Over Time",
    x = "Year",
    y = "Proportion of Males",
    color = "Nest Count",
    fill = "Nest Count (SD Range)"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal()

# not sure what this code is 
rownames(init_w_avg)[(nrow(init_w_avg)-1):nrow(init_w_avg)] <- c("Average", "SD")

init_w_avg <- init_w_avg %>%
  mutate(
    rate_sum = rowSums(across(c(rate_0, rate_1, rate_2, rate_3, rate_4), ~as.numeric(.)), na.rm = TRUE)
  )


