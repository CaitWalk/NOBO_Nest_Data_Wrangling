# Code for analyzing cost scenarios when adult sample size is varied
# pulls out summary statistics from wing_scenario_results and
# genetic_scenario_results from the simulation models. 

######## extract the relevant number of birds marked and recaptures/recovered

# pull out summary stats from each scenario_results
wing_scenarios <- wing_scenario_results[1:5]
genetic_scenarios <- genetic_scenario_results[1:5]

extract_scenario_summary <- function(scenarios, method_label) {
  summaries <- lapply(seq_along(scenarios), function(i) {
    ss <- scenarios[[i]]$summary_stats
    means <- apply(ss, 2, mean, na.rm = TRUE)
    means_rounded <- round(means)
    adult_sample_size <- scenarios[[i]]$scenario$Adult_sample_size
    
    data.frame(
      scenario = i,
      Adult_sample_size = adult_sample_size,
      marked = means_rounded[1],
      fall_recaptures = means_rounded[2],
      winter_recaptures = means_rounded[3],
      harvests = means_rounded[4],
      Method = method_label
    )
  })
  
  # Combine list of data.frames into one
  do.call(rbind, summaries)
}

# Get summaries for each method
wing_summary <- extract_scenario_summary(wing_scenarios, "Wing")
genetic_summary <- extract_scenario_summary(genetic_scenarios, "Genotype")

# Combine into one dataset
adult_sample_scenarios_summary <- rbind(wing_summary, genetic_summary)
rownames(adult_sample_scenarios_summary) <- NULL

adult_sample_scenarios_summary

########################### Cost for Wing tagging
# this is code associated with calculating the code to conduct brood captures as a method
# of marking chicks

# number of brood captures needed to capture the number marked per scenario
adults <- adult_sample_scenarios_summary$Adult_sample_size[
  adult_sample_scenarios_summary$Method == "Wing"]

brood_chicks <- adult_sample_scenarios_summary$marked[
  adult_sample_scenarios_summary$Method == "Wing"]

# define the calculation function
calculate_broods <- function(n_chicks) { #number plus 2 extra to account for potential failures
  ceiling(n_chicks * 13 / 100) + 2
}

# apply to the vector
required_brood_caps <- calculate_broods(brood_chicks)


# Define the labor cost per brood capture
brood_cap_labor <- (22 * 4) * required_brood_caps + #biologist $22 per hour for 4 hours
                  (15 * 4) * required_brood_caps +  #technician $15 per hour for 2 hours
                 (7.14 * 4) * required_brood_caps   #intern $7.14 per hour for 4 hours

# Define the cost of supplies per brood capture
brood_cap_supplies <- adults * 210 +            # cost of adult radio 
                      164 +                     # cost panel construction
                      (0.134 * brood_chicks) +  # cost of bands
                      300                       # cost of mis supplies 

brood_cap_cost <- brood_cap_labor + brood_cap_supplies
brood_cap_cost_per_chick <- brood_cap_cost / brood_chicks

brood_cap_cost_df <- data.frame(
  Scenario = c(1,2,3,4,5),
  Adult_Sample_Size = c(60, 90, 120, 150, 180), 
  Brood_Cap_Cost = brood_cap_cost
)
########################### Cost for genotyping
# fill in with real values
# this is code associated with calculating the code to conduct brood captures as a method
# of marking chicks

# extract values 
adults <- adult_sample_scenarios_summary$Adult_sample_size[
  adult_sample_scenarios_summary$Method == "Genotype"]

genotyped_chicks <- adult_sample_scenarios_summary$marked[
  adult_sample_scenarios_summary$Method == "Genotype"]

fall_feathers <- adult_sample_scenarios_summary$fall_recaptures[
  adult_sample_scenarios_summary$Method == "Genotype"] + 100

winter_feathers <- adult_sample_scenarios_summary$winter_recaptures[
  adult_sample_scenarios_summary$Method == "Genotype"] + 100

harvest_feathers <- adult_sample_scenarios_summary$harvests[
  adult_sample_scenarios_summary$Method == "Genotype"]

feathers <- fall_feathers + winter_feathers + harvest_feathers

plates <- (genotyped_chicks + feathers) / 96  # number of plates needed, assuming 96 samples per plate)


# Define the labor cost for genotyping
genotype_labor <- (22 * plates) +  #geneticist $22 per hour for 1 hr per plate
                  (18 * (4* plates)) + #lab manager $18 hr to set and run plates
                  (15 * (8 * (genotyped_chicks/96))) +  #technician $15 per hour for eggshells (approx 8hrs to do 96)
                  (15 * (4 * (feathers/96))) +  #technician $15 per hour for feathers (approx 4hrs to do 96)
                  (15 * (4 * plates)) #technician $15 per hour for 4 hour to extract dna for 1 plate
# Define the cost of supplies for genotyping
genotype_supplies <-  adults * 210 +  # cost of adult radio
                      25 +            # cost field supplies
                      300 +           # cost of lab supplies
                      500 +           # cost reagents
                      750 +           # cost of primers
                      150 * plates    # cost of genotyping per plate

genotype_cost <- genotype_labor + genotype_supplies

# Calculate cost per sample
samples <- genotyped_chicks + feathers
genotype_cost_per_chick <- genotype_cost / samples 

genotyping_cost_df <- data.frame(
  Scenario = c(1,2,3,4,5),
  Adult_Sample_Size = c(60, 90, 120, 150, 180), 
  Genotype_Cost = genotype_cost
)

# Combine costs into a data frame
total_cost_df <- bind_cols(
  brood_cap_cost_df,
  genotyping_cost_df %>% select(Genotype_Cost)
) %>%
  mutate(
    Brood_Cap_Cost = round(Brood_Cap_Cost, 2),
    Genotype_Cost = round(Genotype_Cost, 2)
  ) %>%
  select(
    Scenario,
    Adult_Sample_Size,
    Brood_Cap_Cost,
    Genotype_Cost
  )

# Convert to long format for plotting
total_cost_long <- total_cost_df %>%
  pivot_longer(
    cols = c(Brood_Cap_Cost, Genotype_Cost),
    names_to = "Method",
    values_to = "Cost"
  ) %>%
  mutate(Method = recode(Method,
                        Brood_Cap_Cost = "Wing Tagging",
                        Genotype_Cost = "Genetic Mark-Recapture"))
#per chick df
# per_chick_cost_summary <- bind_cols(
#   brood_cap_cost_df,
#   genotyping_cost_df %>% select(Genotype_Cost_Per_Chick)
# ) %>%
#   mutate(
#     Brood_Cap_Cost_per_Chick = round(Brood_Cap_Cost_per_Chick, 2),
#     Genotype_Cost_Per_Chick = round(Genotype_Cost_Per_Chick, 2)
#   ) %>%
#   select(
#     Scenario,
#     Adult_Sample_Size,
#     Brood_Cap_Cost_per_Chick,
#     Genotype_Cost_Per_Chick
#   ) 
# 
# per_chick_cost_summary_long <- per_chick_cost_summary %>%
#   pivot_longer(
#     cols = c(Brood_Cap_Cost_per_Chick, Genotype_Cost_Per_Chick),
#     names_to = "Method",
#     values_to = "Cost"
#   ) %>%
#   mutate(
#     Method = recode(Method,
#                     "Brood_Cap_Cost_per_Chick" = "Wing Tagging",
#                     "Genotype_Cost_Per_Chick" = "Genetic Mark-Recapture")
#   )
  
################################################################################# 
########################### compare across methods 

#pull chick survival RMSE from scenario summary
RMSE_df <- combined_scenario_summary %>%
     filter(Scenario <= 5) %>%  # only include the first 5 scenarios
     filter(Parameter == "chick_survival_to_fall") %>%
     select(Scenario, RMSE, Method)
  

#pull chick survival bias from scenario summary
RMSE_summary <- RMSE_df %>%
  group_by(Scenario, Method) %>%
  summarise(
    Mean_RMSE = mean(RMSE),
    SD_RMSE = sd(RMSE),
    n = n(),
    SE_RMSE = SD_RMSE / sqrt(n),
    t_crit = qt(0.975, df = n - 1),
    CI_width = t_crit * SE_RMSE,
    .groups = "drop"
  ) %>%
  mutate(
    Lower_CI = Mean_RMSE - CI_width,
    Upper_CI = Mean_RMSE + CI_width
  )

# Expand cost data to both methods
methods <- unique(RMSE_summary$Method)

cost_long <- per_chick_cost_summary %>%
  tidyr::crossing(Method = methods)

# Join mean bias
total_cost_with_RMSE <- total_cost_long %>%
  left_join(RMSE_summary, by = c("Scenario", "Method"))

print(total_cost_with_RMSE)

#bubble plot for cost vs RMSE
ggplot(total_cost_with_RMSE, aes(x = Mean_RMSE, y = Cost, size = CI_width, color = Method)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(name = "CI Width (RMSE)") +
  labs(
    title = "Cost vs Mean RMSE (Bubble size = CI Width)",
    x = "Mean RMSE",
    y = "Cost",
    color = "Method"
  ) +
  theme_minimal()


