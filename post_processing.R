
load("WingTag_Sim_Results_07172025.RData")

library(dplyr)
library(tidyr)
library(coda)
library(ggplot2)
library(viridis)

# Function to compute scenario metrics for a given list of scenarios
compute_scenario_metrics <- function(scenario_list, method_label) {
  metrics <- lapply(seq_along(scenario_list), function(i) {
    scenario_i <- scenario_list[[i]]
    truth <- scenario_i$scenario
    posterior_samples <- scenario_i$posterior_samples
    
    # extract scenario-specific truths
    chick_survival_to_fall <- truth$Chick_survival
    recap_probs <- as.numeric(strsplit(truth$Recap_probs, ",")[[1]])
    harvest_prob <- truth$Harvest_rate
    
    # constants
    summer_dsr_true <- 0.9951
    winter_dsr_true <- 0.9966302
    
    # derived truths
    chick_month_survival_true <- chick_survival_to_fall^(1/5)  # 5 months
    
    true_vals <- c(
      chick_month_survival = chick_month_survival_true,
      chick_survival_to_fall = chick_survival_to_fall,
      fall_cap_prob = recap_probs[1],
      winter_cap_prob = recap_probs[2],
      harvest_prob = harvest_prob,
      summer_dsr = summer_dsr_true,
      winter_dsr = winter_dsr_true
    )
    
    param_names <- names(true_vals)
    
    # Initialize a list to hold iteration-level results for this scenario
    iter_metrics_list <- vector("list", length(posterior_samples))
    
    # Loop over iterations
    for (iter_idx in seq_along(posterior_samples)) {
      iter_chains <- posterior_samples[[iter_idx]]
      
      # Combine all chains for this iteration
      combined_samples <- list()
      for (chain in iter_chains) {
        chain_mat <- as.matrix(chain)
        colnames(chain_mat) <- c(
          "chick_month_survival",
          "fall_cap_prob",
          "harvest_prob",
          "summer_dsr",
          "winter_cap_prob",
          "winter_dsr"
        )
        
        for (p in colnames(chain_mat)) {
          combined_samples[[p]] <- c(combined_samples[[p]], chain_mat[, p])
        }
      }
      
      # Compute derived
      combined_samples[["chick_survival_to_fall"]] <- combined_samples[["chick_month_survival"]]^5
      
      metrics_list <- lapply(param_names, function(p) {
        samples_p <- combined_samples[[p]]
        true_p <- true_vals[p]
        
        mean_est <- mean(samples_p)
        sd_est <- sd(samples_p)
        cv <- (sd_est / mean_est) * 100
        
        bias <- mean_est - true_p
        ci <- quantile(samples_p, c(0.025, 0.975))
        ci_width <- ci[2] - ci[1]
        coverage <- (true_p >= ci[1]) & (true_p <= ci[2])
        rmse <- sqrt(mean((samples_p - true_p)^2))
        
        tibble(
          Scenario = i,
          Parameter = p,
          True = true_p,
          Estimate = mean_est,
          SD = sd_est,
          CV = cv,
          Bias = bias,
          CI_width = ci_width,
          Coverage = coverage,
          RMSE = rmse
        )
      })
      
      iter_metrics_list[[iter_idx]] <- bind_rows(metrics_list)
    }
    
    bind_rows(iter_metrics_list)
  })
  
  # Combine all scenarios
  summary_df <- bind_rows(metrics)
  
  # Add desired order
  desired_order <- c(
    "chick_survival_to_fall",
    "chick_month_survival",
    "fall_cap_prob",
    "winter_cap_prob",
    "harvest_prob",
    "summer_dsr",
    "winter_dsr"
  )
  
  summary_df$Parameter <- factor(summary_df$Parameter, levels = desired_order)
  
  # Add scenario group
  summary_df <- summary_df %>%
    mutate(
      Scenario_Group = case_when(
        Scenario %in% 1:5   ~ "Adult Sample Size",
        Scenario %in% 6:10  ~ "Nest Success",
        Scenario %in% 11:15 ~ "Recap Probs",
        Scenario %in% 16:20 ~ "Nest Propensity",
        Scenario %in% 21:25 ~ "Chick Survival",
        Scenario %in% 26:30 ~ "Harvest Rate",
        TRUE ~ "Unknown"
      ),
      Method = method_label
    )
  
  summary_df
}

wing_scenario_summary <- compute_scenario_metrics(wing_scenario_results, "Wing Tagging")
geno_scenario_summary <- compute_scenario_metrics(genetic_scenario_results, "Genetic Mark-Recapture")

combined_scenario_summary <- bind_rows(wing_scenario_summary, geno_scenario_summary)

print(combined_scenario_summary)




# joint boxplot for bias by scenario, parameter, and method
ggplot(combined_summary %>% filter(!Parameter %in% c("summer_dsr", "winter_dsr")),
  aes(x = factor(Scenario), y = Bias, fill = Method)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  facet_wrap(~ Parameter, scales = "free_y") +
  labs(
    title = "Bias Comparison by Scenario, Parameter, and Method",
    x = "Scenario",
    y = "Bias",
    fill = "Method"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

################################################################################
############################          Plots           ##########################
################################################################################

############################################# BIAS
# wing tagging
ggplot(wing_scenario_summary, aes(x = factor(Scenario), y = Bias, fill = Scenario_Group)) +
  geom_boxplot() +
  facet_wrap(~ Parameter, scales = "free_y") +
  labs(title = "Wing Tagging Bias by Scenario and Parameter",
       x = "Scenario",
       y = "Bias",
       fill = "Scenario Group") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#genotyping
ggplot(geno_scenario_summary, aes(x = factor(Scenario), y = Bias, fill = Scenario_Group)) +
  geom_boxplot() +
  facet_wrap(~ Parameter, scales = "free_y") +
  labs(title = "Genotyping Bias by Scenario and Parameter",
       x = "Scenario",
       y = "Bias",
       fill = "Scenario Group") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#combined boxplot for bias by method and parameter
ggplot(combined_summary %>% filter(!Parameter %in% c("summer_dsr", "winter_dsr")), 
       aes(x = factor(Scenario), y = Bias, fill = Method)) +
  geom_boxplot() +
  facet_wrap(~ Parameter, scales = "free_y") +
  labs(title = "Bias by Scenario and Method",
       x = "Scenario",
       y = "Bias",
       fill = "Scenario Group") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

################################################################################
############################################# RMSE
#wing tagging
ggplot(wing_scenario_summary, aes(x = factor(Scenario), y = RMSE, fill = Scenario_Group)) +
  geom_boxplot() +
  facet_wrap(~ Parameter, scales = "free_y") +
  labs(title = "RMSE Wing Tagging by Scenario and Parameter",
       x = "Scenario",
       y = "RMSE") +
  theme_bw()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

#genotyping
ggplot(geno_scenario_summary, aes(x = factor(Scenario), y = RMSE, fill = Scenario_Group)) +
  geom_boxplot() +
  facet_wrap(~ Parameter, scales = "free_y") +
  labs(title = "RMSE Genotyping by Scenario and Parameter",
       x = "Scenario",
       y = "RMSE") +
  theme_bw()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

#combined boxplot RSME by scenario and method
ggplot(combined_summary %>% filter(!Parameter %in% c("summer_dsr", "winter_dsr")),
       aes(x = factor(Scenario), y = RMSE, fill = Method)) +
  geom_boxplot() +
  facet_wrap(~ Parameter, scales = "free_y") +
  labs(title = "RMSE by Scenario and Method",
       x = "Scenario",
       y = "RMSE") +
  theme_bw()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

#boxplot CI width by scenario and parameter
# ggplot(combined_summary %>% filter(!Parameter %in% c("summer_dsr", "winter_dsr")),
#        aes(x = factor(Scenario), y = CI_width, fill = Method)) +
#   geom_boxplot() +
#   facet_wrap(~ Parameter, scales = "free_y") +
#   labs(title = "CI Width by Scenario and Parameter",
#        x = "Scenario",
#        y = "CI Width") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))

################################################################################
############################################# RMSE
#wing tagging coverage proportion 
wing_coverage_summary <- wing_scenario_summary %>%
  group_by(Scenario, Parameter, Scenario_Group) %>%
  summarise(Coverage_Prop = mean(Coverage), .groups = "drop")

#Bar plot proportion by scenario and parameter
ggplot(wing_coverage_summary, aes(x = factor(Scenario), y = Coverage_Prop, fill = Scenario_Group)) +
  geom_col() +
  facet_wrap(~ Parameter, scales = "free_y") +
  labs(title = "Coverage Proportion by Scenario and Parameter",
       x = "Scenario",
       y = "Coverage Proportion",
       fill = "Scenario Group") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#heat map of coverage by scenario and parameter
ggplot(wing_coverage_summary, aes(x = factor(Scenario), y = Parameter, fill = Coverage_Prop)) +
  geom_tile(color = "white") +
  facet_wrap(~ Scenario_Group, scales = "free_x", nrow = 1) +
  scale_fill_viridis(option = "G") +
  labs(title = "Coverage Proportion Heatmap by Scenario Group",
       x = "Scenario",
       y = "Parameter",
       fill = "Coverage Proportion") +
  theme_bw() 



# genotyping coverage
geno_coverage_summary <- geno_scenario_summary %>%
  group_by(Scenario, Parameter, Scenario_Group) %>%
  summarise(Coverage_Prop = mean(Coverage), .groups = "drop")

#Bar plot proportion by scenario and parameter
ggplot(geno_coverage_summary, aes(x = factor(Scenario), y = Coverage_Prop, fill = Scenario_Group)) +
  geom_col() +
  facet_wrap(~ Parameter, scales = "free_y") +
  labs(title = "Coverage Proportion by Scenario and Parameter",
       x = "Scenario",
       y = "Coverage Proportion",
       fill = "Scenario Group") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#heat map of coverage by scenario and parameter
ggplot(geno_coverage_summary, aes(x = factor(Scenario), y = Parameter, fill = Coverage_Prop)) +
  geom_tile(color = "white") +
  facet_wrap(~ Scenario_Group, scales = "free_x", nrow = 1) +
  scale_fill_viridis(option = "G") +
  labs(title = "Coverage Proportion Heatmap by Scenario Group",
       x = "Scenario",
       y = "Parameter",
       fill = "Coverage Proportion") +
  theme_bw() 


# coverage by sceanrio and method
combined_coverage_summary <- combined_summary %>%
  group_by(Scenario, Parameter, Scenario_Group, Method) %>%
  summarise(Coverage_Prop = mean(Coverage), .groups = "drop")

ggplot(combined_coverage_summary, aes(x = factor(Scenario), y = Coverage_Prop, fill = Method)) +
  geom_col() +
  facet_wrap(~ Parameter, scales = "free_y") +
  labs(title = "Coverage Proportion by Scenario and Parameter",
       x = "Scenario",
       y = "Coverage Proportion",
       fill = "Scenario Group") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#jitter plot of coverage by scenario and parameter
ggplot(combined_coverage_summary, aes(x = factor(Scenario), y = Coverage_Prop, color = Method)) +
  geom_jitter(width = 0.2, height = 0, alpha = 1, ) +
  facet_wrap(~ Parameter) +
  labs(title = "Jitter plot of Coverage by Scenario and Parameter",
       x = "Scenario",
       y = "Coverage (0 = no, 1 = yes)") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

################################################################################
############################################# CV
# #plot of CV
# ggplot(combined_summary %>% filter(!Parameter %in% c("summer_dsr", "winter_dsr")),
#        aes(x = factor(Scenario), y = CV, fill = Method)) +
#   geom_col(alpha = 0.8) +
#   facet_wrap(~ Parameter, scales = "free_y") +
#   labs(
#     x = "Scenario",
#     y = "CV",
#     fill = "Scenario Group", 
#     title = "Coefficient of Variation (CV) per Scenario and Parameter"
#   ) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Filter to just chick_survival_to_fall
chick_cv <- combined_summary %>%
  filter(Parameter == "chick_survival_to_fall")

ggplot(chick_cv, aes(x = factor(Scenario), y = CV, fill = Method)) +
  geom_boxplot(alpha = 0.8) +
  labs(
    x = "Scenario",
    y = "CV",
    fill = "Scenario Group",
    title = "Coefficient of Variation (CV) for Chick Survival to Fall"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
    legend.position = "right"
  )

