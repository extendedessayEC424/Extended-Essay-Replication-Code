# TASK: Extended Essay

# ==============================================================================
# Clean the environment

rm(list = ls())
setwd()

# ==============================================================================
# Import libraries
#install.packages("gridExtra")
#install.packages("ggpubr")
#install.packages("lpirfs")
# install.packages("vars")
# install.packages("extrafont")
library(readr)
library(tidyverse)
library(purrr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(lpirfs)
library(vars)
library(extrafont)
# # ============================================================================
# # Import fonts from the system
font_import()
# 
# # Select and set fonts for use in plots
loadfonts()

# Importing the Data
ag_data <- read_csv("FINAL_FINAL_data.csv")
ag_data <- ag_data %>%
  mutate(ln_cpi = log(CPI_raw))%>%
  mutate(ln_cpi_food = log(food))%>%
  mutate(ln_cpi_hous = log(housing))%>%
  mutate(ln_cpi_app = log(apparel))%>%
  mutate(ln_cpi_tran = log(transport))%>%
  mutate(ln_cpi_med = log(medical))%>%
  mutate(ln_cpi_rec = log(recreation)) %>%
  mutate(ln_cpi_educ = log(educ_comm)) %>%
  mutate(ln_core_cpi = log(core_cpi))


sample_start <- 1
sample_end <- dim(ag_data)[1]

ag_data <- ag_data %>%
  mutate(year_m = as.Date(year_m, format = "%d/%m/%Y"))

#===============================================================================
# Model 1: Two States
#===============================================================================

# Endogenous data
endog_data <- ag_data[sample_start:sample_end, 28]

# Exogenous data
exog_data <- ag_data[sample_start:sample_end, 6]
exog_data <- exog_data %>%
  mutate(ebp_new = ag_data$ebpnew)

# Shock variable
shock <- ag_data[sample_start:sample_end, 18]

# Nonlinear shock 
shock <- ag_data[sample_start:sample_end, 18]

# Define font settings
font_settings <- theme(text = element_text(family = "Times New Roman", 
                                           size = 13))

# Define line settings
line_settings <- theme(
  axis.line = element_line(size = 1),  # Set thickness of axis lines
)

# Define y-axis limits
y_limits <- c(-0.05, 0.05)

# Define line width
line_width <- 2 

switching_data <- if_else((ag_data$cpi_inf_annualized) > 2.2673, 1, 0)
switch_fr <- data.frame(switching_data)

# Estimate linear model
results_lin_iv <- lp_lin_iv(endog_data = endog_data, lags_endog_lin = 12,
                            #lags_criterion = 'AIC',max_lags = 12,
                            exog_data = exog_data, lags_exog = 1,
                            shock = shock, trend = 1,use_nw = TRUE,
                            confint = 1.65, hor = 24)

# Make and save linear plots
iv_lin_plots <- plot_lin(results_lin_iv)

# Customize linear plots
iv_lin_plots <- lapply(iv_lin_plots, function(plot) {
  plot + 
    labs(title = "Impulse response of ln CPI to monetary policy shock (Linear)") +
    theme(axis.text = element_text(size = 11, face = "bold"),  
          axis.title = element_text(size = 11, face = "bold"),  
          plot.title = element_text(size = 13, face = "bold")) + 
    font_settings +  # Apply Times New Roman font settings
    line_settings +  # Apply line settings
    #coord_cartesian(ylim = y_limits) +  # Set y-axis limits
    geom_line(linewidth = 3)
})
# Nonlinear shock 
switching_data <- if_else((ag_data$cpi_inf_annualized) > 2.2673, 1, 0)

# Estimate nonlinear model
results_nl_iv <- lp_nl_iv(endog_data = endog_data,lags_endog_nl = 12,
                          #lags_criterion = 'AIC',max_lags = 12,
                          exog_data = exog_data, lags_exog = 1,
                          shock = shock, trend = 1,
                          confint = 1.65, hor = 24,use_nw = TRUE,
                          switching = switching_data, lag_switching = TRUE,
                          use_hp = FALSE, use_logistic = FALSE)

# Make and save nonlinear plots
plots_nl_iv_high <- plot_nl(results_nl_iv)$gg_s2
plots_nl_iv_low <- plot_nl(results_nl_iv)$gg_s1

# Customize nonlinear plots
plots_nl_iv_high <- lapply(plots_nl_iv_high, function(plot) {
  plot + 
    labs(title = "Impulse response of ln CPI to monetary policy shock (High Inflation State)") + 
    theme(axis.text = element_text(size = 11, face = "bold"), 
          axis.title = element_text(size = 11, face = "bold"),  
          plot.title = element_text(size = 13, face = "bold")) +
    font_settings +  
    line_settings +  
    coord_cartesian(ylim = y_limits) +
    geom_line(linewidth = 3)
})


plots_nl_iv_low <- lapply(plots_nl_iv_low, function(plot) {
  plot + 
    labs(title = "Impulse response of ln CPI to monetary policy shock (Low Inflation State)") +
    theme(axis.text = element_text(size = 11, face = "bold"), 
          axis.title = element_text(size = 11, face = "bold"),  
          plot.title = element_text(size = 13, face = "bold")) +
    font_settings +  # Apply Times New Roman font settings
    line_settings +  # Apply line settings
    coord_cartesian(ylim = y_limits) +  # Set y-axis limits
    geom_line(linewidth = 3)
})

# Combine linear and nonlinear plots
combine_plots <- c(iv_lin_plots, plots_nl_iv_high, plots_nl_iv_low)

# Show all plots
marrangeGrob(combine_plots, nrow = 3, ncol = 1, top = NULL)

# ==============================================================================
# Model 2: Four Regimes
#===============================================================================

# Endogenous data
endog_data <- ag_data[sample_start:sample_end, 28]

# Exogenous data
exog_data <- ag_data[sample_start:sample_end, 6]
exog_data <- exog_data %>%
  mutate(ebp_new = ag_data$ebpnew)

# Nonlinear shock 
shock <- ag_data[sample_start:sample_end, 18]

# Define switching variables for each regime
switching_data <- list(
  cpi_inf_annualized_1 = if_else(ag_data$cpi_inf_annualized < 1.051, 1, 0),
  cpi_inf_annualized_2 = if_else(ag_data$cpi_inf_annualized > 1.051 &
                                   ag_data$cpi_inf_annualized < 2.2673, 1, 0),
  cpi_inf_annualized_3 = if_else(ag_data$cpi_inf_annualized > 2.2673 &
                                   ag_data$cpi_inf_annualized < 3.5381, 1, 0),
  cpi_inf_annualized_4 = if_else(ag_data$cpi_inf_annualized > 3.5381, 1, 0)
)

# List to store results and plots
results_list <- list()
plots_list <- list()

# Define font settings
font_settings <- theme(text = element_text(family = "Times New Roman", 
                                           size = 13))

# Define line settings
line_settings <- theme(
  axis.line = element_line(size = 1), 
)

# Define y-axis limits
y_limits <- c(-5, 5)

# Define line width
line_width <- 2 

# Iterating over regimes
for (i in 1:length(switching_data)) {
  # Estimate nonlinear model
  results_nl_iv <- lp_nl_iv(endog_data = endog_data, lags_endog_nl = 12,
                            #lags_criterion = 'AIC', max_lags = 12,
                            exog_data = exog_data, lags_exog = 1,
                            shock = shock, trend = 1,
                            confint = 1.65, hor = 24, use_nw = TRUE,
                            switching = switching_data[[i]], lag_switching = TRUE,
                            use_hp = FALSE, use_logistic = FALSE)
  results_list[[i]] <- results_nl_iv
  
  # Make and save nonlinear plots for each regime
  plots_nl_iv <- plot_nl(results_nl_iv)
  plots_list[[i]] <- plots_nl_iv$gg_s2[[1]] +
    labs(title = paste("Impulse response of ln CPI to monetary policy shock - Regime", i)) + 
    theme(axis.text = element_text(size = 11, face = "bold"),  
          axis.title = element_text(size = 11, face = "bold"), 
          plot.title = element_text(size = 13, face = "bold")) +
    font_settings +  # Apply Times New Roman font settings
    line_settings +  # Apply line settings
    #coord_cartesian(ylim = y_limits) +  # Set y-axis limits
    geom_line(linewidth = 3)
}

# Combine plots into a grid
lin_plots_all <- marrangeGrob(grobs = plots_list, nrow = length(switching_data), 
                              ncol = 1, top = NULL)

# Show the combined plots
lin_plots_all
summary(results_nl_iv)

#===============================================================================
# Sub-groups of CPI (Grouping according to Regimes)
#===============================================================================

endog_vars <- c("ln_cpi", "ln_cpi_food", "ln_cpi_hous", "ln_cpi_app", 
                "ln_cpi_tran", "ln_cpi_med", "ln_cpi_rec", "ln_cpi_educ")


var_names <- c("CPI All", "CPI Food", "CPI Housing", "CPI Apparel", 
               "CPI Transportation", "CPI Medical", "CPI Recreation", 
               "CPI Education")

# Define switching variables for each regime
switching_data <- list(
  cpi_inf_annualized_1 = if_else(ag_data$cpi_inf_annualized < 1.051, 1, 0),
  cpi_inf_annualized_2 = if_else(ag_data$cpi_inf_annualized > 1.051 &
                                   ag_data$cpi_inf_annualized < 2.2673, 1, 0),
  cpi_inf_annualized_3 = if_else(ag_data$cpi_inf_annualized > 2.2673 &
                                   ag_data$cpi_inf_annualized < 3.5381, 1, 0),
  cpi_inf_annualized_4 = if_else(ag_data$cpi_inf_annualized > 3.5381, 1, 0)
)

# List to store results and plots for each regime
results_list_all <- list()
plots_list_all <- list()

# Loop over regimes
for (i in 1:length(switching_data)) {
  results_list <- list()
  plots_list <- list()
  
  # Iterating over endogenous variables
  for (j in 1:length(endog_vars)) {
    endog_data <- ag_data[sample_start:sample_end, endog_vars[j]]
    
    # Estimate nonlinear model
    results_nl_iv <- lp_nl_iv(endog_data = endog_data, lags_endog_nl = 12,
                              exog_data = exog_data, lags_exog = 1,
                              shock = shock, trend = 1,
                              confint = 1.65, hor = 24, use_nw = TRUE,
                              switching = switching_data[[i]], 
                              lag_switching = TRUE,
                              use_hp = FALSE, use_logistic = FALSE)
    results_list[[j]] <- results_nl_iv
    
    # Make and save nonlinear plots for each variable
    plots_nl_iv <- plot_nl(results_nl_iv)
    plots_list[[j]] <- plots_nl_iv$gg_s2[[1]] +
      labs(title = paste(var_names[j], i)) + 
      theme(axis.text = element_text(size = 11, face = "bold"),  
            axis.title = element_text(size = 8, face = "bold"), 
            plot.title = element_text(size = 13, face = "bold")) +
      font_settings +  
      line_settings + 
      geom_line(linewidth = 3)
  }
  
  # Store results and plots for each regime
  results_list_all[[i]] <- results_list
  plots_list_all[[i]] <- plots_list
}

# Loop over regimes
for (i in 1:length(switching_data)) {
  plots_list <- list()
  
  # Iterating over endogenous variables
  for (j in 1:length(endog_vars)) {
    plots_list[[j]] <- plots_list_all[[i]][[j]]
  }
  
  # Combine plots for all variables in the current regime into a grid
  lin_plots_all <- marrangeGrob(grobs = plots_list, nrow = 3, ncol = 3, top = NULL)
  
  # Print the combined plots
  print(lin_plots_all)
}


#===============================================================================
# Sub-groups of CPI (Grouping according to Variables)
#===============================================================================
# Initialize the lists
results_list <- list()
plots_list <- list()

# Iterating over endogenous variables
for (j in 1:length(endog_vars)) {
  # Endogenous data for current variable
  endog_data <- ag_data[sample_start:sample_end, endog_vars[j]]
  
  # Create a list to store the plots for each variable
  var_plots <- list()
  
  # Iterating over regimes
  for (i in 1:length(switching_data)) {
    # Estimate nonlinear model
    results_nl_iv <- lp_nl_iv(endog_data = endog_data, lags_endog_nl = 12,
                              exog_data = exog_data, lags_exog = 2,
                              shock = shock, trend = 1,
                              confint = 1.65, hor = 24, use_nw = TRUE,
                              switching = switching_data[[i]], 
                              lag_switching = TRUE,
                              use_hp = FALSE, use_logistic = FALSE)
    results_list[[paste("var", j, "regime", i)]] <- results_nl_iv
    
    # Make and save nonlinear plots for each regime
    plots_nl_iv <- plot_nl(results_nl_iv)
    var_plots[[i]] <- plots_nl_iv$gg_s2[[1]] +
      labs(title = paste("Impulse response of", var_names[j], "to monetary policy shock - Regime", i)) + 
      theme(axis.text = element_text(size = 10, face = "bold"),
            axis.title = element_text(size = 11, face = "bold"),
            plot.title = element_text(size = 13, face = "bold")) +
      font_settings +
      line_settings +
      geom_line(linewidth = 3) 
  }
  
  # Combine the plots for each variable into a single plot
  plots_list[[j]] <- gridExtra::grid.arrange(grobs = var_plots, ncol = 1)
}

# Combine plots for all variables in the current regime into a grid
lin_plots_all <- marrangeGrob(grobs = plots_list, nrow = 1, ncol = 1, top = NULL)

# Print the combined plots
print(lin_plots_all)


#===============================================================================
# Summary statistics
#===============================================================================

# Variables to summarize
variables <- c("MP_shock")

# Conditions
conditions <- list(
  condition1 = ag_data$cpi_inf_annualized < 1.051,
  condition2 = ag_data$cpi_inf_annualized > 1.051 & 
    ag_data$cpi_inf_annualized < 2.2673,
  condition3 = ag_data$cpi_inf_annualized > 2.2673 & 
    ag_data$cpi_inf_annualized < 3.5381,
  condition4 = ag_data$cpi_inf_annualized > 3.5381
)

# Summary dataframe to store results
summary_df <- data.frame(variable = character(), 
                         condition = character(),
                         mean = double(), 
                         median = double(), 
                         sd = double(), 
                         min = double(), 
                         max = double(),
                         stringsAsFactors = FALSE)

# Loop over variables
for (variable in variables) {
  # Loop over conditions
  for (condition_name in names(conditions)) {
    condition <- conditions[[condition_name]]
    
    # Summarize variable based on condition
    summary <- ag_data %>%
      filter(condition) %>%
      summarize(
        mean = mean(get(variable)),
        median = median(get(variable)),
        sd = sd(get(variable)),
        min = min(get(variable)),
        max = max(get(variable))
      ) 
    
    # Add results to summary dataframe
    summary_df <- bind_rows(summary_df, 
                            data.frame(variable = variable, 
                                       condition = condition_name,
                                       mean = summary$mean,
                                       median = summary$median,
                                       sd = summary$sd,
                                       min = summary$min,
                                       max = summary$max))
  }
}

# View summary dataframe
summary_df

#===============================================================================
# Distribution of shocks
#===============================================================================

library(dplyr)

# Define conditions
conditions <- list(
  Regime_1 = ag_data$cpi_inf_annualized < 1.051,
  Regime_2 = ag_data$cpi_inf_annualized > 1.051 & 
    ag_data$cpi_inf_annualized < 2.2673,
  Regime_3 = ag_data$cpi_inf_annualized > 2.2673 & 
    ag_data$cpi_inf_annualized < 3.5381,
  Regime_4 = ag_data$cpi_inf_annualized > 3.5381
)

# Calculate deciles of ff4_hf variable
ff4_hf_deciles <- quantile(ag_data$MP_shock, probs = seq(0, 1, by = 0.1), na.rm = TRUE)

# Create dataframe to store results
result_df <- data.frame(
  decile = character(),
  condition = character(),
  count = integer(),
  stringsAsFactors = FALSE
)

# Loop over deciles
for (i in 1:(length(ff4_hf_deciles) - 1)) {
  lower_bound <- ff4_hf_deciles[i]
  upper_bound <- ff4_hf_deciles[i + 1]
  
  # Loop over conditions
  for (condition_name in names(conditions)) {
    condition <- conditions[[condition_name]]
    
    # Filter data for the current decile and condition
    count <- ag_data %>%
      filter(MP_shock >= lower_bound & MP_shock < upper_bound & !!condition) %>%
      nrow()  # Count number of rows
    
    # Add results to dataframe
    result_df <- rbind(result_df, data.frame(
      decile = paste0("Decile ", i),
      condition = condition_name,
      count = count
    ))
  }
}

# View result dataframe
result_df
# Convert decile to factor for correct ordering in the plot
result_df$decile <- factor(result_df$decile, levels = unique(result_df$decile))

# Create the bar plot
ggplot(result_df, aes(x = decile, y = count, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Decile of Monetary policy shocks",
       y = "Number of Observations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = "Times New Roman")) +  
  scale_fill_brewer(palette = "Spectral") 

