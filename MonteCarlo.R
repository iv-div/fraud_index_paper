# This is a Monte-Carlo experiment to demonstrate that ballot box stuffing 
# creates a high positive correlation of turnout with the incumbent's percent


library(dplyr)
library(ggplot2)

# We set a number of precincts and number of repetitions
n_precincts <- 2000
reps <- 1000
set.seed(123)

results <- data.frame()

# We make 1000 simulations of elections in two regions: Region 1 with fraud
# and Region 2 without fraud

for (r in 1:reps) {
  # We create precincts, distribute them between two regions 
  # and populate them with a random number of registered voters between 50 and 3000
  df <- data.frame(precinct = 1:n_precincts)
  df$region <- ifelse(df$precinct <= n_precincts / 2, 1, 2)
  df$actual_voters_registered <- round(runif(n_precincts, 50, 3000))
  
  # random parameters
  mean_turnout    <- runif(1, 0.1, 0.9)
  var_turnout     <- runif(1, 0.05, 0.15)
  mean_incumbent  <- runif(1, 0.1, 0.9)
  var_incumbent   <- runif(1, 0.05, 0.15)
  mean_fraud      <- round(runif(1, 100, 1000))
  var_fraud       <- round(runif(1, 50, 100))
  
  # for each precinct we generate a random turnout and votes for incumbent (normal distribution)
  df$actual_turnout_percent <- pmin(pmax(rnorm(n_precincts, mean_turnout, var_turnout), 0.01), 1)
  df$actual_turnout_absolute <- round(df$actual_turnout_percent * df$actual_voters_registered)
  df$actual_incumbent_percent <- pmin(pmax(rnorm(n_precincts, mean_incumbent, var_incumbent), 0), 1)
  df$actual_incumbent_votes <- round(df$actual_incumbent_percent * df$actual_turnout_absolute)
  
  # filter precincts with 0 turnout to avoid division by 0
  df <- df[df$actual_turnout_absolute > 0, ]
  n <- nrow(df)
  
  # Stuff ballots on 20% of precincts in region 1
  df$fraudulent_ballots <- ifelse(
    df$region == 1 & runif(n) <= 0.2,
    pmax(round(rnorm(n, mean_fraud, var_fraud)), 0),
    0
  )
  
  # Final data
  df$reported_turnout_absolute <- df$actual_turnout_absolute + df$fraudulent_ballots
  df$reported_incumbent_votes <- df$actual_incumbent_votes + df$fraudulent_ballots
  df$reported_voters_registered <- pmax(df$actual_voters_registered, df$reported_turnout_absolute)
  
  df$reported_turnout_percent <- df$reported_turnout_absolute / df$reported_voters_registered
  df$reported_incumbent_percent <- df$reported_incumbent_votes / df$reported_turnout_absolute
  
  # protect against bad correlations
  subset1 <- df[df$region == 1, ]
  subset2 <- df[df$region == 2, ]
  
  corr_m <- suppressWarnings(cor(subset1$reported_turnout_percent,
                                 subset1$reported_incumbent_percent,
                                 use = "complete.obs"))
  corr_c <- suppressWarnings(cor(subset2$reported_turnout_percent,
                                 subset2$reported_incumbent_percent,
                                 use = "complete.obs"))
  
  if (!is.finite(corr_m)) corr_m <- NA
  if (!is.finite(corr_c)) corr_c <- NA
  
  results <- rbind(results, data.frame(sim_id = r, corr_m = corr_m, corr_c = corr_c))
}



# Visualization
results_long <- results %>%
  tidyr::pivot_longer(cols = c("corr_m", "corr_c"),
                      names_to = "region", values_to = "correlation") %>%
  mutate(region = ifelse(region == "corr_m", "Region 1 (with fraud)", "Region 2 (control)"))

ggplot(results_long, aes(x = region, y = correlation, fill = region)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") +
  labs(title = "Boxplot of Correlation by Region",
       x = "", y = "Correlation") +
  theme_minimal()

# Clean-up: delete all data frames
rm(results, df, results_long, corr_c, corr_m, mean_fraud, mean_incumbent) 
rm(mean_turnout, n_precincts, r, reps, var_fraud, var_incumbent, var_turnout)
rm(subset1, subset2, n)
