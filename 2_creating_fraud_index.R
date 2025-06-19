# This code creates fraud_index

setwd("") #path to your folder
library(dplyr)
library(tidyr)
library(gt)



# Step 1: for each precinct establish turnout and share of votes for incumbent
# turnout is the sum of valid and invalid ballots. Turnout_share is turnout divided by number of registered voters
# incumbent share is votes for incumbent divided by turnout

precinct_df$turnout_abs <- precinct_df$valid_ballots + precinct_df$invalid_ballots
precinct_df$turnout_share <- ifelse(precinct_df$registered_voters > 0, precinct_df$turnout_abs / precinct_df$registered_voters, NA)
precinct_df$incumbent_share <- ifelse(precinct_df$turnout_abs > 0, precinct_df$votes_for_incumbent / precinct_df$turnout_abs, NA)

# Step 2: for each precinct add the probabilities to get round results

# round_probabilities_table.csv is created using a python script create_round_prob_table.py

# We calculate the probability that an election with the given number of voters 
# (registered and actual turnout) would produce a round percentage purely by chance.
# We check how many possible vote counts would yield exactly 1%- or 5%-increment results.
# These probabilities are used to estimate the expected number of precincts with round figures,
# which is then compared with observed counts using chi-squared tests to detect anomalies indicative of protocol falsification


prob_tbl <- read.csv("data/round_probabilities_table.csv", colClasses = c(voters_number = "integer"))
precinct_df <- precinct_df %>%
  left_join(prob_tbl %>% rename(registered_voters = voters_number,
                                p005turnout = p_005,
                                p001turnout = p_001),
            by = "registered_voters") %>%
  left_join(prob_tbl %>% rename(turnout_abs = voters_number,
                                p005incumbent = p_005,
                                p001incumbent = p_001),
            by = "turnout_abs")

# Step 3: create flags for precincts with actual round results

round_flag <- function(count, denom, grid = 1) {
  ks <- seq(grid, 100, by = grid)
  diffs <- abs(count - denom * ks / 100)
  if (any(diffs < 1 - 1e-12)) {
    k <- ks[which.min(diffs)]
    c(flag = 1L, k = as.integer(k))
  } else {
    c(flag = 0L, k = NA_integer_)
  }
}

precinct_df <- precinct_df %>%
  rowwise() %>%
  mutate(
    tmp5 = list(round_flag(turnout_abs, registered_voters, 5)),
    `005round_turnout` = tmp5[[1]],
    tmp1 = list(round_flag(turnout_abs, registered_voters, 1)),
    `001round_turnout` = tmp1[[1]],
    round_turnout_value = tmp1[[2]],
    
    tmp5i = list(round_flag(votes_for_incumbent, turnout_abs, 5)),
    `005round_incumbent` = tmp5i[[1]],
    tmp1i = list(round_flag(votes_for_incumbent, turnout_abs, 1)),
    `001round_incumbent` = tmp1i[[1]],
    round_incumbent_value = tmp1i[[2]]
  ) %>%
  ungroup() %>%
  select(-tmp5, -tmp1, -tmp5i, -tmp1i)

#Step 4 create a table grouped by region and year with indicators for the roundness and correlation

region_summary <- precinct_df %>%
  filter(turnout_abs >= 200, turnout_abs <= 3000) %>%
  group_by(region_code, year) %>%
  group_modify(~ {
    df <- .x
    cor_val <- suppressWarnings(cor(df$turnout_abs / df$registered_voters,
                                    df$votes_for_incumbent / df$turnout_abs,
                                    use = "complete.obs", method = "pearson"))
    
    tibble(
      n_obs = nrow(df),
      sum_registered_voters = sum(df$registered_voters, na.rm = TRUE),
      sum_turnout_abs = sum(df$turnout_abs, na.rm = TRUE),
      sum_votes_for_incumbent = sum(df$votes_for_incumbent, na.rm = TRUE),
      overall_turnout = sum(df$turnout_abs, na.rm = TRUE) / sum(df$registered_voters, na.rm = TRUE),
      overall_incumbent_share = sum(df$votes_for_incumbent, na.rm = TRUE) / sum(df$turnout_abs, na.rm = TRUE),
      
      sum_005round_turnout = sum(df$`005round_turnout`, na.rm = TRUE),
      sum_001round_turnout = sum(df$`001round_turnout`, na.rm = TRUE),
      sum_005round_incumbent = sum(df$`005round_incumbent`, na.rm = TRUE),
      sum_001round_incumbent = sum(df$`001round_incumbent`, na.rm = TRUE),
      
      sum_p005turnout = sum(df$p005turnout, na.rm = TRUE),
      sum_p001turnout = sum(df$p001turnout, na.rm = TRUE),
      sum_p005incumbent = sum(df$p005incumbent, na.rm = TRUE),
      sum_p001incumbent = sum(df$p001incumbent, na.rm = TRUE),
      
      correlation = cor_val
    )
  }) %>%
  rowwise() %>%
  mutate(
    chisq_pval_005turnout = chisq.test(c(sum_005round_turnout, n_obs - sum_005round_turnout),
                                       p = c(sum_p005turnout, n_obs - sum_p005turnout) / n_obs)$p.value,
    chisq_pval_001turnout = chisq.test(c(sum_001round_turnout, n_obs - sum_001round_turnout),
                                       p = c(sum_p001turnout, n_obs - sum_p001turnout) / n_obs)$p.value,
    chisq_pval_005incumbent = chisq.test(c(sum_005round_incumbent, n_obs - sum_005round_incumbent),
                                         p = c(sum_p005incumbent, n_obs - sum_p005incumbent) / n_obs)$p.value,
    chisq_pval_001incumbent = chisq.test(c(sum_001round_incumbent, n_obs - sum_001round_incumbent),
                                         p = c(sum_p001incumbent, n_obs - sum_p001incumbent) / n_obs)$p.value,
    
    flag_005turnout = as.integer(chisq_pval_005turnout < 0.05),
    flag_001turnout = as.integer(chisq_pval_001turnout < 0.05),
    flag_005incumbent = as.integer(chisq_pval_005incumbent < 0.05),
    flag_001incumbent = as.integer(chisq_pval_001incumbent < 0.05),
    
    round_index_005turnout   = ifelse(flag_005turnout == 1, sum_005round_turnout / n_obs, 0),
    round_index_001turnout   = ifelse(flag_001turnout == 1, sum_001round_turnout / n_obs, 0),
    round_index_005incumbent = ifelse(flag_005incumbent == 1, sum_005round_incumbent / n_obs, 0),
    round_index_001incumbent = ifelse(flag_001incumbent == 1, sum_001round_incumbent / n_obs, 0),
    
    correlation_index = ifelse(!is.na(correlation) & correlation > 0.05, correlation, 0),
    round_index = max(round_index_005turnout, round_index_001turnout,
                      round_index_005incumbent, round_index_001incumbent),
    fraud_index = (correlation_index + round_index) / 2
  ) %>%
  ungroup()

# Add English and Russian names
russian_names <- precinct_df %>%
  group_by(region_code) %>%
  summarise(russian_name = first(region[!is.na(region)]), .groups = "drop")

region_summary <- region_summary %>%
  left_join(russian_names, by = "region_code")

converter <- read.csv2("data/region_code_converter.csv", stringsAsFactors = FALSE)

english_names <- converter %>%
  filter(!is.na(english_name)) %>%
  group_by(region_code) %>%
  summarise(english_name = first(english_name), .groups = "drop")

region_summary <- region_summary %>%
  left_join(english_names, by = "region_code")

# Step 5: create a table for publishing

fraud_wide <- region_summary %>%
  select(english_name, year, fraud_index) %>%
  pivot_wider(names_from = year, values_from = fraud_index)

year_cols <- colnames(fraud_wide)[-1]
label_vector <- setNames(paste("Year", year_cols), year_cols)


# Create a  table using gt
fraud_table <- fraud_wide %>%
  arrange(english_name) %>%
  gt() %>%
  tab_header(
    title = "Fraud Index by Region and Year"
  ) %>%
  fmt_number(
    columns = all_of(year_cols),
    decimals = 3
  ) %>%
  cols_label(.list = label_vector) %>%
  tab_options(
    table.width = pct(100),
    heading.align = "left",
    data_row.padding = px(4),
    row_group.padding = px(4)
  )

gtsave(fraud_table, filename = "fraud_table.html")



# Step 6: delete all unnecessary data
region_summary <- region_summary %>% select(-c(n_obs, sum_registered_voters,sum_votes_for_incumbent))
region_summary <- region_summary %>% select(-c(sum_005round_turnout, sum_001round_turnout,sum_005round_incumbent,sum_001round_incumbent))
region_summary <- region_summary %>% select(-c(sum_p005turnout, sum_p001turnout, sum_p005incumbent,sum_p001incumbent))
region_summary <- region_summary %>% select(-c(chisq_pval_005turnout,chisq_pval_001turnout, chisq_pval_005incumbent, chisq_pval_001incumbent))
region_summary <- region_summary %>% select(-c(flag_005turnout,flag_001turnout, flag_005incumbent, flag_001incumbent))
region_summary <- region_summary %>% select(-c(round_index_005turnout, round_index_001turnout,round_index_005incumbent,round_index_001incumbent))

precinct_df <- precinct_df %>% select (-c(region, tik, invalid_ballots, valid_ballots, votes_for_incumbent))
precinct_df <- precinct_df %>% select (-c(p005turnout, p001turnout, p005incumbent, p001incumbent))
precinct_df <- precinct_df %>% select (-c(`005round_turnout`, `001round_turnout`, `005round_incumbent`, `001round_incumbent`))

rm(prob_tbl, round_flag)
rm(russian_names, english_names, converter)
rm(unique_regions)
rm(fraud_table, fraud_wide, label_vector, year_cols)

# Step 6: saving
save(precinct_df, region_summary, file = "elections_data.RData")
