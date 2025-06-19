# this code supports the validation of the fraud index through correlating the fraud index with two metrics of the institutional quality

library(dplyr)
library(readr)
library(gt)

# 1) Load the CSVs
ombudsman <- readr::read_csv2("data/ombudsmanindex.csv")
opora <- readr::read_csv2("data/oporaindex.csv")

# 2) Prepare region_summary for 2012 and 2018 separately and compute fraud ranks (lower fraud = better)
fraud_2012 <- region_summary %>%
  filter(year == 2012) %>%
  filter(region_code %in% opora$region_code) %>%
  mutate(fraud_rank = rank(fraud_index, ties.method = "first"))

fraud_2018 <- region_summary %>%
  filter(year == 2018) %>%
  filter(region_code %in% ombudsman$region_code) %>%
  mutate(fraud_rank = rank(fraud_index, ties.method = "first"))

# 3) Join and conduct Spearman’s correlation

# Opora 2012
opora_joined <- fraud_2012 %>%
  inner_join(opora, by = "region_code")

opora_corr <- cor.test(opora_joined$fraud_rank, opora_joined$opora_2012_rank, method = "spearman")

# Ombudsman 2019 vs 2018 fraud
ombudsman_joined <- fraud_2018 %>%
  inner_join(ombudsman, by = "region_code")

ombudsman_corr <- cor.test(ombudsman_joined$fraud_rank, ombudsman_joined$ombudsman_rank, method = "spearman")

# 4) Format results into a nice table

# Add significance asterisks
ranking_corr_table <- tibble::tibble(
  Comparison = c("Opora 2012 vs Fraud 2012", "Administrative Pressure 2019 vs Fraud 2018"),
  Spearman_rho = c(opora_corr$estimate, ombudsman_corr$estimate),
  p_value = c(opora_corr$p.value, ombudsman_corr$p.value),
  N = c(nrow(opora_joined), nrow(ombudsman_joined))
) %>%
  mutate(
    `Spearman's ρ` = round(Spearman_rho, 3),
    `p-value` = ifelse(p_value < 0.001, "< 0.001", sprintf("%.3f", p_value)),
    Significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  select(Comparison, `Spearman's ρ`, `p-value`, Significance, N)

# Display as gt table
ranking_corr_table %>%
  gt() %>%
  tab_header(
    title = "Correlation Between Institutional Rankings and Electoral Fraud",
  ) %>%
  cols_label(
    Comparison = "Comparison",
    `Spearman's ρ` = "Spearman's ρ",
    `p-value` = "p-value",
    Significance = "",
    N = "N Regions"
  ) %>%
  tab_source_note(md("*Significance codes: *** < 0.001, ** < 0.01, * < 0.05*"))


rm(fraud_2012, fraud_2018, ombudsman, ombudsman_corr, ombudsman_joined, opora, opora_corr, opora_joined, ranking_corr_table)
