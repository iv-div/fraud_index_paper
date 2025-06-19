# This script Correlates the correlation_index and the round_index 
library(gt)
library(webshot2)
library(ggplot2)

# Correlation across the whole dataframe
ggplot(region_summary, aes(x = correlation_index, y = round_index)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Correlation vs Round Index (by Region-Year)",
       x = "Correlation Index", y = "Round Index") +
  theme_minimal()

cor.test(region_summary$correlation_index, region_summary$round_index, use = "complete.obs", method = "pearson")

# Correlation within each year
within_year_corrs <- region_summary %>%
  group_by(year) %>%
  summarise(
    cor_test = list(cor.test(correlation_index, round_index, use = "complete.obs", method = "pearson")),
    .groups = "drop"
  ) %>%
  mutate(
    cor_value = sapply(cor_test, function(x) x$estimate),
    p_value   = sapply(cor_test, function(x) x$p.value),
    n         = sapply(cor_test, function(x) x$parameter + 2)  # degrees of freedom + 2 = number of observations
  ) %>%
  select(year, cor_value, p_value, n)

presidential_years <- c(2000, 2004, 2008, 2012, 2018)

region_corr_table <- within_year_corrs %>%
  mutate(
    cor_value = round(cor_value, 3),
    significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    p_display = ifelse(p_value < 0.001, "< 0.001", sprintf("%.3f", p_value))
  ) %>%
  select(Year = year, `Correlation (r)` = cor_value, `p-value` = p_display, `Significance` = significance, `N` = n) %>%
  gt() %>%
  tab_header(
    title = "Correlation Between Fraud Index Subcomponents by Year",
    subtitle = "Pearson correlation between correlation_index and round_index"
  ) %>%
  cols_label(
    `Correlation (r)` = "Correlation",
    `p-value` = "p-value",
    `Significance` = "",
    `N` = "N Regions"
  ) %>%
  tab_source_note(md("*Significance codes: *** < 0.001, ** < 0.01, * < 0.05*")) %>%
  tab_style(
    style = list(cell_fill(color = "#f2f0f7")),
    locations = cells_body(
      rows = Year %in% presidential_years
    )
  )

region_corr_table
#gtsave(region_corr_table, "correlation_table.html")
gtsave(region_corr_table, "correlation_table.png")


rm(within_year_corrs, presidential_years,region_corr_table)

