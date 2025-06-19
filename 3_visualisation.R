# This script creates visualisations of the key insights

library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(ggpattern)


# Visualisation 1: Kobak's diagram
# This replicates the diagram created by Dmitry Kobak for The Economist (Kobak, 2021).

ggplot(precinct_df, aes(x = turnout_share, y = incumbent_share)) +
  geom_point(
    alpha = 0.005,      
    size  = 0.00001,        
    colour = "steelblue"   
  ) +
  labs(
    title = "Incumbent Share vs Turnout (Kobak's diagram)",
    x = "Turnout",
    y = "Incumbent Share"
  ) +
  theme_minimal()+ 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Visualisation 2: Kobak's diagram for regions with and without round_index
# The following visualization demonstrates the validity of the roundness index.
# We split all precincts into two groups based on whether they belong to region-years
# with a zero roundness index ("non-suspicious") or a non-zero roundness index ("suspicious").
# 
# In the plot, we reproduce Kobak's diagram for each group separately.
# In the suspicious group (right panel), we clearly observe a grid pattern formed by
# precincts reporting round percentages of turnout and incumbent vote share— a sign of electoral manipulation.
# In contrast, this pattern is much less pronounced in the non-suspicious group (left panel), where the distribution appears more diffuse and organic.
# 
# This visual distinction supports the conclusion that the roundness index is effective
# at identifying region-years with signs of suspicious electoral behavior.


precinct_merged <- precinct_df %>%
  left_join(region_summary %>% select(region_code, year, round_index),
            by = c("region_code", "year")) %>%
  mutate(round_group = ifelse(round_index == 0, "Round Index = 0", "Round Index > 0"))


# 2A. Plot with facet_wrap (side-by-side plots)
ggplot(precinct_merged, aes(x = turnout_share, y = incumbent_share)) +
  geom_point(
    aes(color = round_group),
    alpha = 0.005,
    size = 0.00001
  ) +
  #geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.8) +
  scale_color_manual(
    values = c(
      "Round Index = 0" = "steelblue",
      "Round Index > 0" = "firebrick"
    )
  ) +
  facet_wrap(~round_group) +
  labs(
    title = "Kobak's Diagram by Round Index Group",
    x = "Turnout Share",
    y = "Incumbent Share"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


rm(precinct_merged)


# Visualisation 3: Kobak's diagram for regions with and without correlation_index
# This plot presents a validation of the correlation index
# We split all precincts into two groups based on whether they belong to region-years
# with a zero correlation index ("non-suspicious") or a non-zero correlation index ("suspicious").
# 
# In the plot, we reproduce Kobak's diagram for each group separately.
# We clearly observe the difference in the correlation  between the incumbent share and turnout share between the two subgroups.

# This visual distinction supports the conclusion that the correlation index captures the signs of the suspicious electoral behavior.

precinct_corr <- precinct_df %>%
  left_join(region_summary %>% select(region_code, year, correlation_index),
            by = c("region_code", "year")) %>%
  mutate(corr_group = ifelse(correlation_index == 0, "Correlation Index = 0", "Correlation Index > 0"))

group_stats <- precinct_corr %>%
  group_by(corr_group) %>%
  summarise(
    n = n(),
    corr = cor(turnout_share, incumbent_share, use = "complete.obs")
  ) %>%
  mutate(
    label = paste0(
      corr_group,
      "\n(n = ", n, ")",
      "\nρ = ", sprintf("%.2f", corr)
    )
  )

precinct_corr <- precinct_corr %>%
  left_join(group_stats %>% select(corr_group, label), by = "corr_group")

ggplot(precinct_corr, aes(x = turnout_share, y = incumbent_share)) +
  geom_point(aes(color = corr_group), alpha = 0.005, size = 0.00001) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.8) +
  scale_color_manual(
    values = c(
      "Correlation Index = 0" = "steelblue",
      "Correlation Index > 0" = "firebrick"
    )
  ) +
  facet_wrap(~label) +
  labs(
    title = "Kobak's Diagram by Correlation Index Group",
    x = "Turnout Share",
    y = "Incumbent Share"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

rm(precinct_corr, group_stats)


# Visualisation 4. Kobak's diagram for quartiles
# This plot shows Kobak's diagrams (incumbent vote share vs turnout) separately for five categories of region-years, based on their fraud index.
# Group 0 includes regions with a zero fraud index.
# The remaining region-years are divided into quartiles (Groups 1–4) based on the distribution of non-zero fraud indices.

# The panels are color-coded by fraud group to enhance visual comparison.
# This visualization helps assess how the relationship between turnout and incumbent support
# evolves across different levels of suspected electoral manipulation.

region_summary <- region_summary %>%
  mutate(fraud_group = ifelse(fraud_index == 0, "Group 0", NA))

# Quartiles for fraud_index > 0
quantiles <- quantile(region_summary$fraud_index[region_summary$fraud_index > 0],
                      probs = seq(0, 1, 0.25), na.rm = TRUE)

# Divide fraud_index > 0 by quartiles
region_summary$fraud_group <- ifelse(region_summary$fraud_index == 0, "Group 0", NA)
region_summary$fraud_group[region_summary$fraud_index > 0] <- as.character(cut(
  region_summary$fraud_index[region_summary$fraud_index > 0],
  breaks = quantile(region_summary$fraud_index[region_summary$fraud_index > 0], 
                    probs = seq(0, 1, 0.25), na.rm = TRUE),
  include.lowest = TRUE,
  labels = c("Group 1", "Group 2", "Group 3", "Group 4")
))

# Step 2: add fraud_group to each precinct through join
precinct_with_fraud <- precinct_df %>%
  left_join(region_summary %>% select(region_code, year, fraud_group),
            by = c("region_code", "year"))

# Step 3: count n and correlation for each group
group_stats <- precinct_with_fraud %>%
  group_by(fraud_group) %>%
  summarise(
    n = n(),
    corr = cor(turnout_share, incumbent_share, use = "complete.obs")
  ) %>%
  mutate(
    label = paste0(fraud_group, "\n(n = ", n, ")\nρ = ", sprintf("%.2f", corr))
  )

# Step 4: add label to the precincts
precinct_with_fraud <- precinct_with_fraud %>%
  left_join(group_stats %>% select(fraud_group, label), by = "fraud_group")

precinct_with_fraud$fraud_group <- factor(
  precinct_with_fraud$fraud_group,
  levels = c("Group 0", "Group 1", "Group 2", "Group 3", "Group 4")
)


# Step 5: create a Kobak diagram for each group separately
ggplot(precinct_with_fraud, aes(x = turnout_share, y = incumbent_share, color = fraud_group)) +
  geom_point(alpha = 0.005, size = 0.00001) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.8) +
  facet_wrap(~label) +
  scale_color_manual(
    values = c(
      "Group 0" = "green",
      "Group 1" = "gold",
      "Group 2" = "orange",
      "Group 3" = "tomato",
      "Group 4" = "red"
    )
  ) +
  labs(
    title = "Kobak's Diagram by Fraud Index Group",
    x = "Turnout Share",
    y = "Incumbent Share",
    color = "Fraud Group"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Step 6: create a Kobak diagram for all groups on the same plane
ggplot(precinct_with_fraud, aes(x = turnout_share, y = incumbent_share, color = fraud_group)) +
  geom_point(alpha = 0.005, size = 0.00001) +
  scale_color_manual(
    values = c(
      "Group 0" = "green",
      "Group 1" = "gold",
      "Group 2" = "orange",
      "Group 3" = "tomato",
      "Group 4" = "red"
    ),
  ) +
  labs(
    title = "Kobak's Diagram Colored by Fraud Index Group",
    x = "Turnout Share",
    y = "Incumbent Share",
    color = "Fraud Group"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

rm(group_stats, precinct_with_fraud, quantiles)

# Visualisation 5. Geographic distribution
# We visualise the distribution of fraud_index across the regions for each year.
# We use a tilemap created by Anton Mizinov for visualising the geographic distribution of the fraud index for different Russian regions
# Source of the tilemap: https://github.com/quillcraft/tilemap-russia

# Load tilemap and region names
tilemap <- readr::read_csv("https://raw.githubusercontent.com/quillcraft/tilemap-russia/refs/heads/master/tilemap.csv")
region_map <- read.csv("data/region_code_converter.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)

tilemap <- tilemap %>%
  left_join(region_map, by = c("name_ru" = "region"))  # add region_code

tilemap_expanded <- expand.grid(
  region_code = unique(tilemap$region_code[!is.na(tilemap$region_code)]),
  year = years
) %>%
  left_join(tilemap, by = "region_code")

# Merge directly with region_summary (which contains fraud_group)
tiledata <- tilemap %>%
  inner_join(region_summary %>% select(region_code, year, fraud_group), by = "region_code")

# Ensure correct factor ordering
tiledata$fraud_group <- factor(
  tiledata$fraud_group,
  levels = c("Group 0", "Group 1", "Group 2", "Group 3", "Group 4")
)

tiledata <- tiledata %>%
  mutate(pattern = ifelse(region_code %in% c("CRY", "SEV"), "stripe", "none"))

# Plot tilemap
ggplot(tiledata, aes(x = lon, y = -lat)) +
  geom_tile_pattern(
    aes(
      fill = fraud_group,
      pattern = pattern
    ),
    color = "white",
    pattern_fill = "lightgrey",
    pattern_density = 0.01,
    pattern_spacing = 0.02,
    pattern_angle = 45,
    pattern_alpha = 0.4,
    show.legend = c(fill = TRUE, pattern = TRUE)
  ) +
  
  geom_text(aes(label = region_code), size = 2.5) +
  
  scale_fill_manual(
    values = c(
      "Group 0" = "green",
      "Group 1" = "gold",
      "Group 2" = "orange",
      "Group 3" = "tomato",
      "Group 4" = "red"
    ),
    na.value = "grey80",
    name = "Fraud Index Group"
  ) +
  
  scale_pattern_manual(
    values = c("stripe" = "stripe", "none" = "none"),
    name = "Regions",
    labels = c("none" = "Russian region", "stripe" = "Occupied region")
  ) +
  
  guides(
    fill = guide_legend(override.aes = list(pattern = "none")),
    pattern = guide_legend(override.aes = list(fill = "lightgrey"))
  ) +
  
  coord_fixed() +
  facet_wrap(~year) +
  labs(
    title = "Fraud Index by Region and Year (Tile Map)",
    x = NULL, y = NULL
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10)
  )

rm(region_map, tiledata, tilemap, tilemap_expanded)


# Visualization 5a: Geographic distribution divided into three parts

# Group 1
plot_years <- c(2000, 2003, 2004)

facet_order <- c(as.character(plot_years), " ")  # leave 4th facet blank

# Prepare data with a dummy panel to fill the 2×2 grid
tiledata_sub <- tiledata %>%
  filter(year %in% plot_years) %>%
  mutate(year = as.character(year)) %>%
  bind_rows(
    tibble(
      region_code = NA,
      lon = NA,
      lat = NA,
      fraud_group = NA,
      year = " "
    )
  ) %>%
  mutate(year = factor(year, levels = facet_order))  # set desired facet order

# Plot
ggplot(tiledata_sub, aes(x = lon, y = -lat)) +
  geom_tile(
    aes(fill = fraud_group),
    color = "white"
  ) +
  geom_text(aes(label = region_code), size = 2.5, na.rm = TRUE) +
  scale_fill_manual(
    values = c(
      "Group 0" = "green",
      "Group 1" = "gold",
      "Group 2" = "orange",
      "Group 3" = "tomato",
      "Group 4" = "red"
    ),
    na.value = "grey80",
    name = "Fraud Index Group"
  ) +
  coord_fixed() +
  facet_wrap(~year, ncol = 2) +
  labs(
    title = "Fraud Index by Region and Year",
    x = NULL, y = NULL
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10)
  )

# Group 2
plot_years <- c(2007, 2008, 2011)

facet_order <- c(as.character(plot_years), " ")  # leave 4th facet blank

# Prepare data with a dummy panel to fill the 2×2 grid
tiledata_sub <- tiledata %>%
  filter(year %in% plot_years) %>%
  mutate(year = as.character(year)) %>%
  bind_rows(
    tibble(
      region_code = NA,
      lon = NA,
      lat = NA,
      fraud_group = NA,
      year = " "
    )
  ) %>%
  mutate(year = factor(year, levels = facet_order))  # set desired facet order

# Plot
ggplot(tiledata_sub, aes(x = lon, y = -lat)) +
  geom_tile(
    aes(fill = fraud_group),
    color = "white"
  ) +
  geom_text(aes(label = region_code), size = 2.5, na.rm = TRUE) +
  scale_fill_manual(
    values = c(
      "Group 0" = "green",
      "Group 1" = "gold",
      "Group 2" = "orange",
      "Group 3" = "tomato",
      "Group 4" = "red"
    ),
    na.value = "grey80",
    name = "Fraud Index Group"
  ) +
  coord_fixed() +
  facet_wrap(~year, ncol = 2) +
  labs(
    title = "Fraud Index by Region and Year",
    x = NULL, y = NULL
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10)
  )

# Group 3
plot_years <- c(2012, 2016, 2018)

# Create the full list of facet labels, with blank panel last
facet_order <- c(as.character(plot_years), " ")

# Prepare data
tiledata_sub <- tiledata %>%
  filter(year %in% plot_years) %>%
  mutate(year = as.character(year)) %>%
  bind_rows(
    tibble(
      region_code = NA,
      lon = NA,
      lat = NA,
      fraud_group = NA,
      pattern = NA,
      year = " "
    )
  ) %>%
  mutate(year = factor(year, levels = facet_order))  # Control facet order

# Plot
ggplot(tiledata_sub, aes(x = lon, y = -lat)) +
  geom_tile_pattern(
    aes(fill = fraud_group, pattern = pattern),
    color = "white",
    pattern_fill = "lightgrey",
    pattern_density = 0.01,
    pattern_spacing = 0.02,
    pattern_angle = 45,
    pattern_alpha = 0.4,
    show.legend = c(fill = TRUE, pattern = TRUE)
  ) +
  geom_text(aes(label = region_code), size = 2.5, na.rm = TRUE) +
  scale_fill_manual(
    values = c("Group 0" = "green", "Group 1" = "gold", "Group 2" = "orange", "Group 3" = "tomato", "Group 4" = "red"),
    na.value = "grey80",
    name = "Fraud Index Group"
  ) +
  scale_pattern_manual(
    values = c("stripe" = "stripe", "none" = "none"),
    name = "Regions",
    labels = c("none" = "Russian region", "stripe" = "Occupied region")
  ) +
  guides(
    fill = guide_legend(override.aes = list(pattern = "none")),
    pattern = guide_legend(override.aes = list(fill = "lightgrey"))
  ) +
  coord_fixed() +
  facet_wrap(~year, ncol = 2) +
  labs(
    title = "Fraud Index by Region and Year",
    x = NULL, y = NULL
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10)
  )

rm(region_map, tiledata_sub, tiledata, tilemap, tilemap_expanded)
rm(facet_order, plot_years)

# Visualization 6: The dynamics of the fraud index over time.
# Here we look at the dynamics of the fraud index (weighed by the total turnout in the region) across the elections.

national_trend <- region_summary %>%
  group_by(year) %>%
  summarise(weighted_fraud_index = sum(fraud_index * sum_turnout_abs) / sum(sum_turnout_abs), .groups = "drop")

ggplot(national_trend, aes(x = year, y = weighted_fraud_index)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = round(weighted_fraud_index, 2)), vjust = -0.5, size = 3.5) +
  labs(title = "National Turnout-Weighted Fraud Index Over Time",
       x = "Year", y = "Turnout-Weighted Fraud Index") +
  theme_minimal()

rm(national_trend)

