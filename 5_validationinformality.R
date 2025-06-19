# this code supports the validation of the fraud index through correlating the fraud index with informal employment
# First, we create a dataframe of all available data on informal employment in Russian regions in 2001-2018, then we correlate the index
# the data sources are collected from the Federal State Statistics Service (Rosstat)'s website and stored in .../RosstatData

library(rvest)
library(dplyr)
library(docxtractr)
library(readxl)
library(stringr)
library(stringdist)
library(broom)
library(tidyr)
library(zoo)
library(gt)
library(flextable)




### Step 1 - loading the data
data_folder <- "data/RosstatData"

#2001

html_file <- read_html(file.path(data_folder, "rosstatinfor2001.html"))
table_node <- html_file %>% html_element("table")

# Parse the table into a data frame
raw_table <- table_node %>% html_table(fill = TRUE)
informal_employment <- raw_table %>%
  select(region_russian_name = 1, `2001` = 7)

informal_employment <- informal_employment %>%
  filter(!is.na(region_russian_name) & region_russian_name != "")

rm(html_file,table_node,raw_table)

#2004

html_file <- read_html(file.path(data_folder, "rosstatinfor2004.html"))
table_node <- html_file %>% html_element("table")

# Parse the table into a data frame
raw_table <- table_node %>% html_table(fill = TRUE)
data_2004 <- raw_table %>%
  select(region_russian_name = 1, `2004` = 7)
informal_employment <- full_join(informal_employment, data_2004, by = "region_russian_name")
informal_employment <- informal_employment %>%
  filter(!is.na(region_russian_name) & region_russian_name != "")

rm(html_file,table_node,raw_table, data_2004)

#2007
html_file <- read_html(file.path(data_folder, "rosstatinfor2007.html"))
table_node <- html_file %>% html_element("table")

# Parse the table into a data frame
raw_table <- table_node %>% html_table(fill = TRUE)
data_2007 <- raw_table %>%
  select(region_russian_name = 1, `2007` = 7)
informal_employment <- full_join(informal_employment, data_2007, by = "region_russian_name")
informal_employment <- informal_employment %>%
  filter(!is.na(region_russian_name) & region_russian_name != "")

rm(html_file,table_node,raw_table, data_2007)

#2009
html_file <- read_html(file.path(data_folder, "rosstatinfor2009.html"))
table_node <- html_file %>% html_element("table")

# Parse the table into a data frame
raw_table <- table_node %>% html_table(fill = TRUE)
data_2009 <- raw_table %>%
  select(region_russian_name = 1, `2009` = 7)
informal_employment <- full_join(informal_employment, data_2009, by = "region_russian_name")
informal_employment <- informal_employment %>%
  filter(!is.na(region_russian_name) & region_russian_name != "")

rm(html_file,table_node,raw_table, data_2009)

#2012
doc <- read_docx(file.path(data_folder, "rosstatinfor2012.docx"))
table_2012 <- docx_extract_tbl(doc, tbl_number = 1)
data_2012 <- table_2012 %>%
  select(region_russian_name = 1, `2012` = 7)
informal_employment <- full_join(informal_employment, data_2012, by = "region_russian_name")
informal_employment <- informal_employment %>%
  filter(!is.na(region_russian_name) & region_russian_name != "")
rm(doc,table_2012, data_2012)

#2013
xls_2013 <- read_excel(file.path(data_folder, "rosstatinfor2013.xls"))
data_2013 <- xls_2013 %>%
  select(region_russian_name = 1, `2013` = 7)
informal_employment <- full_join(informal_employment, data_2013, by = "region_russian_name")
informal_employment <- informal_employment %>%
  filter(!is.na(region_russian_name) & region_russian_name != "")
rm(xls_2013, data_2013)

#2015
xls_2015 <- read_excel(file.path(data_folder, "rosstatinfor2015.xls"))
data_2015 <- xls_2015 %>%
  select(region_russian_name = 1, `2015` = 7)
informal_employment <- full_join(informal_employment, data_2015, by = "region_russian_name")
informal_employment <- informal_employment %>%
  filter(!is.na(region_russian_name) & region_russian_name != "")
rm(xls_2015, data_2015)

#2017
xls_2017 <- read_excel(file.path(data_folder, "rosstatinfor2017.xls"))
data_2017 <- xls_2017 %>%
  select(region_russian_name = 1, `2017` = 7)
informal_employment <- full_join(informal_employment, data_2017, by = "region_russian_name")
informal_employment <- informal_employment %>%
  filter(!is.na(region_russian_name) & region_russian_name != "")
rm(xls_2017, data_2017)

### Step 2 cleaning the data:

informal_employment <- informal_employment %>%
  mutate(region_russian_name = str_squish(str_replace_all(region_russian_name, "\\u00A0", " ")))

informal_employment <- informal_employment %>% distinct()

regions_to_exclude <- c(
  "Российская Федерация", "Центральный федеральный округ", "Северо-Западный федеральный округ",
  "Южный федеральный округ", "Приволжский федеральный округ", "в том числе:",
  "Сибирский федеральный округ", "Дальневосточный федеральный округ", "Уральский федеральный округ",
  "(тысяч человек)", "Российская Федеpация", "Северо-Кавказский федеральный округ",
  "Крымский федеральный округ", "Центральный федеральный округ"
)

# Remove them from the dataframe
informal_employment <- informal_employment %>%
  filter(!region_russian_name %in% regions_to_exclude)



# Find near-duplicates (edit distance ≤ 2)
region_names <- as.character(unique(informal_employment$region_russian_name))

pairs <- expand.grid(region1 = region_names, region2 = region_names, stringsAsFactors = FALSE) %>%
  filter(region1 < region2) %>%
  mutate(dist = stringdist(region1, region2, method = "lv")) %>%
  filter(dist <= 2)

print(pairs)


# for Arkhangelskaya and Tyumenskaya oblasts prioritize the variants without the autonomous okrugs

merge_rows_prioritize <- function(df, primary, secondary) {
  primary_row <- df %>% filter(region_russian_name == primary)
  secondary_row <- df %>% filter(region_russian_name == secondary)
  
  if (nrow(primary_row) == 0 | nrow(secondary_row) == 0) return(df)  # skip if either is missing
  
  # Coalesce across year-columns (skip region_russian_name and region_code)
  combined_row <- primary_row
  year_cols <- setdiff(names(df), c("region_russian_name", "region_code"))
  
  combined_row[year_cols] <- Map(
    function(p, s) ifelse(is.na(p) | p == "", s, p),
    primary_row[year_cols],
    secondary_row[year_cols]
  )
  
  # Replace the old primary row
  df <- df %>% filter(!region_russian_name %in% c(primary, secondary))
  bind_rows(df, combined_row)
}

informal_employment <- merge_rows_prioritize(
  informal_employment,
  primary = "Архангельская область без авт.округа",
  secondary = "Архангельская область"
)

informal_employment <- merge_rows_prioritize(
  informal_employment,
  primary = "Тюменская область без авт. округов",
  secondary = "Тюменская область"
)


#Manually replace names

name_map <- c(
  "г, Москва" = "Москва",
  "г. Москва1)" = "Москва",
  "г. Москва" = "Москва",
  "Московская область1)" = "Московская область",
  "Республика Северная Осетия - Алания" = "Республика Северная Осетия - Алания",
  "Республика Северная Осетия-Алания" = "Республика Северная Осетия - Алания",
  "в том числе Ненецкий авт.округ" = "Ненецкий автономный округ",
  "в том числе: Ненецкий авт.округ" = "Ненецкий автономный округ",
  "Еврейская авт.область" = "Еврейская автономная область",
  "Республика Адыгея" = "Республика Адыгея (Адыгея)",
  "Республика Татарстан" = "Республика Татарстан (Татарстан)",
  "Чувашская Республика" = "Чувашская Республика - Чаваш республики",
  "Чукотский авт.округ" = "Чукотский автономный округ",
  "Ямало-Ненецкий авт.округ" = "Ямало-Ненецкий автономный округ",
  "в том числе Агинский Бурятский автономный округ" = "Агинский Бурятский автономный округ",
  "в том числе Коми-Пермяцкий автономный округ" = "Коми-Пермяцкий автономный округ",
  "в том числе Корякский автономный округ" = "Корякский автономный округ",
  "в том числе Ненецкий автономный округ" = "Ненецкий автономный округ",
  "в том числе Усть-Ордынский Бурятский автономный округ" = "Усть-Ордынский Бурятский автономный округ",
  "в том числе: Ханты-Мансийский авт.округ" = "Ханты-Мансийский автономный округ",
  "в том числе: Ханты-Мансийский авт.округ - Югра" = "Ханты-Мансийский автономный округ",
  "Ханты-Мансийский автономный округ - Югра" = "Ханты-Мансийский автономный округ",
  "Архангельская область без авт.округа" = "Архангельская область",
  "Тюменская область без авт. округов" = "Тюменская область",
  "г. Севастополь" = "Севастополь",
  "г. Санкт-Петербург" = "Санкт-Петербург",
  "г, Санкт-Петербург" = "Санкт-Петербург"
)

informal_employment <- informal_employment %>%
  mutate(region_russian_name = recode(region_russian_name, !!!name_map))

informal_employment <- informal_employment %>%
  group_by(region_russian_name) %>%
  summarise(across(everything(), ~ first(na.omit(.))), .groups = "drop")


#Add region_codes
region_codes <- read.csv2("data/region_code_converter.csv", stringsAsFactors = FALSE)
informal_employment <- informal_employment %>%
  left_join(region_codes, by = c("region_russian_name" = "region"))


rm(pairs, region_codes, data_folder, name_map, region_names, regions_to_exclude, merge_rows_prioritize)

#convert numbers with decimal comas to decimals with points
year_cols <- grep("^\\d{4}$", names(informal_employment), value = TRUE)

# Convert comma decimals to numeric
informal_employment[year_cols] <- lapply(informal_employment[year_cols], function(col) {
  as.numeric(gsub(",", ".", col))
})

rm(year_cols)

### Step 3 Correlating
# Since the years for the informality and the fraud datasets differ, we employ three different approaches to get a wider picture:

# --- Prep: reshape informal_employment to long format ---
informal_long <- informal_employment %>%
  pivot_longer(cols = -c(region_russian_name, region_code), names_to = "year", values_to = "informal_index") %>%
  mutate(year = as.integer(year))

# --- Prep: region_summary already in long format ---
fraud_data <- region_summary %>%
  select(region_code, year, fraud_index)

# --- 1. Match only overlapping years: 2004, 2007, 2012 ---
overlap_years <- c(2004, 2007, 2012)

matched_df <- fraud_data %>%
  filter(year %in% overlap_years) %>%
  left_join(informal_long %>% filter(year %in% overlap_years),
            by = c("region_code", "year"))

matched_corr <- cor.test(matched_df$fraud_index, matched_df$informal_index, use = "complete.obs")

# --- 2. Interpolation ---
# Interpolation target years = election years
election_years <- c(2000, 2003, 2004, 2007, 2008, 2011, 2012, 2016, 2018)

# Create a complete panel of region_code × all election years
region_years <- expand.grid(
  region_code = unique(informal_long$region_code),
  year = election_years
)

# Interpolate informality values per region
interpolated_informality <- informal_long %>%
  complete(region_code, year = full_seq(c(year, election_years), 1)) %>%
  arrange(region_code, year) %>%
  group_by(region_code) %>%
  mutate(informal_interp = na.approx(informal_index, year, na.rm = FALSE)) %>%
  ungroup() %>%
  filter(year %in% election_years) %>%
  select(region_code, year, informal_index = informal_interp)

# Join with fraud data for the same election years
interp_df <- fraud_data %>%
  filter(year %in% election_years) %>%
  left_join(interpolated_informality, by = c("region_code", "year"))

# Ensure numeric
interp_df <- interp_df %>%
  mutate(
    fraud_index = as.numeric(fraud_index),
    informal_index = as.numeric(informal_index)
  )

# Correlation test
interp_corr <- cor.test(interp_df$fraud_index, interp_df$informal_index, use = "complete.obs")

# --- 3. Closest years 
#(2000 elections - 2001 informality
#2004 elections - 2004 informality
#2007 elections - 2007 informality
#2008 elections - 2009 informality
#2012 elections - 2012 informality
#2016 elections - 2015 informality
#2018 elections - 2017 informality
#) ---
closest_mapping <- data.frame(
  election_year = c(2000, 2003, 2004, 2007, 2008, 2011, 2012, 2016, 2018),
  informality_year = c(2001, 2004, 2004, 2007, 2009, 2012, 2012, 2015, 2017)
)

closest_df <- fraud_data %>%
  inner_join(closest_mapping, by = c("year" = "election_year")) %>%
  left_join(informal_long, by = c("region_code", "informality_year" = "year"))

closest_corr <- cor.test(closest_df$fraud_index, closest_df$informal_index, use = "complete.obs")

# --- Collect results ---
correlation_results <- tibble::tibble(
  Approach = c("Matched years (2004, 2007, 2012)", "Interpolated", "Closest-year"),
  Pearson_r = c(matched_corr$estimate, interp_corr$estimate, closest_corr$estimate),
  p_value = c(matched_corr$p.value, interp_corr$p.value, closest_corr$p.value)
)

# Make a table
correlation_results_styled <- correlation_results %>%
  mutate(
    Pearson_r = round(Pearson_r, 3),
    significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    p_display = ifelse(p_value < 0.001, "< 0.001", sprintf("%.3f", p_value))
  ) %>%
  select(
    Method = Approach,
    `Correlation (r)` = Pearson_r,
    `p-value` = p_display,
    Significance = significance
  ) %>%
  gt() %>%
  tab_header(
    title = "Correlation Between Informal Employment and Electoral Fraud",
  ) %>%
  cols_label(
    `Correlation (r)` = "Correlation",
    `p-value` = "p-value",
    Significance = ""
  ) %>%
  tab_source_note(md("*Significance codes: *** < 0.001, ** < 0.01, * < 0.05*"))

correlation_results_styled

rm(correlation_results, fraud_data, informal_long, interp_df, interp_corr, interpolated_informality, matched_corr, matched_df, region_years)
rm(closest_corr, closest_df, closest_mapping, correlation_results_styled)
rm(election_years, overlap_years)
rm(informal_employment)
