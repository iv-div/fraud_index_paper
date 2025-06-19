# This code downloads data from Kobak's GitHub, renames columns appropriately and aggregates all data into one dataframe
setwd("") #path to your folder

# Step 0. Libraries
library(dplyr)
library(ggplot2)
library(readr)
library(utils)
library(httr)

# Setting years and github address
years <- c(2000, 2003, 2004, 2007, 2008, 2011, 2012, 2016, 2018)

base_url <- "https://raw.githubusercontent.com/dkobak/elections/master/data/" # Kobak's GitHub

# Temporary folder for unzipping
temp_dir <- tempdir()

# Download, unzip, upload each year's data into a separate dataframe
for (year in years) {
  zip_url <- paste0(base_url, year, ".csv.zip")
  zip_path <- file.path(temp_dir, paste0(year, ".zip"))
  csv_path <- file.path(temp_dir, paste0(year, ".csv"))
  
  # Downloading zip
  GET(zip_url, write_disk(zip_path, overwrite = TRUE))
  
  # unzipping CSV
  unzip(zip_path, exdir = temp_dir)
  
  # reading CSV
  df <- read.csv(csv_path, stringsAsFactors = FALSE)
  
  # saving as df_Year
  assign(paste0("df_", year), df)
}


col_match <- function(pattern, cols, exact_prefix = FALSE) {
  if (exact_prefix) grep(paste0("^", pattern), cols, value = TRUE)
  else grep(pattern, cols, value = TRUE, ignore.case = TRUE)
}

all_data <- list()

# choose necessary columns in each year's dataframe. 
for (year in years) {
  df <- get(paste0("df_", year))
  cols <- colnames(df)
  
  col_region     <- col_match("^region$", cols)
  col_tik        <- col_match("^tik$", cols)
  col_uik        <- col_match("^uik$", cols)
  col_voters     <- col_match("включ.*спис|внес.*спис|включ.*избирател", cols)
  col_valid      <- col_match("Число.действительных", cols, exact_prefix = TRUE)
  col_invalid    <- if (year == 2000) "Общее.число.недействительных.избирательных.бюллетеней"
  else col_match("Число.недействительных", cols, exact_prefix = TRUE)
  
  # Define as "incumbent" an appropriate candidate and party: Putin for 2000, 2004, 2012, 2018, 
  # United Russia for 2003, 2007, 2011, 2016, Medvedev for 2008
  
  col_incumbent  <- switch(as.character(year),
                           "2000" = "X7..ПУТИН.Владимир.Владимирович",
                           "2003" = "X20...Политическая.партия..Единая.Россия.",
                           "2004" = "Путин.Владимир.Владимирович",
                           "2007" = "X10.Всероссийская.политическая.партия..ЕДИНАЯ.РОССИЯ.",
                           "2008" = "Медведев.Дмитрий.Анатольевич",
                           "2011" = "X6..Всероссийская.политическая.партия..ЕДИНАЯ.РОССИЯ.",
                           "2012" = "Путин.Владимир.Владимирович",
                           "2016" = "X4..Всероссийская.политическая.партия..ЕДИНАЯ.РОССИЯ.",
                           "2018" = "Путин.Владимир.Владимирович"
  )
  
  if (anyNA(c(col_region, col_tik, col_uik, col_voters, col_invalid, col_valid, col_incumbent))) {
    warning(paste("Missing columns", year, "- missing file"))
    next
  }
  
  all_data[[as.character(year)]] <- data.frame(
    year,
    region = df[[col_region]],
    tik = df[[col_tik]],
    uik = df[[col_uik]],
    registered_voters = df[[col_voters]],
    invalid_ballots = df[[col_invalid]],
    valid_ballots = df[[col_valid]],
    votes_for_incumbent = df[[col_incumbent]],
    stringsAsFactors = FALSE
  )
}

# Adding all data into one working dataframe
precinct_df <- do.call(rbind, all_data)

# Adding region codes
matched <- read.csv2("data/region_code_converter.csv", stringsAsFactors = FALSE)
precinct_df <- merge(precinct_df, matched[, c("region", "region_code")], by = "region", all.x = TRUE)


#Deleting all unnecessary dataframes and values
rm(all_data, df, df_2000, df_2003, df_2004, df_2007, df_2008, df_2011, df_2012, df_2016, df_2018, matched)
rm(base_url, col_incumbent, col_invalid, col_region, col_tik, col_uik, col_valid, col_voters, cols, csv_path, temp_dir)
rm(year, zip_path, zip_url, col_match)
