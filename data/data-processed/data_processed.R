## ---- load packages -----------------------------------------------------------

library(tidyverse)
library(vroom)
library(here)


## ---- construct meta data -----------------------------------------------------

meta_data <- tibble(
  # Paths to all ride files.
  file_path = list.files(here("data/data-raw"), full.names = TRUE)
) %>%
  
  # extract variables from file path.
  separate(file_path,
    into = c("date", "time", "source"),
    sep ="_", remove = FALSE
  ) %>%
  
  # tidy formatting of variables.
  mutate(
    date = str_remove(date, ".*/data-raw/") %>% as.Date(),
    time = str_replace_all(time, "x", ":"),
    source = str_remove(source,".csv"),
    date_time = paste(date, time) %>% as.POSIXct(),
    
    # Identify file structure by date.
    file_str = if_else(
      date <= as.Date("2020-02-08"),
      "short", "long"
    )
  ) %>%
  
  select(file_path, file_str, date, date_time, source)


## ---- read raw data ----------------------------------------------------------

# Read data in two parts depending on file format.
dat <- map(c("short", "long"), function(str){
  str_md <- meta_data %>% filter(file_str == str)
  vroom(str_md$file_path, id = "file_path")
  }) %>%
  setNames(c("short", "long"))

# Bind data, selecting/renaming columns to consistent formats.
dat <- bind_rows(
  dat[["short"]] %>%
    mutate(seconds = round(60 * Minutes)) %>%
    select(file_path, seconds, power = Watts, cadence = Cadence, hr = Hrate),
  
  dat[["long"]] %>% 
    select(file_path, seconds = secs, power = watts, cadence = cad, hr)
)

# Join with meta_data and derive date time.
dat <- left_join(meta_data, dat) %>%
  mutate(
    date_time = date_time + seconds
  ) %>% select(-file_path, -file_str, -seconds)


## ---- heart rate correction --------------------------------------------------
dat <- dat %>%
  mutate(
    hr = ifelse(hr == 0, NA, hr)
  )


## ---- widen data -------------------------------------------------------------

# Make data wide to facilitate comparison of power sources; result is
# a single row for a given date/time with data from both power sources in the 
# same row.
dat <- dat %>%
  pivot_wider(
    id_cols = c("date", "date_time"),
    names_from = source,
    values_from = c("power", "cadence", "hr")
  )


## ---- correct for offset -----------------------------------------------------

# Lag in data capture / misalligned time stamps may mean that we need to
# offset the data from the two sources to ensure they best align. The optimal offset is
# identified by maximising cross covariance.

dat <- purrr::map(unique(meta_data$date), function(dt){
  
  # Get data for the current date; remove any rows with NAs.
  dat_dt <- dat %>%
    filter(as.character(date) == dt) %>%
    filter(!is.na(power_kickr), !is.na(power_4iiii) )
  
  # Calculate cross correlation in a window of +/- 90 seconds.
  ccf_fit <- ccf(dat_dt$power_4iiii, dat_dt$power_kickr, lag = 90, plot = FALSE)
  
  # Identify optimal offset; print result.
  offset <- which.max(ccf_fit$acf) - 91
  cat(dt, ", offset:", offset, "\n")
  
  # Apply offset.
  if(offset < 0)
    dat_dt <- dat_dt %>% mutate(
      power_4iiii = lag(power_4iiii, -offset),
      cadence_4iiii = lag(cadence_4iiii, -offset)
    )
  if(offset > 0)
    dat_dt <- dat_dt %>% mutate(
      power_4iiii = lead(power_4iiii, offset),
      cadence_4iiii = lead(cadence_4iiii, offset)
    )
  
  return(dat_dt)
}) %>% bind_rows()


## ---- write ------------------------------------------------------------------
dat <- dat %>% arrange(date_time)

write_rds(dat, here("./data/data-processed/data_processed.Rda"))

rm(list = ls())
gc()