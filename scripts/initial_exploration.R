# Week 1: Initial Data Exploration ====
# Author: [Matt May]
# Date: [30.01.2026]

# Load packages ====
library(tidyverse)
library(here)
library(naniar)
library(janitor)
library(skimr)
# Load data ====
mosquito_egg_raw <- read_csv(here("data", "mosquito_egg_data.csv"),
                             name_repair = janitor::make_clean_names)

# Basic overview ====
glimpse(mosquito_egg_raw)
summary(mosquito_egg_raw)
skim(mosquito_egg_raw)

# React table====
# view interactive table of data
view(mosquito_egg_raw)


# Counts by site and treatment====

mosquito_egg_raw |> 
  group_by(site, treatment) |> 
  summarise(n = n())

# Observations ====
# Your observations (add as comments below):
# - What biological system is this?
#   Observing mosquitoes reproductive system and pesticides effect on population levels.
# - What's being measured?
#   How many eggs hatch out of total laid
# - How many observations?
#   205
# - Anything surprising?
#   Female 23 laid less than hatched
# - Any obvious problems?
#   Missing values in body mass, eggs laid, and eggs hatched

mosquito_egg_raw |>
  distinct(site)

str_c("Site", "B", sep = "_")
str_c("Site", "C", sep = "_")
str_c("Site", "A", sep = "_")

mosquito_egg_raw |>
  distinct(site)

mosquito_egg_raw |>
  distinct(collector)

mosquito_egg_raw |>
  distinct(treatment)

mosquito_egg_clean <- mosquito_egg_raw |>
  mutate(site = case_when(
    site == "Site B" ~ "Site_B",
    site == "Site A" ~ "Site_A",
    site == "Site C" ~ "Site_C",
    site == "Site-C" ~ "Site_C",
    site == "Site-B" ~ "Site_B",
    site == "Site-A" ~ "Site_A",
    site == "site c" ~ "Site_C",
    site == "site b" ~ "Site_B",
    site == "site a" ~ "Site_A",
    .default = as.character(site) 
  )
  ) 

mosquito_egg_clean |>
  distinct(site)

mosquito_egg_clean <- mosquito_egg_raw |> 
  mutate(site = stringr::str_to_upper(site))

mosquito_egg_clean |>
  distinct(site)

mosquito_egg_raw |> 
  filter(duplicated(across(everything())))
sum()

mosquito_egg_raw |> 
  summarise(
    n = n(),
    n_distinct(female_id)
  )

mosquito_egg_raw |> 
  summarise(
    n = n(),
    n_distinct(collection_date)
  )

library(skimr)
skimr::skim(mosquito_egg_raw)

# Check ranges of all numeric variables at once
mosquito_egg_raw |> 
  summarise(across(where(is.numeric), 
                   list(min = ~min(., na.rm = TRUE),
                        max = ~max(., na.rm = TRUE))))            
