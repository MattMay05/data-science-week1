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