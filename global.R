library(shiny)
library(shinydashboard)

# LIBRARY

library(scales)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(glue)

# READ DATA

results <- read.csv("results.csv")

# DATA CLEANSING

results <- results %>% 
  
  # CHANGE DATA TYPE
  
  mutate(
  home_team = as.factor(home_team),
  away_team = as.factor(away_team),
  result = as.factor(result),
  season = as.factor(season)
  
)