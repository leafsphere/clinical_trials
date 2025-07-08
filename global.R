# sourced at the beginning of the Shiny app and loads global variables

library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(sqldf)
library(forcats)
library(DT)

# grabs functions from util.R
source("util.R")

# load datasets, all of which are tables sourced from clinicaltrials.gov database from 10/2023
load("data/conditions.rda")
load("data/countries.rda")
load("data/endpoints.rda")
load("data/studies.rda")

# set maximum number of studies to display
max_num_studies <- 1000
