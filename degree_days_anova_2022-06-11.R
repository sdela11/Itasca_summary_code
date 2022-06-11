#Title: Degree-day (by trapezoids) ANOVA
#By: Sara DeLaurentis
#Created: 2022-06-11
#Purpose: Run statistical analysis on data calculated by degree-hr trapezoids.
# ... so I can write something nice in my results section.

library(tidyverse)
library(dplyr)
library(stringr)
library(lme4)
library(ggplot2)

setwd("C:/Users/sbaue/coding/R_TEMPRY/Itasca_project_19-21")

data1 <- read.csv("degree_hours_OCT_v1.csv")

view(data1)
