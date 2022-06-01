#"Dailies_boxplots_2022-06-01"

#Cleaner code to summarize dailies and to PLOT them! Uses "ALL.csv", which contains the entire dataset.
#Columns in ALL.csv:

#X (integer)
#date.time (YYYY-MM-DD HH:MM:SS)
#unit (C for Celsius)
#value (temperature in C)
#name (ex: C2A_R0_air_i106_2020)
#site
#rep
#position
#buttonID
#season (YYYY, the year it was downloaded.)

#By: Sara DeLaurentis


library(lubridate)
library(stringr)
library(tidyverse)
#library(cowplot)
library(stringr)
library(dplyr)
library(glue) #from dplyr


#setwd("C:/Users/sbaue/coding/R_TEMPRY/Itasca_project_19-21")
setwd("C:/Users/sbaue/coding/Itasca_project_19-21")

temps <- read.csv("ALL.csv")
head(temps)

temps <- temps %>% group_by(name, date(as.POSIXct(date.time))) %>% 
  mutate(date = date(as.POSIXct(date.time)), 
            meantemp = mean(value), 
            max = max(value), 
            min = min(value)) %>% 
  ungroup()
temps.s <- temps %>% 
  distinct(date,name, .keep_all = TRUE)

temps.s[ ,c("X", "unit", "value", "date.time", "date(as.POSIXct(date.time))")] <- NULL #remove those excess columns!
temps.s[,"treatment"] <- substring(temps.s$site, 1,2)

head(temps.s)
str(temps.s)

May.m10 <- temps.s %>%  
  filter(date >= as.POSIXct("2020-05-01") & date <= as.POSIXct("2020-05-31")) %>% 
  filter(position == "m10")

head(May.m10)
str(May.m10)

data <- May.m10
f <- ggplot(data, aes(site, max), fill = treatment)
myplot <- f + geom_boxplot() +
  labs(title = "May Daily Maximum Temperatures", subtitle = "10cm below mineral surface") #fill = "Treatment"

print(myplot)
