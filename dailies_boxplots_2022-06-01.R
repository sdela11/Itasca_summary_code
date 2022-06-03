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
library(RColorBrewer)


setwd("C:/Users/sbaue/coding/R_TEMPRY/Itasca_project_19-21")
#setwd("C:/Users/sbaue/coding/Itasca_project_19-21")

temps1 <- read.csv("ALL.csv")
head(temps1)


temps2 <- temps1 %>% group_by(name, date(as.POSIXct(date.time))) %>% 
  mutate(date = date(as.POSIXct(date.time)), 
            meantemp = mean(value), 
            max = max(value), 
            min = min(value)) %>% 
  ungroup()
temps.s <- temps2 %>% 
  distinct(date,name, .keep_all = TRUE)

temps.s[ ,c("X", "unit", "value", "date.time", "date(as.POSIXct(date.time))")] <- NULL #remove those excess columns!
temps.s[,"treatment"] <- substring(temps.s$site, 1,2)

head(temps.s)
str(temps.s)

May <- temps.s %>%  
  filter(date >= as.POSIXct("2020-05-01") & date <= as.POSIXct("2020-05-31")) %>% 
  #filter(position == "m10") %>% 
  filter(max < 70)

head(May)
str(May)

#Beginning of ggplot function

data <- May #set data
f <- ggplot(data, aes(site, max, fill = treatment))
myplot <- f + geom_boxplot() +
  facet_wrap(vars(position)) +
  stat_summary(fun = "mean", shape = 18, color = "Black", show.legend = FALSE) +
  labs(x = "Site", y = expression(paste("Daily Maximum Temperature (", degree~ C, ")")), 
       title = "May 2021 Daily Maximum Temperatures", 
       fill = "Treatment") +
  scale_fill_grey(start = 0.95, end = 0.3)
#display.brewer.all(colorblindFriendly = TRUE)
print(myplot)


df2 <- data.frame(x = 1:5 , y = 1:25, z = 1:25)
p <- ggplot(df2, aes(x, y))
p <- p + geom_point(aes(shape = z), size = 4) +
  scale_shape_identity()
print(p)

