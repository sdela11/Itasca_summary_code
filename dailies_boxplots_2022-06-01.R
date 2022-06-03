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
temps.NA <- temps1[is.na(temps$value),] #check for correct version of "ALL.csv"
view(temps.NA)

#on-the-fly editing of m01surf and m02surf:
temps1$position[temps1$position == "m01surf"] <- "lsurf"
temps1$position[temps1$position == "m02surf"] <- "m0surf"

#checks
#head(temps1[grep("C5A_R1_lsurf", temps1$name),])
#head(temps1[grep("C5A_R3_m01surf", temps$name),])
#head(temps1[grep("C5A_R3_m02surf", temps$name),])

#remove unk_unk_unk... files on the fly
str(temps1[grep("unk", temps1$name, ignore.case = TRUE),]) #find the "unk" files
temps1 <- temps1[-(grep("unk", temps1$name, ignore.case = TRUE)),] #delete them


#create temps.2 and temps.s
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
  filter(max < 70)

head(May)
str(May)



#Beginning of ggplot function
Boxplot.FUN <- function(month,year){
  
 data <- temps.s %>% 
   filter(date >= as.POSIXct(glue("{year}-{month}-01")) & date <= as.POSIXct("{year}-{month}-31")) %>% 
   filter(max < 70 & min > -40)

 month.name <- month.name(month)

f <- ggplot(data, aes(site, max, fill = treatment)) 
myplot <- f + geom_boxplot(outlier.shape = NA) +
  facet_wrap(vars(position), scales = "free_y") +
  stat_summary(fun = "mean", shape = 18, color = "Black", show.legend = FALSE) +
  geom_jitter(size = 0.5, alpha = 0.5) +
  labs(x = "Site", y = expression(paste("Daily Maximum Temperature (", degree~ C, ")")), 
       title = glue("{month.name} {year} Daily Maximum Temperatures"), 
       fill = "Treatment") +
  scale_fill_grey(start = 0.95, end = 0.3)
#display.brewer.all(colorblindFriendly = TRUE)
print(myplot) 
}  
###function end.

#input creation:

month.seq <- seq.Date(my("11-2019"), my("11-2021"), by = "month")
input.df <- tibble(month = month(month.seq), year = year(month.seq))

map2(input.df$month, input.df$year, Boxplot.FUN)

