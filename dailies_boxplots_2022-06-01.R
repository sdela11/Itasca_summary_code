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
temps1.NA <- temps1[is.na(temps1$value),] #check for correct version of "ALL.csv"
view(temps1.NA)

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
Boxplot.FUN <- function(year, month){
  
  month.name <- month.name[[as.numeric(month)]] #select the month name that corresponds to the month number. month.name is a constant list of month names in English.
  
  png(file = glue("dailymax_{month.name}_{year}.png"), width = 1100, height = 450)
  
  date1 <- as.POSIXct(as.character(glue("{year}-{month}-01")), format = "%Y-%m-%d")
  print(date1)
  
  date2 <- as.POSIXct(as.character(glue("{year}-{as.numeric(month)+1}-01")), format = "%Y-%m-%d")-ddays(1) #To get last day of month: 1st day of next month, subtract 1 day.
  print(date2)
 data <- temps.s %>% 
   filter(date >= date1 & date <= date2) %>% 
   filter(max < 70 & min > -40)

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
dev.off()

}  

###function end.

#input creation:

month.seq <- seq.Date(my("11-2019"), my("11-2021"), by = "month") #create sequence of months b/t start and end dates.

input.df <- tibble(year = year(month.seq), month = sprintf("%02d", month(month.seq))) #create tibble out of separate elements, use sprintf to include leading zero.

view(input.df)
class(input.df$year)
class(input.df$month)

#Test one month:
Boxplot.FUN(input.df[1,1], input.df[1,2])

#map2 over input.df (all months in data set)
map2(input.df$year, input.df$month, Boxplot.FUN)

#MISC TESTING:

test.string <- as.POSIXct(glue("{input.df[1,1]}-{input.df[1,2]}-01"))-ddays(1)

month <- as.numeric(05)
month
year <- as.numeric(2020)
date2 <- as.POSIXct(as.character(glue("{year}-{month+1}-01")), format = "%Y-%m-%d")-ddays(1)
date2

month.name[[4]]

