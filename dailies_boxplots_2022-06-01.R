#"Dailies_boxplots_2022-06-01"

#By: Sara DeLaurentis

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



library(lubridate)
library(stringr)
library(tidyverse)
#library(cowplot)
library(stringr)
library(dplyr)
library(glue) #from dplyr
library(RColorBrewer)


setwd("C:/Users/sbaue/coding/R_TEMPRY/Itasca_project_19-21") #wd for Sara's desktop
setwd("C:/Users/sbaue/coding/Itasca_project_19-21") #wd for Sara's laptop

temps1 <- read.csv("ALL.csv") #Giant .csv (see notes above)
head(temps1)
temps1.NA <- temps1[is.na(temps1$value),] #check for correct version of "ALL.csv". It should have around 20 rows with NA in $value
#view(temps1.NA)


## CLEANING ##

#on-the-fly editing of m01surf and m02surf:
temps1$position[temps1$position == "m01surf"] <- "lsurf" #where $position == "m01surf", replace with "lsurf"
temps1$position[temps1$position == "m02surf"] <- "m0surf" #similar to above.

#checks: have they all been renamed?
#head(temps1[grep("C5A_R1_lsurf", temps1$name),])
#head(temps1[grep("C5A_R3_m01surf", temps$name),])
#head(temps1[grep("C5A_R3_m02surf", temps$name),])

#remove unk_unk_unk... files on the fly. One day I'll remove these from ALL.csv...
str(temps1[grep("unk", temps1$name, ignore.case = TRUE),]) #find the "unk" files
temps1 <- temps1[-(grep("unk", temps1$name, ignore.case = TRUE)),] #delete them
str(temps1)

#remove D2B R3 lsurf i90 2021 on the fly (compromised temp sensor). I'll remove this from ALL.csv, too...
str(temps1[grep("D2B_R3_lsurf", temps1$name, ignore.case = TRUE),]) #find
temps1 <- temps1[-(grep("D2B_R3_lsurf_i90_2021", temps1$name)),] #
str(temps1)

#remove high pre-install temps from November 2019 on the fly. This will need to stay here.
str(temps1[(temps1$date.time <= as.POSIXct("2019-11-30") & temps1$value > 15),]) #find
#view(temps1[(temps1$date.time <= as.POSIXct("2019-11-30") & temps1$value > 15),]) #view

temps1 <- temps1[!(temps1$date.time < as.POSIXct("2019-11-30") & (temps1$value > 15)),] #destroy. temps1 "does not equal" (!) the rows where these two criteria are met.
head(temps1)
str(temps1)

#create temps.2 and temps.s (takes some time to run)
#temps2 <- group temps1 by name and date, calculate date, meantemp, max, min.
#temps.s (s for summarized) <- only keep rows of distinct date and name, then calculate difference and amplitude.
#   Then, remove unnecessary columns.
#   Then, add a treatment (C2, D2, C5, D5) column.
temps2 <- temps1 %>% group_by(name, date(as.POSIXct(date.time))) %>%
  mutate(date = date(as.POSIXct(date.time)), 
            meantemp = mean(value), 
            max = max(value), 
            min = min(value))%>% 
  ungroup()
temps.s <- temps2 %>% 
  distinct(date,name, .keep_all = TRUE) %>% 
  mutate(difference = max - min, amp = abs(max - min)/2)

temps.s[ ,c("X", "unit", "value", "date.time", "date(as.POSIXct(date.time))")] <- NULL #remove those excess columns!
temps.s[,"treatment"] <- substring(temps.s$site, 1,2) #Add treatment column (C2, D2, C5, D5). Create from a substring (1st two elements of "site")

head(temps.s)
str(temps.s)

#checks: Are those high November 2019 temps gone??
#Nov.19 <- temps.s[temps.s$date < as.POSIXct("2019-11-30") & temps.s$max > 15,]
#Nov.19 <- temps1[temps1$date.time < as.POSIXct("2019-11-30") & temps1$max > 15,]
#view(Nov.19)



### About Boxplot.FUN: ###

#Objective: Create Boxplots of monthly temperature data, where x = site, y = daily max, min, diff, OR amp. Output is one set of depths plotted (faceted) per month.
  #Use function to iterate over two lists of year/month pairs to output plots very quickly. (using map2 or similar)
#Plot characteristics: grayscale, jitter added to retain data spread information, png output is 1100 x 450 px as of 2022-06-08.

#Inputs: 
    # year - YYYY - numeric or character, 
    # month - mm - character, with leading zeroes for single-digit
    # the main dataframe (data <- temps.s) is included in the function and queried using the month and year.

#NOTES: 
  #Inputs, year and month, are used to determine the date range (glued together and used with grep), plot name, file name

  #I've created separate folders for each y-variable, and I adjust the function by hand after performing each one. 
    #For different y-variables (max, min, diff, amp), you'll need to adjust some things by hand. See below.



### Beginning of ggplot function ###

Boxplot.FUN <- function(year, month){
  
  month.name <- month.name[[as.numeric(month)]] #select the month name that corresponds to the month number. month.name is a list of month names in English contained in R.
   #our function input month needs to be converted to numeric so it can be used to select the right list element.
  
  png(file = glue("./AVG_DAILY_AMP/dailyamp_{month.name}_{year}.png"), width = 1100, height = 450)

 data <- temps.s[grep(glue("{year}-{month}-"), temps.s$date),] #select based on whether the date contains the string made by gluing the year and month together.
 data <- data %>% filter(max < 70 & min > -40) #get rid of any remaining extreme outliers
 
 #SWAPS FOR DIFFERENT STATS
 # png file name (above), as needed
 # y variable: maximum = max, minimum = min, difference = difference, amplitude = amp
 # y label and title: change as needed.
 
f <- ggplot(data, aes(site, amp, fill = treatment)) 
myplot <- f + geom_boxplot(outlier.shape = NA) +
  facet_wrap(vars(position), scales = "free_y") +
  stat_summary(fun = "mean", shape = 18, color = "Black", show.legend = FALSE) +
  geom_jitter(size = 0.5, alpha = 0.5) +
  labs(x = "Site", y = expression(paste("Daily Amplitude (", degree~ C, ")")), #SOME NIFTY NOTATION FOR ADDING SUPERSCRIPT DEGREE SIGN
       title = glue("{month.name} {year} Daily Amplitude (Max-Min)/2"), 
       fill = "Treatment") +
  scale_fill_grey(start = 0.95, end = 0.3)
#display.brewer.all(colorblindFriendly = TRUE)

print(myplot)
dev.off() #turn plotting device off. Or else it won't work.

}  

###function end.

#input creation:

month.seq <- seq.Date(my("11-2019"), my("10-2021"), by = "month") #create sequence of months b/t start and end dates.

input.df <- tibble(year = year(month.seq), month = sprintf("%02d", month(month.seq))) #create tibble out of separate elements, use sprintf to include leading zero.

view(input.df)

#Test one month:
Boxplot.FUN(input.df[1,1], input.df[1,2])



#### map2 over input.df (ALL MONTHS IN DATA SET) ###
map2(input.df$year, input.df$month, Boxplot.FUN)




#MISC TESTING:

test.string <- as.POSIXct(glue("{input.df[1,1]}-{input.df[1,2]}-01"))-ddays(1)


month

date2 <- as.POSIXct(as.character(glue("{year}-{month+1}-01")), format = "%Y-%m-%d")-ddays(1)
year
May <- temps.s %>%  filter(grepl(glue("{year}-{month}-")), temps.s$date)

May <- temps.s[grep("2020-05-", temps.s$date),]

#Here's something that works (for filtering rows with desired month):
month <- as.character("05")
year <- as.numeric(2020)
May <- temps.s[grep(glue("{year}-{month}-"), temps.s$date),]

May
date2

month.name[[4]]

