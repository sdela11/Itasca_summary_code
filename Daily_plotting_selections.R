#Title: dailies_plotting_selections

#Description: selection of data and running of functions for plotting dailies
#Date created: 05/04/2021
#By: Sara DeLaurentis
#Project: Itasca 2019-2021

#Notes:

library(tidyverse)
library(viridis)
library(hrbrthemes)


dailydata <- read.csv("merged_dailies.csv")
dailydata$longdate <- as.Date(dailydata$longdate, format = "%m/%d/%Y") #change longdate column to correct format

head(dailydata)

#selection examples and setup:
C2A_R0_air <- filter(dailydata, labels == "C2A_R0_air_i106_2020")
C2A_R1_lsurf <- filter(dailydata, labels == "C2A_R1_lsurf_i34")

#use this code to create your "set"
set <- dailydata %>% 
  filter(labels %in% c("C2A_R0_air_i106_2020", "C2A_R1_lsurf_i34_2020"))


print(set)

#ggplotFUN:
#set = selected data
#title = "graph title"
#pngname = "_____.png"
#caption = "insert notes here"

view(set)

## Plots for profiles of average temperatures

#C2A
set <- C2A_R1

C2A_R1 <- dailydata %>% 
  filter(treatment %in% c("C2A")) %>% 
  filter(rep %in% c("R0", "R1"))

C2A_R2 <- dailydata %>% 
  filter(treatment %in% c("C2A")) %>% 
  filter(rep %in% c("R0", "R2"))

C2A_R3 <- dailydata %>% 
  filter(treatment %in% c("C2A")) %>% 
  filter(rep %in% c("R0", "R3"))

#C2B
C2B_R1 <- dailydata %>% 
  filter(treatment %in% c("C2B")) %>% 
  filter(rep %in% c("R0", "R1"))

C2B_R2 <- dailydata %>% 
  filter(treatment %in% c("C2B")) %>% 
  filter(rep %in% c("R0", "R2"))

C2B_R3 <- dailydata %>% 
  filter(treatment %in% c("C2B")) %>% 
  filter(rep %in% c("R0", "R3"))

#C5A
C5A_R1 <- dailydata %>% 
  filter(treatment %in% c("C5A")) %>% 
  filter(rep %in% c("R0", "R1"))

C5A_R2 <- dailydata %>% 
  filter(treatment %in% c("C5A")) %>% 
  filter(rep %in% c("R0", "R2"))

C5A_R3 <- dailydata %>% 
  filter(treatment %in% c("C5A")) %>% 
  filter(rep %in% c("R0", "R3"))

#C5B
C5B_R1 <- dailydata %>% 
  filter(treatment %in% c("C5B")) %>% 
  filter(rep %in% c("R0", "R1"))

C5B_R2 <- dailydata %>% 
  filter(treatment %in% c("C5B")) %>% 
  filter(rep %in% c("R0", "R2"))

C5B_R3 <- dailydata %>% 
  filter(treatment %in% c("C5B")) %>% 
  filter(rep %in% c("R0", "R3"))

#D2A
D2A_R1 <- dailydata %>% 
  filter(treatment %in% c("D2A")) %>% 
  filter(rep %in% c("R0", "R1"))

D2A_R2 <- dailydata %>% 
  filter(treatment %in% c("D2A")) %>% 
  filter(rep %in% c("R0", "R2"))

D2A_R3 <- dailydata %>% 
  filter(treatment %in% c("D2A")) %>% 
  filter(rep %in% c("R0", "R3"))

#D2B
D2B_R1 <- dailydata %>% 
  filter(treatment %in% c("D2B")) %>% 
  filter(rep %in% c("R0", "R1"))

D2B_R2 <- dailydata %>% 
  filter(treatment %in% c("D2B")) %>% 
  filter(rep %in% c("R0", "R2"))

D2B_R3 <- dailydata %>% 
  filter(treatment %in% c("D2B")) %>% 
  filter(rep %in% c("R0", "R3"))

#D5A
D5A_R1 <- dailydata %>% 
  filter(treatment %in% c("D5A")) %>% 
  filter(rep %in% c("R0", "R1"))

D5A_R2 <- dailydata %>% 
  filter(treatment %in% c("D5A")) %>% 
  filter(rep %in% c("R0", "R2"))

D5A_R3 <- dailydata %>% 
  filter(treatment %in% c("D5A")) %>% 
  filter(rep %in% c("R0", "R3"))

#D5B
D5B_R1 <- dailydata %>% 
  filter(treatment %in% c("D5B")) %>% 
  filter(rep %in% c("R0", "R1"))

D5B_R2 <- dailydata %>% 
  filter(treatment %in% c("D5B")) %>% 
  filter(rep %in% c("R0", "R2"))

D5B_R3 <- dailydata %>% 
  filter(treatment %in% c("D5B")) %>% 
  filter(rep %in% c("R0", "R3"))

head(D5A_R3)


D5A_R1

#function run

#ggplotFUN:
#set = selected data
#title = "graph title"
#pngname = "_____.png"
#caption = "insert notes here"

ggplotFUN.dAMP(C2A_R1, "color test", "colortest.png")
dev.off()

#Real plotting for daily averages, full season:

ggplotFUN(C2A_R1, "C2A R1 daily means", "C2A_R1_daily_mean_.png")
dev.off()

ggplotFUN(C2A_R2, "C2A R2 daily means", "C2A_R2_daily_mean.png")
dev.off()

ggplotFUN(C2A_R3, "C2A R3 daily means", "C2A_R3_daily_mean.png")
dev.off()

ggplotFUN(C2B_R1, "C2B R1 daily means", "C2B_R1_daily_mean.png")
dev.off()

ggplotFUN(C2B_R2, "C2B R2 daily means", "C2B_R2_daily_mean.png")
dev.off()

ggplotFUN(C2B_R3, "C2B R3 daily means", "C2B_R3_daily_mean.png")
dev.off()

ggplotFUN(C5A_R1, "C5A R1 daily means", "C5A_R1_daily_mean.png")
dev.off()

ggplotFUN(C5A_R2, "C5A R2 daily means", "C5A_R2_daily_mean.png")
dev.off()

ggplotFUN(C5A_R3, "C5A R3 daily means", "C5A_R3_daily_mean.png")
dev.off()

ggplotFUN(C5B_R1, "C5B R1 daily means", "C5B_R1_daily_mean.png")
dev.off()

ggplotFUN(C5B_R2, "C5B R2 daily means", "C5B_R2_daily_mean.png")
dev.off()

ggplotFUN(C5B_R3, "C5B R3 daily means", "C5B_R3_daily_mean.png")
dev.off()

ggplotFUN(C2A_R1, "D2A R1 daily means", "D2A_R1_daily_mean.png")
dev.off()

ggplotFUN(D2A_R2, "D2A R2 daily means", "D2A_R2_daily_mean.png")
dev.off()

ggplotFUN(D2A_R3, "D2A R3 daily means", "D2A_R3_daily_mean.png")
dev.off()

ggplotFUN(D2B_R1, "D2B R1 daily means", "D2B_R1_daily_mean.png")
dev.off()

ggplotFUN(D2B_R2, "D2B R2 daily means", "D2B_R2_daily_mean.png")
dev.off()

ggplotFUN(D2B_R3, "D2B R3 daily means", "D2B_R3_daily_mean.png")
dev.off()

ggplotFUN(D5A_R1, "D5A R1 daily means", "D5A_R1_daily_mean.png")
dev.off()

ggplotFUN(D5A_R2, "D5A R2 daily means", "D5A_R2_daily_mean.png")
dev.off()

ggplotFUN(D5A_R3, "D5A R3 daily means", "D5A_R3_daily_mean.png")
dev.off()

ggplotFUN(D5B_R1, "D5B R1 daily means", "D5B_R1_daily_mean.png")
dev.off()

ggplotFUN(D5B_R2, "D5B R2 daily means", "D5B_R2_daily_mean.png")
dev.off()

ggplotFUN(D5B_R3, "D5B R3 daily means", "D5B_R3_daily_mean.png")
dev.off()



##daily MAXIMUMS

ggplotFUN.dMAX(C2A_R1, "C2A R1 daily maximums", "C2A_R1_daily_max.png")
dev.off()

ggplotFUN.dMAX(C2A_R2, "C2A R2 daily maximums", "C2A_R2_daily_max.png")
dev.off()

ggplotFUN.dMAX(C2A_R3, "C2A R3 daily maximums", "C2A_R3_daily_max.png")
dev.off()

ggplotFUN.dMAX(C2B_R1, "C2B R1 daily maximums", "C2B_R1_daily_max.png")
dev.off()

ggplotFUN.dMAX(C2B_R2, "C2B R2 daily maximums", "C2B_R2_daily_max.png")
dev.off()

ggplotFUN.dMAX(C2B_R3, "C2B R3 daily maximums", "C2B_R3_daily_max.png")
dev.off()

ggplotFUN.dMAX(C5A_R1, "C5A R1 daily maximums", "C5A_R1_daily_max.png")
dev.off()

ggplotFUN.dMAX(C5A_R2, "C5A R2 daily maximums", "C5A_R2_daily_max.png")
dev.off()

ggplotFUN.dMAX(C5A_R3, "C5A R3 daily maximums", "C5A_R3_daily_max.png")
dev.off()

ggplotFUN.dMAX(C5B_R1, "C5B R1 daily maximums", "C5B_R1_daily_max.png")
dev.off()

ggplotFUN.dMAX(C5B_R2, "C5B R2 daily maximums", "C5B_R2_daily_max.png")
dev.off()

ggplotFUN.dMAX(C5B_R3, "C5B R3 daily maximums", "C5B_R3_daily_max.png")
dev.off()

ggplotFUN.dMAX(C2A_R1, "D2A R1 daily maximums", "D2A_R1_daily_max.png")
dev.off()

ggplotFUN.dMAX(D2A_R2, "D2A R2 daily maximums", "D2A_R2_daily_max.png")
dev.off()

ggplotFUN.dMAX(D2A_R3, "D2A R3 daily maximums", "D2A_R3_daily_max.png")
dev.off()

ggplotFUN.dMAX(D2B_R1, "D2B R1 daily maximums", "D2B_R1_daily_max.png")
dev.off()

ggplotFUN.dMAX(D2B_R2, "D2B R2 daily maximums", "D2B_R2_daily_max.png")
dev.off()

ggplotFUN.dMAX(D2B_R3, "D2B R3 daily maximums", "D2B_R3_daily_max.png")
dev.off()

ggplotFUN.dMAX(D5A_R1, "D5A R1 daily maximums", "D5A_R1_daily_max.png")
dev.off()

ggplotFUN.dMAX(D5A_R2, "D5A R2 daily maximums", "D5A_R2_daily_max.png")
dev.off()

ggplotFUN.dMAX(D5A_R3, "D5A R3 daily maximums", "D5A_R3_daily_max.png")
dev.off()

ggplotFUN.dMAX(D5B_R1, "D5B R1 daily maximums", "D5B_R1_daily_max.png")
dev.off()

ggplotFUN.dMAX(D5B_R2, "D5B R2 daily maximums", "D5B_R2_daily_max.png")
dev.off()

ggplotFUN.dMAX(D5B_R3, "D5B R3 daily maximums", "D5B_R3_daily_max.png")
dev.off()



#Daily MINIMUMS

ggplotFUN.dMIN(C2A_R1, "C2A R1 daily MINimums", "C2A_R1_daily_min.png")
dev.off()

ggplotFUN.dMIN(C2A_R2, "C2A R2 daily MINimums", "C2A_R2_daily_min.png")
dev.off()

ggplotFUN.dMIN(C2A_R3, "C2A R3 daily MINimums", "C2A_R3_daily_min.png")
dev.off()

ggplotFUN.dMIN(C2B_R1, "C2B R1 daily MINimums", "C2B_R1_daily_min.png")
dev.off()

ggplotFUN.dMIN(C2B_R2, "C2B R2 daily MINimums", "C2B_R2_daily_min.png")
dev.off()

ggplotFUN.dMIN(C2B_R3, "C2B R3 daily MINimums", "C2B_R3_daily_min.png")
dev.off()

ggplotFUN.dMIN(C5A_R1, "C5A R1 daily MINimums", "C5A_R1_daily_min.png")
dev.off()

ggplotFUN.dMIN(C5A_R2, "C5A R2 daily MINimums", "C5A_R2_daily_min.png")
dev.off()

ggplotFUN.dMIN(C5A_R3, "C5A R3 daily MINimums", "C5A_R3_daily_min.png")
dev.off()

ggplotFUN.dMIN(C5B_R1, "C5B R1 daily MINimums", "C5B_R1_daily_min.png")
dev.off()

ggplotFUN.dMIN(C5B_R2, "C5B R2 daily MINimums", "C5B_R2_daily_min.png")
dev.off()

ggplotFUN.dMIN(C5B_R3, "C5B R3 daily MINimums", "C5B_R3_daily_min.png")
dev.off()

ggplotFUN.dMIN(C2A_R1, "D2A R1 daily MINimums", "D2A_R1_daily_min.png")
dev.off()

ggplotFUN.dMIN(D2A_R2, "D2A R2 daily MINimums", "D2A_R2_daily_min.png")
dev.off()

ggplotFUN.dMIN(D2A_R3, "D2A R3 daily MINimums", "D2A_R3_daily_min.png")
dev.off()

ggplotFUN.dMIN(D2B_R1, "D2B R1 daily MINimums", "D2B_R1_daily_min.png")
dev.off()

ggplotFUN.dMIN(D2B_R2, "D2B R2 daily MINimums", "D2B_R2_daily_min.png")
dev.off()

ggplotFUN.dMIN(D2B_R3, "D2B R3 daily MINimums", "D2B_R3_daily_min.png")
dev.off()

ggplotFUN.dMIN(D5A_R1, "D5A R1 daily MINimums", "D5A_R1_daily_min.png")
dev.off()

ggplotFUN.dMIN(D5A_R2, "D5A R2 daily MINimums", "D5A_R2_daily_min.png")
dev.off()

ggplotFUN.dMIN(D5A_R3, "D5A R3 daily MINimums", "D5A_R3_daily_min.png")
dev.off()

ggplotFUN.dMIN(D5B_R1, "D5B R1 daily MINimums", "D5B_R1_daily_min.png")
dev.off()

ggplotFUN.dMIN(D5B_R2, "D5B R2 daily MINimums", "D5B_R2_daily_min.png")
dev.off()

ggplotFUN.dMIN(D5B_R3, "D5B R3 daily MINimums", "D5B_R3_daily_min.png")
dev.off()



#Plotting daily amplitudes

ggplotFUN.dAMP(C2A_R1, "C2A R1 daily Amplitudes", "C2A_R1_daily_amp.png")
dev.off()

ggplotFUN.dAMP(C2A_R2, "C2A R2 daily Amplitudes", "C2A_R2_daily_amp.png")
dev.off()

ggplotFUN.dAMP(C2A_R3, "C2A R3 daily Amplitudes", "C2A_R3_daily_amp.png")
dev.off()

ggplotFUN.dAMP(C2B_R1, "C2B R1 daily Amplitudes", "C2B_R1_daily_amp.png")
dev.off()

ggplotFUN.dAMP(C2B_R2, "C2B R2 daily Amplitudes", "C2B_R2_daily_amp.png")
dev.off()

ggplotFUN.dAMP(C2B_R3, "C2B R3 daily Amplitudes", "C2B_R3_daily_amp.png")
dev.off()

ggplotFUN.dAMP(C5A_R1, "C5A R1 daily Amplitudes", "C5A_R1_daily_amp.png")
dev.off()

ggplotFUN.dAMP(C5A_R2, "C5A R2 daily Amplitudes", "C5A_R2_daily_amp.png")
dev.off()

ggplotFUN.dAMP(C5A_R3, "C5A R3 daily Amplitudes", "C5A_R3_daily_amp.png")
dev.off()

ggplotFUN.dAMP(C5B_R1, "C5B R1 daily Amplitudes", "C5B_R1_daily_amp.png")
dev.off()

ggplotFUN.dAMP(C5B_R2, "C5B R2 daily Amplitudes", "C5B_R2_daily_amp.png")
dev.off()

ggplotFUN.dAMP(C5B_R3, "C5B R3 daily Amplitudes", "C5B_R3_daily_amp.png")
dev.off()

ggplotFUN.dAMP(C2A_R1, "D2A R1 daily Amplitudes", "D2A_R1_daily_amp.png")
dev.off()

ggplotFUN.dAMP(D2A_R2, "D2A R2 daily Amplitudes", "D2A_R2_daily_amp.png")
dev.off()

ggplotFUN.dAMP(D2A_R3, "D2A R3 daily Amplitudes", "D2A_R3_daily_amp.png")
dev.off()

ggplotFUN.dAMP(D2B_R1, "D2B R1 daily Amplitudes", "D2B_R1_daily_amp.png")
dev.off()

ggplotFUN.dAMP(D2B_R2, "D2B R2 daily Amplitudes", "D2B_R2_daily_amp.png")
dev.off()

ggplotFUN.dAMP(D2B_R3, "D2B R3 daily Amplitudes", "D2B_R3_daily_amp.png")
dev.off()

ggplotFUN.dAMP(D5A_R1, "D5A R1 daily Amplitudes", "D5A_R1_daily_amp.png")
dev.off()

ggplotFUN.dAMP(D5A_R2, "D5A R2 daily Amplitudes", "D5A_R2_daily_amp.png")
dev.off()

ggplotFUN.dAMP(D5A_R3, "D5A R3 daily Amplitudes", "D5A_R3_daily_amp.png")
dev.off()

ggplotFUN.dAMP(D5B_R1, "D5B R1 daily Amplitudes", "D5B_R1_daily_amp.png")
dev.off()

ggplotFUN.dAMP(D5B_R2, "D5B R2 daily Amplitudes", "D5B_R2_daily_amp.png")
dev.off()

ggplotFUN.dAMP(D5B_R3, "D5B R3 daily Amplitudes", "D5B_R3_daily_amp.png")
dev.off()


#Plotting daily difference (MAX-MIN)

ggplotFUN.dDIFF(C2A_R1, "C2A R1 daily Difference", "C2A_R1_daily_diff.png")
dev.off()

ggplotFUN.dDIFF(C2A_R2, "C2A R2 daily Difference", "C2A_R2_daily_diff.png")
dev.off()

ggplotFUN.dDIFF(C2A_R3, "C2A R3 daily Difference", "C2A_R3_daily_diff.png")
dev.off()

ggplotFUN.dDIFF(C2B_R1, "C2B R1 daily Difference", "C2B_R1_daily_diff.png")
dev.off()

ggplotFUN.dDIFF(C2B_R2, "C2B R2 daily Difference", "C2B_R2_daily_diff.png")
dev.off()

ggplotFUN.dDIFF(C2B_R3, "C2B R3 daily Difference", "C2B_R3_daily_diff.png")
dev.off()

ggplotFUN.dDIFF(C5A_R1, "C5A R1 daily Difference", "C5A_R1_daily_diff.png")
dev.off()

ggplotFUN.dDIFF(C5A_R2, "C5A R2 daily Difference", "C5A_R2_daily_diff.png")
dev.off()

ggplotFUN.dDIFF(C5A_R3, "C5A R3 daily Difference", "C5A_R3_daily_diff.png")
dev.off()

ggplotFUN.dDIFF(C5B_R1, "C5B R1 daily Difference", "C5B_R1_daily_diff.png")
dev.off()

ggplotFUN.dDIFF(C5B_R2, "C5B R2 daily Difference", "C5B_R2_daily_diff.png")
dev.off()

ggplotFUN.dDIFF(C5B_R3, "C5B R3 daily Difference", "C5B_R3_daily_diff.png")
dev.off()

ggplotFUN.dDIFF(C2A_R1, "D2A R1 daily Difference", "D2A_R1_daily_diff.png")
dev.off()

ggplotFUN.dDIFF(D2A_R2, "D2A R2 daily Difference", "D2A_R2_daily_diff.png")
dev.off()

ggplotFUN.dDIFF(D2A_R3, "D2A R3 daily Difference", "D2A_R3_daily_diff.png")
dev.off()

ggplotFUN.dDIFF(D2B_R1, "D2B R1 daily Difference", "D2B_R1_daily_diff.png")
dev.off()

ggplotFUN.dDIFF(D2B_R2, "D2B R2 daily Difference", "D2B_R2_daily_diff.png")
dev.off()

ggplotFUN.dDIFF(D2B_R3, "D2B R3 daily Difference", "D2B_R3_daily_diff.png")
dev.off()

ggplotFUN.dDIFF(D5A_R1, "D5A R1 daily Difference", "D5A_R1_daily_diff.png")
dev.off()

ggplotFUN.dDIFF(D5A_R2, "D5A R2 daily Difference", "D5A_R2_daily_diff.png")
dev.off()

ggplotFUN.dDIFF(D5A_R3, "D5A R3 daily Difference", "D5A_R3_daily_diff.png")
dev.off()

ggplotFUN.dDIFF(D5B_R1, "D5B R1 daily Difference", "D5B_R1_daily_diff.png")
dev.off()

ggplotFUN.dDIFF(D5B_R2, "D5B R2 daily Difference", "D5B_R2_daily_diff.png")
dev.off()

ggplotFUN.dDIFF(D5B_R3, "D5B R3 daily Difference", "D5B_R3_daily_diff.png")
dev.off()