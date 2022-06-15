#Title: Degree-day (by trapezoids) ANOVA
#By: Sara DeLaurentis
#Created: 2022-06-11
#Purpose: Run statistical analysis on data calculated by degree-hr trapezoids.
# ... so I can write something nice in my results section.

library(tidyverse)
library(broom)
library(dplyr)
library(stringr)
library(lme4)
library(ggplot2)
library(glue)

setwd("C:/Users/sbaue/coding/R_TEMPRY/Itasca_project_19-21")

data1 <- read.csv("degree_hours_OCT_v1.csv")


#view(data1)

#Step 1:

   #ANOVA on depths at treatment:

data1$treatment <- as.factor(substring(data1$site,1,2)) #create treatment column

head(data1)
view(data1$treatment)
str(data1)
data1$position[data1$position == "m01surf"] <- "lsurf" #where $position == "m01surf", replace with "lsurf"
data1$position[data1$position == "m02surf"] <- "m0surf" #similar to above.
data1 <- data1[!(data1$site == "D2B" & data1$rep == "R3" & data1$position == "lsurf"), ] #UGLY: remove D2B R3 lsurf on the fly.

d1.factors <- c("site", "treatment", "rep", "position")
data1[d1.factors] <- lapply(data1[d1.factors], factor)
data1 <- data1[,c(32,1:31)]
str(data1)
view(data1)

#AIR
#can't do an ANOVA/summary, there's only 1 observation per group.
fm.air <- lm(degree.days ~ site, data = data1[grep("air", data1$position),]) #degree.days as function of site, data consists of all rows with "air" in position column.
anova(fm.air)
summary(fm.air)

plot(degree.days ~ site, data = data1[grep("air", data1$position),] )
#results in a perfect fit, since there's only one measurement per group.

fivenum(data1[grep("air", data1$position), "degree.days"], na.rm = TRUE)
?fivenum()


#LSURF

fm.lsurf.trt <- lm(degree.days ~ treatment, data = data1[grep("lsurf", data1$position),])
anova(fm.lsurf.trt)
summary(fm.lsurf.trt)

#treatment is not significant.

fm.lsurf.site <- lm(degree.days ~ site, data = data1[grep("lsurf", data1$position),])
anova(fm.lsurf.site)
summary(fm.lsurf.site)

#Intercept is C2A (4950), highly significant, D2A is significant (.00106), D2B is very highly significant (0.0352)
#Adjusted R-squared: 0.6585, p-value: 0.003993.

fm.m0surf.trt <- lm(degree.days ~ treatment, data = data1[grep("m0surf", data1$position),])
anova(fm.m0surf.trt)
summary(fm.m0surf.trt)

#significant treatment effect (p = 0.0003256)
#Intercept is C2, very highly significant (4360), C5 is very highly significant, as is D5. C5 and D5 have around 500 days more than C2.
#Adj R-squared: 0.5957, p = 0.0003256

fm.m0surf.site <- lm(degree.days ~ site, data = data1[grep("m0surf", data1$position),])
anova(fm.m0surf.site)
summary(fm.m0surf.site)

#Site has highly significant effect: p = 2.348e-05.
#C2A (<2e-16) est: 4451, C5A (0.000198) very highly sig. (above intercept est.)
#C5B (0.009824) and D5A (0.002762) are highly sig. (above intercept est.)
#D2A (0.0502) is almost sig. (below intercept est.)

#Attempt at developing function. Not in use at the moment.

output.FUN <- function(position){
  output.list <- list()
  position <- as.character(position)
  #print(position)
  #class(position)
  data <- data1
  data <- data1[grep(position, data1$position, ignore.case = TRUE),]
  fm.data <- lm(degree.days ~ treatment, data = data)
  anova <- tidy(anova(fm.data))
  summary <- tidy(summary(fm.data))
  output.list <- c(anova,summary)
  print(output.list)
}

output.FUN("m30")

position <- "m30"
data <- data1[grep(as.character(position), data1$position, ignore.case = TRUE),]
head(data)




