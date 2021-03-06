#Playing with trapezoids
#Sara DeLaurentis
#2022-05-24

#Stuff I'm trying:
#Create a line between two points.
#Solve for x = 0

library(tidyverse)
library(dplyr)
library(lubridate)


#create simple matrix, 2x2 or 2x4
data.2.4 <- as.data.frame(matrix(c(1,2,3,4,5,-2,7,9), nrow = 4, ncol = 2, dimnames = list(c("p1", "p2", "p3", "p4"), c("x","y"))))


data <- matrix(c(1,2,5,-2), nrow = 2, ncol = 2, dimnames = list(c("p1", "p2"), c("x","y")))


#line1 <- line(c(p1,p2))
#line is from stats package. Is that part of base R? It creates a tukeyline object, and I don't know if it's optimal for my use.

#plot(data)
#segments(1,5, x1=2, y1=-2)

line2 <- lm(y~x, as.data.frame(data))
summary(line2)

co <- coef(line2)
int <- co["(Intercept)"]
slope <- co["x"]

x <- (-slope)/int #this is how we solve for x when y = 0
nupoint <- c(x, y = 0) #create new datapoint
extras.df <- rbind(data, nupoint) #append the new datapoint
extras.df[order(rownames(extras.df)),] #re-order the dataframe

data.2.4$switch <- as.logical((data.2.4$y)*(lead(data.2.4$y)) < 0) #detect if the next point has a sign-switch 

switch.df <- data.2.4[data.2.4$switch == TRUE,]
switch.df

triangle.FUN <- function(data){
  #return(str(data))
  switch.df <- data[data$switch == "TRUE" | lag(data$switch) == "TRUE",]
  RN.vec <- as.list(c(row.names(switch.df))) #creates a list of row names where the next point is across the axis. might not be needed.
return(switch.df)
  
  
  }
  
  matrix(c())
  line2 <- lm(y~x, as.data.frame(data))
  summary(line2)
  
  co <- coef(line2)
  int <- co["(Intercept)"]
  slope <- co["x"]
  
  x <- (-slope)/int #this is how we solve for x when y = 0
  nupoint <- c(x, y = 0) #create new datapoint
  extras.df <- rbind(data, nupoint) #append the new datapoint
  extras.df[order(rownames(extras.df)),] #re-order the dataframe
  
  
}
  
triangle.FUN(data.2.4)

data.2.4$coef <- with(data.2.4, if(data.2.4["switch" == "TRUE"]){
  print("it worked")
})

#potential strengths with this method:
 #All points can be ordered by date/time after they are appended.
?with
#problems to solve:
 #turning date format into x-coordinate
 #create an if statement to process these, drawing in the previous and next columns.

#y = mx + b

#0 = mx + b
#-b/m = x

#-int/co = x
#create point: xy.coords(-int/co, y = 0)

