#Playing with trapezoids
#Sara DeLaurentis
#2022-05-24

#Stuff I'm trying:
#Create a line between two points.
#Solve for x = 0

library(tidyverse)
library(dplyr)
library(lubridate)

p1 <- xy.coords(1, y = 5)
p2 <- xy.coords(2, y = -2)
class(p1)


data <- matrix(c(1,2,5,-2), nrow = 2, ncol = 2, dimnames = list(c("p1", "p2"), c("x","y")))


#line1 <- line(c(p1,p2))
#line is from stats package. Is that part of base R? It creates a tukeyline object, and I don't know if it's optimal for my use.

plot(data)
segments(1,5, x1=2, y1=-2)

line2 <- lm(y~x, as.data.frame(data))
summary(line2)

co <- coef(line2)

#y = mx + b

#0 = mx + b
#-b/m = x

#-int/co = x
#create point: xy.coords(-int/co, y = 0)


solve()

str(line1)
plot(line1)

#example from help file:
ff <- stats::fft(1:9)
xy.coords(ff)
xy.coords(ff, xlab = "fft") # labels "Re(fft)",  "Im(fft)"


