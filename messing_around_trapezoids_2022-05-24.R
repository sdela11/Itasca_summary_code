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

matrix(c(1,2,5,-2), nrow = 2, ncol = 2, dimnames = list(c("p1", "p2"), c("x", "y")))


class(p1)

line1 <- line(c(p1,p2))

plot(c(p1,p2))
segments(1,5, x1=2, y1=-2)

str(line1)
plot(line1)

#example from help file:
ff <- stats::fft(1:9)
xy.coords(ff)
xy.coords(ff, xlab = "fft") # labels "Re(fft)",  "Im(fft)"


