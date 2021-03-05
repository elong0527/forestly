# introduction
## data visualisation
## data transformation
## exploratory data analysis

# data visualisation
library(tidyverse)
# For example, ggplot2::ggplot() tells you explicitly that we’re using the ggplot() function from the ggplot2 package.
mpg
#displ:a car's engine size
#hwy: a car’s fuel efficiency on the highway, in miles per gallon (mpg).
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class))
#workflow:basics
r_rocks <- 2 ^ 3
r_rocks
my_variable <- 10
my_variable
mpg
ggplot(dota = mpg) +
  geom_point(mapping = aes(x = "displ", y = "hwy"))

fliter(mpg, cyl = 8)
filter(diamond, carat > 3)
