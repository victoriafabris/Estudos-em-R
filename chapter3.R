install.packages("tidyverse")
library(tidyverse)
mpg
#ggplot() creates a coordinate system that you can add layers to
#geom_point() adds a layer of points to your plot, which creates a scatterplot
ggplot(data=mpg) + geom_point(mapping = aes(x = displ, y = hwy)) #mpg is the dataset of the graph
#the plot shows a negative relationship between displ and hwy
ggplot(data = mpg)
?mpg
ggplot(data = mpg) + geom_point(mapping = aes(x = hwy, y = cyl))
ggplot(data = mpg) + geom_point(mapping = aes(x = class, y = drv))
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = class)) #color:  aesthetic choosed
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, size = class)) #size: another aesthetic, gets and warning

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, alpha = class))#alpha: aesthetic that changes the transparency 

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, shape = class))#shape: only use six shapes at a time

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy), color = "purple")
#----exercise 1
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = "blue")) #does not turn the point into blue because the aestethic needs to goes outside of aes()

#----exercise 2
#categorical variabels in mpg: manufacturer, model, drv, fl, class
#continuous variables in mpg: displ, year, cyl, cty, hwy

#----exercise 3
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = year)) 
#fot continuous variables, the color aes. behaves doing a color gradien
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = model))
#fot categ. variables, the color aes. behaves doing a association of different color for each category

#----exercise 4
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = year, shape = year)) 

#----exercise 5
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, stroke = 3))
#for shapes that have a border is possible to colour the inside and outside separately, stroke aes. modifies the width of the border

#----exercise 6
ggplot(data = mpg) + geom_point(mapping = aes(colour = displ <5))
#-----------------------------#

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2) #the variable passed to facet_wrap() should be discrete!

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(. ~ cyl)

#----exercise 1
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ year, nrow = 2)

#----exercise 2
#means that there's no point in that subsection, for example, doesnt exists a car that has 5 cyl and is the type 4wd
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = drv, y = cyl))

#----exercise 3
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .) #facets into rows

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl) #facets into columns

#----exercise 4
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

#----exercise 5
?facet_wrap

#----exercise 6

#-----------------------------#
