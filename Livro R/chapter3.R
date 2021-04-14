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

#Geometric objects

#plots with different representation of data, but iqual data, uses different GEOMS
#A geom is the geometrical object that a plot uses to represent data.
#to change the geom use different function of geom

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

#not every aesthetic works with every geom
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))

#ggplot2 provides over 40 geoms, and extension packages provide more https://exts.ggplot2.tidyverse.org/gallery/
#http://rstudio.com/resources/cheatsheets

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

#set the group aes to a categorical variable
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

ggplot(data = mpg) +
  geom_smooth(
    mapping = aes(x = displ, y = hwy, color = drv),
    show.legend = FALSE
  )

#plot with 2 geoms in the same plot
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))
#plot with 2 geoms in the same plot but passing a set of mappings to ggplot()
#will aply to each geom in the graph
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + #global mapping
  geom_point() + 
  geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + #global mapping
  geom_point(mapping = aes(color = class)) + #local mapping
  geom_smooth()

#this command selects only the subcompact cars for the smooth geom
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)

#----exercise 1
ggplot(data = mpg) + 
  geom_line(mapping = aes(x = drv, y = cyl))

#----exercise 2
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point() + 
  geom_smooth(se = FALSE)
#----exercise 3
#show.legend = FALSE doesnt show the legend for the graph

#----exercise 4
# se displays confidence interval around smooth, its TRUE by default

#----exercise 5
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

ggplot() + 
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy))

# os gráficos n são diferentes, no primeiro temos mapping global, no segundo mapping local
#----exercise 6
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth(se = FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(mapping = aes(group= drv), se=FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color= drv)) +
  geom_point() +
  geom_smooth(se=FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(aes(color= drv)) +
  geom_smooth(se=FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(aes(color= drv)) +
  geom_smooth(aes(group = drv), se=FALSE)

#nesse é preciso inverter as cores, transparente por fora e colorido por dentro!
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color= drv)) +
  geom_point(        alpha=0.5,
                     size=3,
                     stroke = 2)

#-----------------------------#

#Statistical transformations

# bar chart
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut)) #bar charts, histograms and frequency polygons bin the data and then plot bin counts, the number of points that fall in each bin
# algorithm used to calculate new values for a graph: stat, short for statistical transformation
# need to see the default value for stat for every graph
#eom_bar() uses stat_count() as default

ggplot(data = diamonds) + 
  stat_count(mapping = aes(x = cut))
#every geom has a default stat and every stat has a default geom


demo <- tribble(
  ~cut,         ~freq,
  "Fair",       1610,
  "Good",       4906,
  "Very Good",  12082,
  "Premium",    13791,
  "Ideal",      21551
)

ggplot(data = demo) +
  geom_bar(mapping = aes(x = cut, y = freq), stat = "identity")

#display a bar chart of proportion, rather than count
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = stat(prop), group = 1))

ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.min = min,
    fun.max = max,
    fun = median
  )

?stat_bin

#----exercise 1
?stat_summary
#geom = "pointrange"

?geom_pointrange
ggplot(data = diamonds) + 
  geom_pointrange(
    mapping = aes(x = cut, y = depth)
  )
#----exercise 2
?geom_col()
?geom_bar()
#these are the 2 tipes of bar charts,
#geom_bar() makes the height of the bar proportional to the number of cases in each group 
#If you want the heights of the bars to represent values in the data, use geom_col() instead. 
#geom_bar() uses stat_count() by default: it counts the number of cases at each x position. 
#geom_col() uses stat_identity(): it leaves the data as is.

#----exercise 3
#----exercise 4
?stat_smooth
#use the same arguments of geom_smooth
#----exercise 5
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = after_stat(prop)))
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = color, y = after_stat(prop)))

#-----------------------------#

#Position adjustments

#to colour a bar we can use colour or fill:
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, colour = cut))
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))