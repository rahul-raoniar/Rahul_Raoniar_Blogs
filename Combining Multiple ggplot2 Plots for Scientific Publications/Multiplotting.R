########################
# Combining plots
########################

# View dataset

head(mtcars)

# Import tideverse library
library(tidyverse)

glimpse(mtcars)

# Convert gear to categorical

class(mtcars$gear)
class(mtcars$am)

mtcars$gear <- as.factor(mtcars$gear)
mtcars$am <- as.factor(mtcars$am)

class(mtcars$gear)
class(mtcars$am)


############################
# plot1: Create a scatter plot

plot1 <- ggplot(data = mtcars, mapping = aes(x = wt, y = mpg,
                                             colour = gear)) +
  geom_point(size=3) +
  ggtitle("plot1")

plot1

############################
# plot2: Create a histogram plot


plot2 <- ggplot(data = mtcars, mapping = aes(x = mpg)) +
  geom_histogram(binwidth = 5) +
  ggtitle("plot2")

plot2


############################
# plot3: Create a boxplot


plot3 <- ggplot(data = mtcars,
                mapping = aes(x = interaction(gear,am), y=mpg)) +
  geom_boxplot() +
  ggtitle("plot3")

plot3

############################
# plot4: Create a line plot


plot4 <- ggplot(data = mtcars, mapping = aes(x = hp, y=mpg)) +
  geom_line() +
  ggtitle("plot4")

plot4




###############################
# Combining plots using "grid" and "gridExtra"
###############################

# Method 1

# grid library

# install.packages("grid")
citation("grid")

# Import grid package
library(grid)


# Create a new page

grid.newpage()


# Next push the vissible area with a layout of 2 columns and 2 row using pushViewport()

pushViewport(viewport(layout = grid.layout(2,2)))


# Put the chart on the the area by row and column position

print(plot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(plot2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(plot3, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(plot4, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))



# Method 2

# Use the gridExtra package

# install.packages("gridExtra")
citation("gridExtra")

# import gridExtra Package
library(gridExtra)

grid.arrange(plot1, plot2, plot3, plot4, nrow=2, ncol=2)

# Changing the order
grid.arrange(plot3, plot2, plot4, plot1, nrow=2, ncol=2)




## Method 3

library(ggpubr)

figure <- ggarrange(plot1, plot2, plot3, plot4,
                    ncol = 2, nrow = 2)
figure




ggarrange(
  plot4,    # plot4 in first row
  ggarrange(plot1, plot2, ncol = 2), 
  nrow = 2  # plot1 and plot2 in second row
) 


library(patchwork)
install.packages("patchwork")

plot1 + plot2


(plot1 | plot2 | plot3) /
  plot4
