#use built in iris dataset
#take a look at it 
head(iris)
#load in some tidyverse packages
library(dplyr)
library(ggplot2)

#####################################
##### Part 1: for loops         #####
#####################################

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length

# hint: consider using a list, and also new vectors for regression variables

storeRegression <- list() # new list created

flower <- iris[iris$Species=='versicolor',] #versicolor data

for (i in 1:3) { #3 iteration for loop
#the 3 relationshpis are correspondents with regression tables needed
  if (i == 1) {
    storeRegression[[i]] <- lm(flower$Sepal.Length ~ flower$Sepal.Width)
  }
  if (i == 2) {
    storeRegression[[i]] <- lm(flower$Petal.Length ~ flower$Petal.Width)
  }
  if (i == 3) {
    storeRegression[[i]] <-  lm(flower$Sepal.Length ~ flower$Petal.Length)
  }
}
storeRegression
# print to test
#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))
newIris <- data.frame(iris) #new iris data frame
full_join(newIris, height)            # Apply full_join dplyr function



#####################################
##### Part 3: plots in ggplot2  #####
#####################################

#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot
#aes represents x and y axis
#geom_point plots Them
my_plot = ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + 
  geom_point()
my_plot

#3b. make a scatter plot with ggplot and get rid of busy grid lines
#adding theme_classic() to ggplot removes the grid lines
my_plot = ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + 
  geom_point() + theme_classic()
my_plot

#3c. make a scatter plot with ggplot, remove grid lines, add a title and axis labels, 
#    show species by color, and make the point size proportional to petal length
#aes for ggplot allows you to change color by species which is what we input
#show.legend being true shows the legend for our plot
#ggtitle represents the title
#xlab and ylab label the axis respectively.
my_plot = ggplot(iris, aes(Sepal.Length, Sepal.Width, colour = Species)) + 
  geom_point(aes(size = Sepal.Length), show.legend = TRUE)+ 
  theme_classic() + 
  ggtitle("Scatter plot of Sepal length and Width") +
  xlab("Sepal Length") + ylab("Sepal Width")
my_plot

#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################		