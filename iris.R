getwd()

setwd("C:/Users/sameer jadhav/Documents/r function/Data_science/dataset and analysis/iris_Dataset")

########1. Import iris.csv file from folder iris_dataset. ####
data <- read.csv("iris.csv")
print(data)

data <-read.csv("iris.csv",stringsAsFactors = FALSE)
######## 2 Exploratory Analysis.#############
#######A. Explore / Print first 3 Records from Dataset. ###
head(data,3)
#tail(data)
#####E. Find Structure of Data.###
str(data)

###### F. Find mean, median, quartile, max, min data for every feature.###
summary(data)
########### C. Find Names , Class of features in the Dataset. ###
names(data)
class(data)
#nrow(data)
########B. Find Dimension of Dataset. #########
dim(data)

#attributes(data)
###D. Find missing values (if any) & make the data consistent by removing it.#######
is.na(data)
na.omit(data)


colSums(is.na(data))

View(data)


##########################################
#use the summary(data)

print(summary(data))
###########################################
# G. Plot a Boxplot Graph, Pie chart respective to their Species##

# pie chart to the species.

data <- c("setosa","versicolor","virginica")
print(data)

#
png = (file = "species.jpg")

pie(table(data))

# save the file
dev.off()




# box plot to the species
library("ggplot2")
boxplot(iris$Sepal.Length~iris$Species)
boxplot(iris$Sepal.Length~iris$Species,xlab="Species",ylab="Sepal Length",main="Species Information")


####################################
#### H. Subset tuples based on their Species in different R-Object######
#sepal_length
#sepal_width
#petal_length
#petal_width
#species

Sl <- subset(data,sepal_length == 4:8)
species <- subset(data,sepal_lenght=sl)
print(species$sepal_length)


sw <- subset(data,sepal_width == 2:4.4)
species <- subset(data,sepal_length=sw)
print(species$sepal_width)

pl <- subset(data,petal_length == 1:6.9)
species <- subset(data,petal_length=pl)
print(species$petal_length)

pw <- subset(data,petal_width == 0.1:2.5)
species <- subset(data,petal_width=pw)
print(species$petal_width)

ss <- subset(data,species == character())
species <- subset(data,species=ss)
print(species$species)


###################################
#### I. Plot a BoxPlot Graph for Individual R-Object. ######
install.packages("ggplot2")
library("ggplot2")
png(file="boxplot2.png")
box <- ggplot(data, aes(x=species, y=sepal_length)) +
  geom_boxplot(aes(fill=species)) +  ylab("Sepal Length") +  ggtitle("Iris Boxplot") +
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) 
png
dev.off()

#####################################

#J Plot a Histogram on feature Petal lengths of iris dataset .#######3
png(file="histo1.png")
hist(data$petal_length,xlab = "length",col="blue",border="green")
dev.off()
############################################
####K. Plot a Histogram for Petal Lengths of Different Species on different Graph#####
library("ggplot2")
ggplot(data, aes(species, petal_length, fill=species)) + 
  geom_boxplot()+
  scale_y_continuous("Petal Length (cm)", breaks= seq(0,30, by=.5))+
  labs(title = "Iris Petal Length Box Plot", x = "Species")
png = (file = "different species.jpg")
dev.off()



################################################
##########L. Find correlation between multiple features also plot a scatter plot for correlation.####

corr <- cor(data[,1:4])
round(corr,3)
png(file = "scatterplot1.jpg")
pairs(data[,1:4])
print(corr)
dev.off()
#############################################
#4. Classify Data based on iris Species and plot a Decision Tree.#

library(rpart)
install.packages("rpart.plot")
library(rattle)
library(rpart.plot)
png(file ="decisiontree2.png")
input.dat <- data
print(input.dat)
fm = species ~ sepal_length + sepal_width + petal_length + petal_width
fit <- rpart(formula = fm,input.dat,method = 'class')
fancyRpartPlot(fit)
dev.off()


