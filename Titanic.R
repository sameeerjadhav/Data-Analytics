getwd()
setwd("C:/Users/sameer jadhav/Documents/r function/Data_science/dataset and analysis/Titanic Dataset")

##Importing the Titanic dataset

titanic_data <- read.csv("train.csv",stringsAsFactors = FALSE)
print(titanic_data)

##Performing Exploratory Data Analysis (EDA)

##Print first n Records from Dataset
head(titanic_data)
tail(titanic_data)
str(titanic_data)
##Find mean, median, quartile, max, min data for every feature
summary(titanic_data)
nrow(titanic_data)
dim(titanic_data)
attributes(titanic_data)
is.na(titanic_data)
colSums(is.na(titanic_data))
View(titanic_data)

##Number of Passengers according to their Group Class: 1st , 2nd , 3rd

nop <- as.factor(titanic_data$Pclass)
summary(nop)    ## There were 216 people in 1st class,184 in 2nd class & 491 in 3rd class

##Number of Passengers according to their Group Sex: Male , Female

psex <- as.factor(titanic_data$Sex)
summary(psex)  ## There were 314 Females & 577 Males

##Stats of Passengers Age

page <- as.factor(titanic_data$Age)
summary(page)

##Number of Passengers according to their Group Embarked: Cherbourg, Queenstown or Southampton

ploc <- as.factor(titanic_data$Embarked)
summary(ploc)  ## Cherbourg = 168 , Queenstown = 77 & Southampton = 644

##Number of passengers who Survived / Not Survived

psurvive <- as.factor(titanic_data$Survived)
summary(psurvive)  ## 549 Not Survived & 342 Survived

##Working with only four input variables and one response variable

ipop <- titanic_data[,c(3,5,6,12)]
print(ipop)

##Data Cleaning

clean_data <- na.omit(titanic_data)
rownames(clean_data)

##Categorizing Age  

clean_data$Age[clean_data$Age <= 18] = "child"
clean_data$Age[(clean_data$Age > 18) & (clean_data$Age <= 60) & (clean_data$Age != "child")] = "adult"
clean_data$Age[(clean_data$Age != "child") & (clean_data$Age != "adult")] = "senior"
clean_data$Age = as.factor(clean_data$Age)
head(clean_data)

##Barplot of Input Variables  
library("ggplot2")
png(file="titanicbarplot.png")
barplot(table(clean_data$Pclass), xlab="Class", ylab="Input Variables", main="Barplot")
dev.off()

##Converting Categorical Dataframe into Numeric Dataframe

clean_data$Pclass = as.integer(clean_data$Pclass)
clean_data$Sex = as.integer(clean_data$Sex)
clean_data$Age = as.integer(clean_data$Age)
clean_data$Embarked = as.integer(clean_data$Embarked)
clean_data$Survived = as.integer(clean_data$Survived)
head(clean_data)

##Number of survivors on an average from Class & Plot a scatter plot

me_pclass = c(0,0,0)
me_pclass[1] = mean(clean_data$Survived[clean_data$Pclass==1])
me_pclass[2] = mean(clean_data$Survived[clean_data$Pclass==2])
me_pclass[3] = mean(clean_data$Survived[clean_data$Pclass==3])
png(file="Scatter.png")
plot(me_pclass, type="o", main="Scatter Plot", xlab="Class", ylab="Main Effect")
axis(1, at=c(1,2,3), labels=c("1st", "2nd", "3rd"))
dev.off()

##Number of survivors on an average from Gender & Plot a scatter plot

me_Sex = c(0,0)
me_Sex[1] = mean(titanic_data$Survived[titanic_data$Sex=="male"])
me_Sex[2] = mean(titanic_data$Survived[titanic_data$Sex=="female"])
png(file="Scatter2.png")
plot(me_Sex, type="o", main="Scatter Plot", xlab="Sex", ylab="Main Effect")


axis(1, at=c(1,2), labels=c("Male", "Female"))
dev.off()

##Number of survivors on an average from Every Port & Plot a scatter plot

me_emb = c(0,0,0)
me_emb[1] = mean(titanic_data$Survived[titanic_data$Embarked=="C"])
me_emb[2] = mean(titanic_data$Survived[titanic_data$Embarked=="Q"])
me_emb[3] = mean(titanic_data$Survived[titanic_data$Embarked=="S"])
png(file="Scatter3.png")
plot(me_emb, type="o", main="Embarkment", xlab="Port of Embarkment", ylab="Main Effect")
axis(1, at=c(1,2,3), labels=c("Cherbourg", "Queenstown", "Southampton"))
dev.off()


##Validating scatterplots using ANOVA

a1 = aov(titanic_data$Survived ~ titanic_data$Pclass)
anova(a1)

a2 = aov(titanic_data$Survived ~ titanic_data$Sex)
anova(a2)

a3 = aov(titanic_data$Survived ~ titanic_data$Embarked)

anova(a3)

