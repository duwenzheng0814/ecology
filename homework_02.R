rm(list = ls())
install.packages("caret")
install.packages("tree")
#Section I: Read data and data pre-Process – preliminary. This section covers:
  #- reading env and fish data, and summarizing fish abundance data by sites, and combining env and total fish to a new data frame named as "env_fish".
library(ade4)
library(tidyverse)
data(doubs, package="ade4")
env <- doubs$env
site <- row.names(env)
env <- cbind(site,env)
fish <- doubs$fish      #extract fish from doubs
env_fish <- cbind(env,total_fish=rowSums(fish))     #summarizing fish abundance data by sites, and combining env and total fish to a new data frame named as "env_fish"
env_fish

  #-visualizing the features of the new env_fish set using scatterplot(). Do you find any linear relationships between environmental variables and the total fish abundance at the sites?
library(caret)
featurePlot(x=env_fish[,c(-1,-13)],
            y=env_fish[, 13],
            plot = "scatter",
            type=c("p","smooth"),
            layout=c(4,3))

#- having the sites with no fishes? If yes, deleting such sites. Having null values or outliers? If yes, removing all rows where any column contains an outlier. 
env_fish <- subset(env_fish, total_fish != 0)      #deleting the sites with no fishes
attach(env_fish)
boxplot(dfs)
boxplot(alt)
boxplot(slo)
boxplot(flo);flo_outlier<-which(flo %in% boxplot.stats(flo)$out)
boxplot(pH);pH_outlier<-which(pH %in% boxplot.stats(pH)$out)
boxplot(har);har_outlier<-which(har %in% boxplot.stats(har)$out)
boxplot(pho);pho_outlier<-which(pho %in% boxplot.stats(pho)$out)
boxplot(nit);nit_outlier<-which(nit %in% boxplot.stats(nit)$out)
boxplot(amm);amm_outlier<-which(amm %in% boxplot.stats(amm)$out)
boxplot(oxy)
boxplot(bdo);bdo_outlier<-which(bdo %in% boxplot.stats(bdo)$out)
boxplot(total_fish)        #check the data for outliers 
detach(env_fish)
x <- c(flo_outlier, pH_outlier, har_outlier, pho_outlier, nit_outlier, amm_outlier, bdo_outlier)
y <- x[!duplicated(x)]
env_fish <- env_fish[-c(y),]     #removing all rows where any column contains an outlier

#- identifying near zero-variance, outlies of the env variables. If yes, excluding them for analysis.
env2 <- env_fish[,c(-1,-13)]
nearZeroVar(env2, saveMetrics = TRUE)

#- detecting the collinearity among env variables or removing highly correlated features (with an absolute correlation of 0.75 or higher) 
findLinearCombos(env2)      #detect collinearity among env variables
a <- findCorrelation(cor(env2), cutoff = 0.95, names = TRUE)
env_fish <- select(env_fish, -dfs)      #detect and remove highly correlated features

#Section II: Building a regression model. This section covers:
 #- splitting data into training and test sets, and visualizing the features and targets of the training set
set.seed(2023)
sample <- sample(c(TRUE, FALSE), nrow(env_fish), replace=TRUE, prob=c(0.8,0.2))
train <- env_fish[sample, ]
test <- env_fish[!sample, ]     #split data into training and test sets
featurePlot(x=train[,c(-1,-12)],y=train[, 12],plot="scatter",type=c("p","smooth"))       #visualizing the features and targets of the training set

 #- Creating and evaluating a baseline model between the environmental variables and the total fish abundance with the tree-based algorithm
set.seed(222)
library(rpart)
treemodel <-rpart(total_fish~.,data = train, control=rpart.control(minsplit=2),)
plot(treemodel)
