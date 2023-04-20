rm(list = ls())
library(ade4)
library(tidyverse)
library(vegan)
library(stats)
library(factoextra)
#Read in the doubs into your R and delete the site 8 which has no fishes. 
#Write code and answer the questions: Which site has the most species (and how many species)? 
#Which species is the most widespread (i.e., found in the most sites)?
data(doubs, package="ade4")      #Read in the doubs
fish <- doubs$fish
fish <- fish[-8,]    #delete the site 8 which has no fishes
site <- data.matrix(rowSums(fish!=0))
site[which.max(site),]      #find the site 29 which has the most species 26
species <- data.matrix(colSums(fish!=0))
species[which.max(species),]      #find species "Lece" is the most widespread, and the "Lece" is found in 25 sites
spe <- doubs$species
spe[which.max(species),]      #find the scientific name for "Lece"

#Select a suitable association measure of species, write code for clusters and answer: 
#In terms of the fish community composition, which groups of species can you identify? 
#Which groups of species are related to these groups of sites?

#R Mode:cluster analysis of species by site
fish.t <-t(fish)      #transpose the data
fish.t.chi <- decostand(fish.t, "chi.square")
fish.t.D16 <- dist(fish.t.chi)       #get the euclidean distance
fish.t.chi.single <- hclust(fish.t.D16, method = "single")       #single linkage agglomerative clustering
fish.t.chi.complete <- hclust(fish.t.D16, method = "complete")        #complete linkage agglomerative clustering

#Q Mode:cluster analysis of environmental variables by site
env <- doubs$env
env <- env[-8,] 
env.norm <- decostand(env, "normalize")
env.ch <- vegdist(env.norm, "euc")       #get the chord distance
env.ch.single <- hclust(env.ch, method = "single")       #single linkage agglomerative clustering
env.ch.complete <- hclust(env.ch, method = "complete")        #complete linkage agglomerative clustering

par(mfrow=c(2,2))
plot(fish.t.chi.single, main = "欧式距离-单连接")
plot(env.ch.single, main = "弦距离-单连接")
plot(fish.t.chi.complete, main = "欧式距离-完全连接")
plot(env.ch.complete, main = "弦距离-完全连接")

#k-means cluster analysis of site
fish.de <- vegdist(scale(fish), "euc")
fish.kmeans <- kmeans(fish.de, centers = 4, nstart = 100)
fish.kmeans.g <-fish.kmeans$cluster
env.de <- vegdist(scale(env), "euc")
env.kmeans <- kmeans(env.de, centers = 4, nstart = 100)
env.kmeans.g <- env.kmeans$cluster
fviz_cluster(fish.kmeans,data = fish)
fviz_cluster(env.kmeans,data = env)
table(fish.kmeans.g, env.kmeans.g)
fisher.test(table(fish.kmeans.g, env.kmeans.g))

#Do RDA analysis, and then write code and answer: 
#Which environmental variables cause a community to vary across a landscape?
fish.hel <- decostand(fish, "hellinger")       #hellinger transform the species data
fish.rda <- rda(fish.hel ~ ., env)
summary(fish.rda)
plot(fish.rda)
#according to the RDA plot, all the environmental variables except pH cause a community to vary across a landscape
