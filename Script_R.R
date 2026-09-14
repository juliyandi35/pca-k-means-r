## PCA Section
library(GGally)
library(tidyverse)
library(readr)

# import dataset from uci.edu database
library(readxl)
Dataset <- read_excel("Dataset.xlsx")
Dataset <- lapply(Dataset, function(x) replace(x, is.na(x), 0))
Dataset <- data.frame(Dataset)
head(Dataset)
Qdata <- subset(Dataset, select = -c(Gender,Age,Pendidikan.Terakhir,Domisili.sebelum.pandemi,Domisili.saat.pandemi..2020.2023.))
head(Qdata)
# calculate covariance matrix

Qdata%>%cor()%>%round(3)

# visualize density plots, scatter plots, and correlation coefficients between original features

ggpairs(Qdata)

# perform PCA

pca <- prcomp(Qdata, scale. = TRUE)

# extract important components
summary(pca)

# calculate covariance matrix in PCA space

cor(pca$x[,1:45])%>%round(3)

# convert PCA matrix into a dataframe

pcdf<-as.data.frame(pca$x[,1:45])
names(pcdf)<-c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10","PC11","PC12","PC13","PC14","PC15","PC16","PC17","PC18","PC19","PC20","PC21", "PC22","PC23","PC24", "PC25","PC26","PC27","PC28","PC29","PC30","PC31","PC32","PC33","PC34","PC35","PC36","PC37","PC38","PC39","PC40","PC41","PC42","PC43","PC44","PC45")

## K-Means Clustering Section
# Define each dataset
GenreData <- data.frame(pcdf,Dataset$Gender)
AgeData <- data.frame(pcdf,Dataset$Age)
Pendidikan.TerakhirData <- data.frame(pcdf,Dataset$Pendidikan.Terakhir)
Domisili.sebelum.pandemiData <- data.frame(pcdf,Dataset$Domisili.sebelum.pandemi)
Domisili.saat.pandemi..2020.2023.Data <- data.frame(pcdf,Dataset$Domisili.saat.pandemi..2020.2023.)
#import ggplot library to plot the dataset
# Genre Clust
library(ggplot2)

# set the point on initial seed
set.seed(15)

# create k-Means Cluster for iris data set
Cluster <- kmeans(GenreData[,1:45], 4)
Cluster

# print the values in tabular form
table(Cluster$cluster, GenreData$Dataset.Gender)

Cluster$cluster <- as.factor(Cluster$cluster)

# plot cluster using K-Means model
ggplot(GenreData, aes(PC1, PC2, color = Cluster$cluster)) + geom_point()
ggplot(GenreData, aes(PC3, PC4, color = Cluster$cluster)) + geom_point()
ggplot(GenreData, aes(PC5, PC6, color = Cluster$cluster)) + geom_point()
ggplot(GenreData, aes(PC7, PC8, color = Cluster$cluster)) + geom_point()
ggplot(GenreData, aes(PC9, PC10, color = Cluster$cluster)) + geom_point()
ggplot(GenreData, aes(PC11, PC12, color = Cluster$cluster)) + geom_point()
ggplot(GenreData, aes(PC12, PC14, color = Cluster$cluster)) + geom_point()
ggplot(GenreData, aes(PC15, PC16, color = Cluster$cluster)) + geom_point()
ggplot(GenreData, aes(PC17, PC18, color = Cluster$cluster)) + geom_point()
ggplot(GenreData, aes(PC19, PC20, color = Cluster$cluster)) + geom_point()
ggplot(GenreData, aes(PC21, PC22, color = Cluster$cluster)) + geom_point()
ggplot(GenreData, aes(PC23, PC24, color = Cluster$cluster)) + geom_point()
ggplot(GenreData, aes(PC25, PC26, color = Cluster$cluster)) + geom_point()
ggplot(GenreData, aes(PC27, PC28, color = Cluster$cluster)) + geom_point()
ggplot(GenreData, aes(PC29, PC30, color = Cluster$cluster)) + geom_point()
ggplot(GenreData, aes(PC31, PC32, color = Cluster$cluster)) + geom_point()
ggplot(GenreData, aes(PC33, PC34, color = Cluster$cluster)) + geom_point()
ggplot(GenreData, aes(PC35, PC36, color = Cluster$cluster)) + geom_point()
ggplot(GenreData, aes(PC37, PC38, color = Cluster$cluster)) + geom_point()
ggplot(GenreData, aes(PC39, PC40, color = Cluster$cluster)) + geom_point()
ggplot(GenreData, aes(PC41, PC42, color = Cluster$cluster)) + geom_point()
ggplot(GenreData, aes(PC43, PC44, color = Cluster$cluster)) + geom_point()

# Age Clust
# create k-Means Cluster for iris data set
Cluster <- kmeans(AgeData[,1:45], 4)
Cluster

# print the values in tabular form
table(Cluster$cluster, AgeData$Dataset.Age)

Cluster$cluster <- as.factor(Cluster$cluster)

# plot cluster using K-Means model
ggplot(AgeData, aes(PC1, PC2, color = Cluster$cluster)) + geom_point()
ggplot(AgeData, aes(PC3, PC4, color = Cluster$cluster)) + geom_point()
ggplot(AgeData, aes(PC5, PC6, color = Cluster$cluster)) + geom_point()
ggplot(AgeData, aes(PC7, PC8, color = Cluster$cluster)) + geom_point()
ggplot(AgeData, aes(PC9, PC10, color = Cluster$cluster)) + geom_point()
ggplot(AgeData, aes(PC11, PC12, color = Cluster$cluster)) + geom_point()
ggplot(AgeData, aes(PC12, PC14, color = Cluster$cluster)) + geom_point()
ggplot(AgeData, aes(PC15, PC16, color = Cluster$cluster)) + geom_point()
ggplot(AgeData, aes(PC17, PC18, color = Cluster$cluster)) + geom_point()
ggplot(AgeData, aes(PC19, PC20, color = Cluster$cluster)) + geom_point()
ggplot(AgeData, aes(PC21, PC22, color = Cluster$cluster)) + geom_point()
ggplot(AgeData, aes(PC23, PC24, color = Cluster$cluster)) + geom_point()
ggplot(AgeData, aes(PC25, PC26, color = Cluster$cluster)) + geom_point()
ggplot(AgeData, aes(PC27, PC28, color = Cluster$cluster)) + geom_point()
ggplot(AgeData, aes(PC29, PC30, color = Cluster$cluster)) + geom_point()
ggplot(AgeData, aes(PC31, PC32, color = Cluster$cluster)) + geom_point()
ggplot(AgeData, aes(PC33, PC34, color = Cluster$cluster)) + geom_point()
ggplot(AgeData, aes(PC35, PC36, color = Cluster$cluster)) + geom_point()
ggplot(AgeData, aes(PC37, PC38, color = Cluster$cluster)) + geom_point()
ggplot(AgeData, aes(PC39, PC40, color = Cluster$cluster)) + geom_point()
ggplot(AgeData, aes(PC41, PC42, color = Cluster$cluster)) + geom_point()
ggplot(AgeData, aes(PC43, PC44, color = Cluster$cluster)) + geom_point()

# Pendidikan.Terakhir Clust
# create k-Means Cluster for iris data set
Cluster <- kmeans(Pendidikan.TerakhirData[,1:45], 4)
Cluster

# print the values in tabular form
table(Cluster$cluster, Pendidikan.TerakhirData$Dataset.Pendidikan.Terakhir)

Cluster$cluster <- as.factor(Cluster$cluster)

# plot cluster using K-Means model
ggplot(Pendidikan.TerakhirData, aes(PC1, PC2, color = Cluster$cluster)) + geom_point()
ggplot(Pendidikan.TerakhirData, aes(PC3, PC4, color = Cluster$cluster)) + geom_point()
ggplot(Pendidikan.TerakhirData, aes(PC5, PC6, color = Cluster$cluster)) + geom_point()
ggplot(Pendidikan.TerakhirData, aes(PC7, PC8, color = Cluster$cluster)) + geom_point()
ggplot(Pendidikan.TerakhirData, aes(PC9, PC10, color = Cluster$cluster)) + geom_point()
ggplot(Pendidikan.TerakhirData, aes(PC11, PC12, color = Cluster$cluster)) + geom_point()
ggplot(Pendidikan.TerakhirData, aes(PC12, PC14, color = Cluster$cluster)) + geom_point()
ggplot(Pendidikan.TerakhirData, aes(PC15, PC16, color = Cluster$cluster)) + geom_point()
ggplot(Pendidikan.TerakhirData, aes(PC17, PC18, color = Cluster$cluster)) + geom_point()
ggplot(Pendidikan.TerakhirData, aes(PC19, PC20, color = Cluster$cluster)) + geom_point()
ggplot(Pendidikan.TerakhirData, aes(PC21, PC22, color = Cluster$cluster)) + geom_point()
ggplot(Pendidikan.TerakhirData, aes(PC23, PC24, color = Cluster$cluster)) + geom_point()
ggplot(Pendidikan.TerakhirData, aes(PC25, PC26, color = Cluster$cluster)) + geom_point()
ggplot(Pendidikan.TerakhirData, aes(PC27, PC28, color = Cluster$cluster)) + geom_point()
ggplot(Pendidikan.TerakhirData, aes(PC29, PC30, color = Cluster$cluster)) + geom_point()
ggplot(Pendidikan.TerakhirData, aes(PC31, PC32, color = Cluster$cluster)) + geom_point()
ggplot(Pendidikan.TerakhirData, aes(PC33, PC34, color = Cluster$cluster)) + geom_point()
ggplot(Pendidikan.TerakhirData, aes(PC35, PC36, color = Cluster$cluster)) + geom_point()
ggplot(Pendidikan.TerakhirData, aes(PC37, PC38, color = Cluster$cluster)) + geom_point()
ggplot(Pendidikan.TerakhirData, aes(PC39, PC40, color = Cluster$cluster)) + geom_point()
ggplot(Pendidikan.TerakhirData, aes(PC41, PC42, color = Cluster$cluster)) + geom_point()
ggplot(Pendidikan.TerakhirData, aes(PC43, PC44, color = Cluster$cluster)) + geom_point()

# Domisili.sebelum.pandemi Clust
# create k-Means Cluster for iris data set
Cluster <- kmeans(Domisili.sebelum.pandemiData[,1:45], 4)
Cluster

# print the values in tabular form
table(Cluster$cluster, Domisili.sebelum.pandemiData$Dataset.Domisili.sebelum.pandemi)

Cluster$cluster <- as.factor(Cluster$cluster)

# plot cluster using K-Means model
ggplot(Domisili.sebelum.pandemiData, aes(PC1, PC2, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.sebelum.pandemiData, aes(PC3, PC4, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.sebelum.pandemiData, aes(PC5, PC6, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.sebelum.pandemiData, aes(PC7, PC8, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.sebelum.pandemiData, aes(PC9, PC10, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.sebelum.pandemiData, aes(PC11, PC12, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.sebelum.pandemiData, aes(PC12, PC14, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.sebelum.pandemiData, aes(PC15, PC16, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.sebelum.pandemiData, aes(PC17, PC18, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.sebelum.pandemiData, aes(PC19, PC20, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.sebelum.pandemiData, aes(PC21, PC22, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.sebelum.pandemiData, aes(PC23, PC24, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.sebelum.pandemiData, aes(PC25, PC26, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.sebelum.pandemiData, aes(PC27, PC28, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.sebelum.pandemiData, aes(PC29, PC30, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.sebelum.pandemiData, aes(PC31, PC32, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.sebelum.pandemiData, aes(PC33, PC34, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.sebelum.pandemiData, aes(PC35, PC36, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.sebelum.pandemiData, aes(PC37, PC38, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.sebelum.pandemiData, aes(PC39, PC40, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.sebelum.pandemiData, aes(PC41, PC42, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.sebelum.pandemiData, aes(PC43, PC44, color = Cluster$cluster)) + geom_point()

# Domisili.saat.pandemi..2020.2023. Clust
# create k-Means Cluster for iris data set
Cluster <- kmeans(Domisili.saat.pandemi..2020.2023.Data[,1:45], 4)
Cluster

# print the values in tabular form
table(Cluster$cluster, Domisili.saat.pandemi..2020.2023.Data$Dataset.Domisili.saat.pandemi..2020.2023.)

Cluster$cluster <- as.factor(Cluster$cluster)

# plot cluster using K-Means model
ggplot(Domisili.saat.pandemi..2020.2023.Data, aes(PC1, PC2, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.saat.pandemi..2020.2023.Data, aes(PC3, PC4, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.saat.pandemi..2020.2023.Data, aes(PC5, PC6, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.saat.pandemi..2020.2023.Data, aes(PC7, PC8, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.saat.pandemi..2020.2023.Data, aes(PC9, PC10, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.saat.pandemi..2020.2023.Data, aes(PC11, PC12, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.saat.pandemi..2020.2023.Data, aes(PC12, PC14, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.saat.pandemi..2020.2023.Data, aes(PC15, PC16, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.saat.pandemi..2020.2023.Data, aes(PC17, PC18, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.saat.pandemi..2020.2023.Data, aes(PC19, PC20, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.saat.pandemi..2020.2023.Data, aes(PC21, PC22, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.saat.pandemi..2020.2023.Data, aes(PC23, PC24, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.saat.pandemi..2020.2023.Data, aes(PC25, PC26, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.saat.pandemi..2020.2023.Data, aes(PC27, PC28, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.saat.pandemi..2020.2023.Data, aes(PC29, PC30, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.saat.pandemi..2020.2023.Data, aes(PC31, PC32, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.saat.pandemi..2020.2023.Data, aes(PC33, PC34, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.saat.pandemi..2020.2023.Data, aes(PC35, PC36, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.saat.pandemi..2020.2023.Data, aes(PC37, PC38, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.saat.pandemi..2020.2023.Data, aes(PC39, PC40, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.saat.pandemi..2020.2023.Data, aes(PC41, PC42, color = Cluster$cluster)) + geom_point()
ggplot(Domisili.saat.pandemi..2020.2023.Data, aes(PC43, PC44, color = Cluster$cluster)) + geom_point()
