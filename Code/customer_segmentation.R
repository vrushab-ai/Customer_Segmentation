setwd("C:\\Users\\Admin\\Documents\\Projects\\Customer_Segmentation\\Data")

# Load Data
data <- read.csv("Mall_Customers.csv")

## Install Packages
library(purrr)
library(cluster) 
library(gridExtra)
library(grid)
library(NbClust)
library(factoextra)
library(ggplot2)

# Check dimension, Data Types, names and summary
dim(data)
head(data)
str(data)

data$Gender <- as.factor(data$Gender) #Convert Gender to factor variable

summary(data)    

colnames(data)

names(data)[4] <- "Annual_Income"
names(data)[5] <- "Spending_Score"

# Check for Missing Values
sum(is.na(data))
sapply(data, function(x) sum(is.na(x))) #There are no missing Values

# Check for Duplicate Values
table(duplicated(data)) # There are no duplicate Value.

#Check for Outliers
boxplot(data[, c(3:5)], col = "#2CA25F")

###################################################################################################################

## EDA
library(ggplot2)

# Gender Count
plot(data$Gender, main = "Gender Class Count", xlab = "Gender", ylab = "Count", col = c("blue","green")) 

# Age Distribution
hist(data$Age, main = "Age Distribution", xlab = "Age", ylab = "Frequency", col = "#CC79A7", labels = TRUE)

# Age Distribution vs Gender
ggplot(data, aes(x = Age, fill = Gender)) + geom_histogram(col = "black") + facet_grid(Gender ~.) + ggtitle('Age distribution by Gender') 

# Annual Income Distribution
hist(data$Annual_Income, main = "Annual Income Distribution", xlab = "Annual Income (K)", ylab = "Frequency", col = "#E69F00", labels = TRUE)

# Density Plot
plot(density(data$Annual_Income), main = "Annual Income Density Plot", xlab = "Annual Income (K)", ylab = "Density")
polygon(density(data$Annual_Income), col="#E69F00")

# Annual Income Distribution vs Gender
ggplot(data, aes(x = Annual_Income, fill = Gender)) + geom_histogram(col = "black") + facet_grid(Gender ~.) + ggtitle('Annual Income Distribution by Gender')

# Boxplot for Gender vs Annual Income
ggplot(data, aes(x = Gender, y = Annual_Income, fill = Gender)) + geom_boxplot() + ggtitle("Annual Income vs Gender") + theme(legend.position = "bottom")

# Spending Score Distribution
hist(data$Spending_Score, main = "Spending Score Distribution", xlab = "Spending Score", ylab = "Frequency", col = "#2CA25F", labels = TRUE)

# Spending Score Distribution vs Gender
ggplot(data, aes(x = Spending_Score, fill = Gender)) + geom_histogram(col = "black") + facet_grid(Gender ~.) + ggtitle('Spending Score Distribution by Gender')

# Boxplot for Gender vs Spending Score
ggplot(data, aes(x = Gender, y = Spending_Score, fill = Gender)) + geom_boxplot() + ggtitle("Spending Score vs Gender") + theme(legend.position = "bottom")

############################################################################################################

## K-means CLustering

# Determine Optimal Clusters

## 1. ELbow Method
library(purrr)
set.seed(100)

# calculate total Intra-cluster sum of square (iss)

iss <- function(k) {
  kmeans(data[,3:5], k, iter.max = 100, nstart = 100, algorithm = "Lloyd" )$tot.withinss
}

k.values <- 1:10

iss_values <- map_dbl(k.values, iss)

#plot
plot(k.values, iss_values, type ="b", pch = 19, frame = FALSE, xlab = "Number of Clusters k", ylab = "Total Intra-clusters Sum of Squares (iss)")

## 2. Average Silhouette Method
library(cluster) 
library(gridExtra)
library(grid)

k_1 <- kmeans(data[,3:5], 2, iter.max = 100, nstart = 50, algorithm = "Lloyd")
s_1 <- plot(silhouette (k_1$cluster, dist(data[,3:5], "euclidean")))

k_2 <- kmeans(data[,3:5], 3, iter.max = 100, nstart = 50, algorithm = "Lloyd")
s_2 <- plot(silhouette (k_2$cluster, dist(data[,3:5], "euclidean")))

k_3 <- kmeans(data[,3:5], 4, iter.max = 100, nstart = 50, algorithm = "Lloyd")
s_3 <- plot(silhouette (k_3$cluster, dist(data[,3:5], "euclidean")))

k_4 <- kmeans(data[,3:5], 5, iter.max = 100, nstart = 50, algorithm = "Lloyd")
s_4 <- plot(silhouette (k_4$cluster, dist(data[,3:5], "euclidean")))

k_5 <- kmeans(data[,3:5], 6, iter.max = 100, nstart = 50, algorithm = "Lloyd")
s_5 <- plot(silhouette (k_5$cluster, dist(data[,3:5], "euclidean")))

k_6 <- kmeans(data[,3:5], 7, iter.max = 100, nstart = 50, algorithm = "Lloyd")
s_6 <- plot(silhouette (k_6$cluster, dist(data[,3:5], "euclidean")))

k_7 <- kmeans(data[,3:5], 8, iter.max = 100, nstart = 50, algorithm = "Lloyd")
s_7 <- plot(silhouette (k_7$cluster, dist(data[,3:5], "euclidean")))

k_8 <- kmeans(data[,3:5], 9, iter.max = 100, nstart = 50, algorithm = "Lloyd")
s_8 <- plot(silhouette (k_8$cluster, dist(data[,3:5], "euclidean")))

k_9 <- kmeans(data[,3:5], 10, iter.max = 100, nstart = 50, algorithm = "Lloyd")
s_9 <- plot(silhouette (k_9$cluster, dist(data[,3:5], "euclidean")))

# Plot
library(NbClust)
library(factoextra)

fviz_nbclust(data[,3:5], kmeans, method = "silhouette")

## 3. Gap Statistic Method

set.seed(111)
stat_gap <- clusGap(data[,3:5], FUN = kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(stat_gap)

# Selecting k=6 as optimal number of clusters
k6 <- kmeans(data[,3:5], 6, iter.max = 100, nstart = 50, algorithm = "Lloyd")
k6

# Let's look at clusters using PCA

pcclust <- prcomp(data[,3:5], scale = FALSE)
summary(pcclust)
pcclust$rotation[,1:2]

# Cluster plot for Annual Income and Spending Score
library(ggplot2)
set.seed(121)
ggplot(data, aes(x = Annual_Income, y = Spending_Score)) + geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name = " ", breaks = c("1", "2", "3", "4", "5","6"), labels = c("Cluster - 1", "Cluster - 2", "Cluster - 3", "Cluster - 4", "Cluster - 5","Cluster - 6")) +
  ggtitle("Customers grouped by Annual Income and Spending Score")

# Cluster plot for Spending Score and Age
ggplot(data, aes(x = Spending_Score, y = Age)) + geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name = " ", breaks = c("1", "2", "3", "4", "5","6"), labels = c("Cluster - 1", "Cluster - 2", "Cluster - 3", "Cluster - 4", "Cluster - 5","Cluster - 6")) +
  ggtitle("Customers grouped by Spending Score and Age")

#plots by PCA
kCols = function(vec){cols = rainbow(length(unique(vec)))
return (cols[as.numeric(as.factor(vec))])}
digCluster <- k6$cluster
dignm <- as.character(digCluster) # K-means clusters

plot(pcclust$x[,1:2], col = kCols(digCluster), pch = 19, xlab = "K-means", ylab = "classes")
legend("bottomright", unique(dignm), fill = unique(kCols(digCluster)))

