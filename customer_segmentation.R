customer_data=read.csv("D:/Downloads/RStudio/Mall_Customer.csv")
customer_data_preprocessing=read.csv("D:/Downloads/RStudio/Mall_Customer.csv")
str(customer_data)
names(customer_data)
for(i in 1:ncol(customer_data)){
  customer_data[is.na(customer_data[,i]), i] <- mean(customer_data[,i], na.rm = TRUE)
}
for(i in 1:ncol(customer_data_preprocessing)){
  customer_data_preprocessing[is.na(customer_data_preprocessing[,i]), i] <- mean(customer_data_preprocessing[,i], na.rm = TRUE)
}
install.packages("tidyr")
install.packages("dplyr")
library(tidyr)
library(dplyr)
require(tidyr)
require(dplyr)

customer_data_preprocessing = customer_data_preprocessing %>% mutate(value = 1)  %>% spread(Qualifications, value,  fill = 0 ) 
customer_data_preprocessing = customer_data_preprocessing %>% mutate(value = 1)  %>% spread(Gender, value,  fill = 0 )
head(customer_data_preprocessing)
#calculate principal components
results <- prcomp(customer_data_preprocessing, scale = TRUE)

#reverse the signs
results$rotation <- -1*results$rotation

#display principal components
results$rotation

head(customer_data)
age_18 <- customer_data[which(customer_data$Age<0),]
dim(age_18)

customer_data <- customer_data[-which(customer_data$Age<0),]
dim(customer_data)

summary(customer_data$Age)

head(customer_data)

sd(customer_data$Age)
summary(customer_data$Annual.Income..k..)
sd(customer_data$Annual.Income..k..)
summary(customer_data$Age)


a=table(customer_data$Gender)
barplot(a,main="Using BarPlot to display Gender Comparision",
       ylab="Count",
       xlab="Gender",
       col=rainbow(2),
       legend=rownames(a))

install.packages('plotrix')
library(plotrix)


pct=round(a/sum(a)*100)
lbs=paste(c("Female","Male")," ",pct,"%",sep=" ")
library(plotrix)
pie3D(a,labels=lbs,
   main="Pie Chart Depicting Ratio of Female and Male")

hist(customer_data$Age,
    col="blue",
    main="Histogram to Show Count of Age Class",
    xlab="Age Class",
    ylab="Frequency",
    labels=TRUE)

summary(customer_data$YearlyIncome)
hist(customer_data$YearlyIncome,
  col="#660033",
  main="Histogram for Annual Income",
  xlab="Annual Income Class",
  ylab="Frequency",
  labels=TRUE)


plot(density(customer_data$YearlyIncome),
    col="yellow",
    main="Density Plot for Annual Income",
    xlab="Annual Income Class",
    ylab="Density")
polygon(density(customer_data$YearlyIncome),
        col="#ccff66")

hist(customer_data$Spending.Score..1.100.,
    main="HistoGram for Spending Score",
    xlab="Spending Score Class",
    ylab="Frequency",
    col="#6600cc",
    labels=TRUE)

 t.test(YearlyIncome ~ Gender, data = customer_data)

wilcox.test(YearlyIncome ~ Gender, data = customer_data,alternative = 'greater')
head(customer_data)

library(purrr)
set.seed(123)
# function to calculate total intra-cluster sum of square 
iss <- function(k) {
  kmeans(customer_data[,3:5],k,iter.max=100,nstart=100,algorithm="Lloyd" )$tot.withinss
}

k.values <- 1:10


iss_values <- map_dbl(k.values, iss)

plot(k.values, iss_values,
    type="b", pch = 19, frame = FALSE, 
    xlab="Number of clusters K",
    ylab="Total intra-clusters sum of squares")

install.packages("factoextra")
library(factoextra)

install.packages("ClusterR")
install.packages("cluster")
library(cluster)

k2 <- kmeans(customer_data[,3:4], centers = 3, nstart = 25)
k3 <- kmeans(customer_data[,3:4], centers = 3, nstart = 25)
k4 <- kmeans(customer_data[,3:4], centers = 4, nstart = 25)
k5 <- kmeans(customer_data[,3:4], centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = customer_data[,3:4]) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = customer_data[,3:4]) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = customer_data[,3:4]) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = customer_data[,3:4]) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

library(cluster)
install.packages("fpc")
library(fpc)
install.packages("dbscan")
library(dbscan)
library(factoextra)

customer_prep = customer_data[3:4]

# scaling the dataset
customer_prep = scale(customer_prep)
customer_prep %>% head()


eps_plot = kNNdistplot(customer_prep, k=3)

eps_plot %>% abline(h = 0.45, lty = 2)

set.seed(50)

# creation of an object km which store the output of the function kmeans
d <- dbscan::dbscan(customer_prep, eps = 0.45, MinPts =  2)
d

# cluster visualisation
fviz_cluster(d, customer_prep, geom = "point")