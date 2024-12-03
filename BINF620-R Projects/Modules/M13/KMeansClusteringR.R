#https://www.statsandr.com/blog/clustering-analysis-k-means-and-hierarchical-clustering-by-hand-and-in-r/

install.packages("NbClust")
install.packages("factoextra")

library(NbClust)
library(factoextra)

Eurojobs <- read.csv("C:/Users/zugui/Desktop/work/UDFall2020/Module11/Eurojobs.csv", stringsAsFactors=TRUE)
#View(Eurojobs)
head(Eurojobs)
dim(Eurojobs)
#Eurojobs=Eurojobs[,-1]
model <- kmeans(Eurojobs, centers = 2)
print(model$cluster)
Eurojobs <- read.csv(
  file = "Eurojobs.csv",
  sep = ",", dec = ".", header = TRUE, row.names = 1
)
model <- kmeans(Eurojobs, centers = 2)

Eurojobs_cluster <- data.frame(Eurojobs,
                               cluster = as.factor(model$cluster)
)
head(Eurojobs_cluster)

# BSS and TSS are extracted from the model and stored
BSS <- model$betweenss
TSS <- model$totss
BSS / TSS * 100

model2 <- kmeans(Eurojobs, centers = 2, nstart = 10)
100 * model2$betweenss / model2$totss

model3 <- kmeans(Eurojobs, centers = 3)
BSS3 <- model3$betweenss
TSS3 <- model3$totss
BSS3 / TSS3 * 100

# load required packages
library(factoextra)
library(NbClust)

# Elbow method
fviz_nbclust(Eurojobs, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) + # add line for better visualisation
  labs(subtitle = "Elbow method") # add subtitle

# Silhouette method
fviz_nbclust(Eurojobs, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

# Gap statistic
set.seed(42)
fviz_nbclust(Eurojobs, kmeans,
             nstart = 25,
             method = "gap_stat",
             nboot = 500
) + # reduce it for lower computation time (but less precise results)
  labs(subtitle = "Gap statistic method")

nbclust_out <- NbClust(
  data = Eurojobs,
  distance = "euclidean",
  min.nc = 2, # minimum number of clusters
  max.nc = 5, # maximum number of clusters
  method = "kmeans" # one of: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid", "kmeans"
)

# create a dataframe of the optimal number of clusters
nbclust_plot <- data.frame(clusters = nbclust_out$Best.nc[1, ])
# select only indices which select between 2 and 5 clusters
nbclust_plot <- subset(nbclust_plot, clusters >= 2 & clusters <= 5)

# create plot
ggplot(nbclust_plot) +
  aes(x = clusters) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  labs(x = "Number of clusters", y = "Frequency among all indices", title = "Optimal number of clusters") +
  theme_minimal()

#Based on all 30 indices, the best number of clusters is 3/2 clusters.
library(factoextra)

km_res <- kmeans(Eurojobs, centers = 2, nstart = 20)
fviz_cluster(km_res, Eurojobs, ellipse.type = "norm")

km_res <- kmeans(Eurojobs, centers = 3, nstart = 20)
fviz_cluster(km_res, Eurojobs, ellipse.type = "norm")
