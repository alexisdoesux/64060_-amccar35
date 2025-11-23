r
cereals <- read.csv("Cereals.csv")
r
str(cereals)

# Remove cereals with missing values
cereals_clean <- na.omit(cereals)

r
nrow(cereals_clean)

# Normalize the numeric variables
r
cereals_num <-cereals_clean[, sapply(cereals_clean, is.numeric)]

r
cereals_norm <- scale(cereals_num)

# Load the cluster package
r
install.packages("cluster")
library(cluster)

# Compute the distance matrix with Euclidean distance
r
dist_matrix <- dist(cereals_norm, method = "euclidean")

# Apply AGNES for four linkage methods
r
agnes_single <- agnes(cereals_norm, method = "single")
agnes_complete <- agnes(cereals_norm, method = "complete")
agnes_average <- agnes(cereals_norm, method = "average")
agnes_ward <- agnes(cereals_norm, method = "ward")

#Compare the agglomerative coefficient
r
agnes_single$ac
agnes_complete$ac
agnes_average$ac
agnes_ward$ac

#Dendrograms plot
r
par(mfrow=c(2,2))
plot(agnes_single, main="Single Linkage")
plot(agnes_complete, main="Complete Linkage")
plot(agnes_average, main="Average Linkage")
plot (agnes_ward, main="Ward's Method")

r
plot(agnes_ward, main="Ward's Method")

# I selected 4 clusters since the dendrogram from Ward's Method shows a separation into four major branches before a large height jump, which leads me to conclude a natural grouping structure for this dataset.

r
set.seed(123) # for reproducibility
index <-sample(1:nrow(cereals_norm), nrow(cereals_norm)/2)

r
index

# 1. Clean the original data
cereals_clean <- na.omit(cereals)
cereals_num <- cereals_clean[, sapply(cereals_clean, is.numeric)]
cereals_norm <- scale(cereals_num)

# 2. Partition into A and B
set.seed(123)
index <- sample(1:nrow(cereals_norm), nrow(cereals_norm)/2)
A <- cereals_norm[index, ]
B <- cereals_norm[-index, ]

# 3. Cluster Partition A
library(cluster)
agnes_A <- agnes(A, method = "ward")
clusters_A <- cutree(agnes_A, k = 4)

# 4. Display cluster sizes in A
table(clusters_A)

# Compute centroids for the 4 clusters in A
centroids_A <- aggregate(A, by = list(cluster = clusters_A), mean)
centroids_A

# Turn centroids into numeric matrix (drop the first column which is just the cluster label)
centroids_mat <- as.matrix(centroids_A[, -1])

# Compute distances from each row in B to each centroid
dist_B_to_centroids <- as.matrix(dist(rbind(centroids_mat, B)))[-(1:4), 1:4]

# For each row in B, choose the nearest centroid (cluster)
assigned_B <- apply(dist_B_to_centroids, 1, which.min)

# See how many B records got assigned to each cluster
table(assigned_B)

# Full-data clusters using Ward and 4 clusters (on all cereals_norm)
full_clusters <- cutree(agnes_ward, k = 4)

# Take the same B rows from the full clustering
full_clusters_B <- full_clusters[-index]

# Compare: Assigned via A's centroids vs full-data clustering
table(assigned_B, full_clusters_B)

