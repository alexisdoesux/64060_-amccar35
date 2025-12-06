############################################################
# BA 64060 Final Project 
# Author: Alexis McCartney, Kristen Durkin, Marvelle Horton
#
# This script does:
# 1) Load and clean 2023 EIA-923 YTD data (Group3Data.csv)
# 2) Create an efficiency feature (MMBtu per MWh)
# 3) Scale variables for clustering
# 4) Split into train/test (75/25) per assignment
# 5) Use elbow and silhouette methods to pick k
# 6) Run k-means with k = 3
# 7) Summarize and visualize the clusters

############################################################
# 1. Load libraries
############################################################

# dplyr: data wrangling
# ggplot2: plotting
# cluster: silhouette calculations
# factoextra: convenient cluster visualizations
library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)

############################################################
# 2. Load and clean the data
############################################################

# I’m reading in the CSV that has the last 6 YTD columns
# from the EIA-923 Page 1 Generation and Fuel Data sheet.
# This file should be in my working directory.
df <- read.csv("Group3Data.csv", check.names = FALSE)

# Rename columns to something shorter and easier to read.
colnames(df) <- c(
  "total_fuel_qty",      # Total Fuel Consumption Quantity (YTD)
  "elec_fuel_qty",       # Electric Fuel Consumption Quantity (YTD)
  "total_fuel_mmbtu",    # Total Fuel Consumption MMBtu (YTD)
  "elec_fuel_mmbtu",     # Electric Fuel Consumption MMBtu (YTD)
  "net_gen_mwh",         # Net Generation (MWh, YTD)
  "Year"                 # Year
)

# A lot of these values are stored as character strings with commas.
# Here I strip commas and convert everything to numeric where needed.
df_clean <- as.data.frame(
  lapply(df, function(x) {
    if (is.character(x)) {
      as.numeric(gsub(",", "", x))
    } else {
      x
    }
  })
)

# Now I keep only:
# - rows from 2023
# - rows with non-missing fuel + generation
# - rows where net generation is > 0 (to avoid divide-by-zero)
df_2023 <- df_clean %>%
  filter(
    Year == 2023,
    !is.na(total_fuel_mmbtu),
    !is.na(net_gen_mwh),
    net_gen_mwh > 0
  )

# Create a new engineered feature:
# efficiency_mmbtu_per_mwh = total_fuel_mmbtu / net_gen_mwh
# This measures how many MMBtu of fuel are used per MWh produced.
df_2023 <- df_2023 %>%
  mutate(efficiency_mmbtu_per_mwh = total_fuel_mmbtu / net_gen_mwh)

# Quick sanity checks
head(df_2023)
str(df_2023)
summary(df_2023$efficiency_mmbtu_per_mwh)

############################################################
# 3. Select variables and scale them
############################################################

# I’m clustering on six features:
# - total_fuel_qty
# - elec_fuel_qty
# - total_fuel_mmbtu
# - elec_fuel_mmbtu
# - net_gen_mwh
# - efficiency_mmbtu_per_mwh
#
# Because these variables are on very different scales, I standardize
# them (mean ~ 0, sd ~ 1). This keeps any single variable from
# dominating the distance calculations.
cluster_vars <- df_2023 %>%
  select(
    total_fuel_qty,
    elec_fuel_qty,
    total_fuel_mmbtu,
    elec_fuel_mmbtu,
    net_gen_mwh,
    efficiency_mmbtu_per_mwh
  )

cluster_scaled <- scale(cluster_vars)

# Confirm scaling: means should be ~0 and sds should be ~1
colMeans(cluster_scaled)
apply(cluster_scaled, 2, sd)

############################################################
# 4. Train/Test split (75% / 25%)
############################################################

# Even though this is unsupervised, the assignment requires a split,
# so I randomly assign 75% of rows to a "train" set and 25% to "test".
set.seed(123)  # for reproducibility

n <- nrow(cluster_scaled)
train_idx <- sample(1:n, size = 0.75 * n)

train <- cluster_scaled[train_idx, ]
test  <- cluster_scaled[-train_idx, ]

dim(train)  # training dimensions
dim(test)   # test dimensions

############################################################
# 5. Elbow method to choose k
############################################################

# I compute total within-cluster sum of squares (WSS) for k = 1 to 10.
# The "elbow" is the point where adding more clusters stops giving
# big decreases in WSS.
wss <- sapply(1:10, function(k) {
  kmeans(train, centers = k, nstart = 20)$tot.withinss
})

# Plot WSS vs k so I can visually inspect the elbow.
plot(
  1:10, wss, type = "b",
  xlab = "Number of Clusters (k)",
  ylab = "Total Within-Cluster Sum of Squares (WSS)",
  main = "Elbow Method for Choosing k"
)

############################################################
# 6. Silhouette method to choose k
############################################################

# I also compute the average silhouette width for k = 2 to 10.
# Higher silhouette values indicate better cluster separation and cohesion.
avg_sil <- sapply(2:10, function(k) {
  km <- kmeans(train, centers = k, nstart = 20)
  ss <- silhouette(km$cluster, dist(train))
  mean(ss[, 3])
})

# Plot average silhouette width by k.
plot(
  2:10, avg_sil, type = "b",
  xlab = "Number of Clusters (k)",
  ylab = "Average Silhouette Width",
  main = "Silhouette Method for Choosing k"
)

# Inspect the silhouette values to see which k performs best.
avg_sil

# Based on the elbow curve and silhouette pattern, I choose:
k_opt <- 3

############################################################
# 7. Final k-means model with k = 3
############################################################

# I run k-means on the training data using k = 3 clusters and
# multiple random starts (nstart = 25) for stability.
set.seed(123)
km_final <- kmeans(train, centers = k_opt, nstart = 25)

# Cluster sizes show how many units fall into each group.
km_final$size

# Cluster centers summarize the average standardized profile for each cluster.
km_final$centers

############################################################
# 8. Attach clusters back to original data
############################################################

# I take the original (unscaled) df_2023 rows corresponding to the
# train indices and add the cluster assignments. This lets me interpret
# the clusters in real-world units (MMBtu, MWh, etc.).
train_original <- df_2023[train_idx, ]
train_original$cluster <- km_final$cluster

head(train_original)

############################################################
# 9. Summarize each cluster
############################################################

# Here I create a summary table by cluster that includes:
# - Number of units
# - Average total fuel MMBtu
# - Average net generation (MWh)
# - Average efficiency (MMBtu per MWh)
#
# This is what I would talk through in my presentation as
# "what each cluster actually represents."
cluster_summary <- train_original %>%
  group_by(cluster) %>%
  summarise(
    n_units = n(),
    avg_total_fuel_mmbtu = mean(total_fuel_mmbtu, na.rm = TRUE),
    avg_net_gen_mwh = mean(net_gen_mwh, na.rm = TRUE),
    avg_efficiency_mmbtu_mwh = mean(efficiency_mmbtu_per_mwh, na.rm = TRUE),
    .groups = "drop"
  )

cluster_summary

############################################################
# 10. Visualize clusters in reduced space
############################################################

# This plot uses factoextra to show the clusters in a reduced
# 2D space. Each point is a plant/unit and colored by its
# assigned cluster. The convex hulls make the grouping clearer.
fviz_cluster(
  list(data = train, cluster = km_final$cluster),
  geom = "point",
  ellipse.type = "convex",
  main = "K-Means Clustering of Power Units (k = 3)"
)

############################################################
# 11. Visualize efficiency vs. generation by cluster
############################################################

# This scatter plot compares net generation (MWh) versus efficiency
# (MMBtu per MWh). Coloring by cluster lets me see which groups are:
# - high output and high intensity,
# - lower output and less efficient,
# - or more balanced in terms of fuel use per MWh.
ggplot(
  train_original,
  aes(
    x = net_gen_mwh,
    y = efficiency_mmbtu_per_mwh,
    color = factor(cluster)
  )
) +
  geom_point(alpha = 0.5) +
  labs(
    title = "Clusters by Net Generation and Fuel Efficiency",
    x = "Net Generation (MWh)",
    y = "Efficiency (MMBtu per MWh)",
    color = "Cluster"
  )


