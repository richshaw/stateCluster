
# Load data
data <- read.csv(file="censusStateClean.csv", header=T, sep=",", row.names=1)
# Scale data because using different units
data2 <- data.frame(scale(data))
# Run PCA
pc <- prcomp(data2)
# Choose number of principal compnets that account for > 85% of variance or underneath elbow
plot(pc, type='l')
summary(pc)
# User first 5 components
comp <- data.frame(pc$x[,1:5])
#K-means cluster
k <- kmeans(comp, 4, nstart=25, iter.max=1000)
# Plot clusters across components
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp, col=k$clust, pch=16)
# Cluster sizes
sort(table(k$clust))
clust <- names(sort(table(k$clust)))
# First cluster
row.names(data[k$clust==clust[1],])
# Second Cluster
row.names(data[k$clust==clust[2],])
# Third Cluster
row.names(data[k$clust==clust[3],])
# Fourth Cluster
row.names(data[k$clust==clust[4],])

# > # First cluster
#     > row.names(data[k$clust==clust[1],])
# [1] "California" "Florida"    "New York"   "Texas"     
# > # Second Cluster
#     > row.names(data[k$clust==clust[2],])
# [1] "Connecticut"   "Delaware"      "Illinois"      "Maryland"      "Massachusetts" "New Jersey"   
# [7] "Rhode Island"  "Virginia"     
# > # Third Cluster
#     > row.names(data[k$clust==clust[3],])
# [1] "Colorado"      "Idaho"         "Iowa"          "Kansas"        "Maine"         "Minnesota"    
# [7] "Montana"       "Nebraska"      "Nevada"        "New Hampshire" "North Dakota"  "Oregon"       
# [13] "South Dakota"  "Utah"          "Vermont"       "Wisconsin"     "Wyoming"      
# > # Fourth Cluster
#     > row.names(data[k$clust==clust[4],])
# [1] "Alabama"        "AriNAona"       "Arkansas"       "Georgia"        "Indiana"        "Kentucky"      
# [7] "Louisiana"      "Michigan"       "Mississippi"    "Missouri"       "New Mexico"     "North Carolina"
# [13] "Ohio"           "Oklahoma"       "Pennsylvania"   "South Carolina" "Tennessee"      "West Virginia" 