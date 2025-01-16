# Urban-Development-Project
## For this project my team and I aimed to provide an Urban Development company with a virtual product that could help them making-decisions about which urban element to include in newly constructed Dutch neighbourhoods in order to increase the wellbeing of the citizens. The output of this project was a Shiny App that showed how certain urban design choices affect social indicators such as the happiness and social cohesion of residents. Below you can see the main steps taken to reach the result:

## Data Collection - Data was collected from several publicly available sources and cleaned then given the dimension of the dataset, non-relevant columns had to be deleted to avoid multicollinearity issues. In order to do this a correlation analysis was performed


    threshold <- 0.2 # Insignificant correlations are 0 thus below threshold
    
    filtered_matrix <-
    significant_correlations[abs(significant_correlations$Social_cohesion_21) >=
    threshold, ]
    transposed_matrix <- t(filtered_matrix)
    transposed_matrix <- as.data.frame(transposed_matrix)
    cormatrix_refined <-
    transposed_matrix[abs(transposed_matrix$Social_cohesion_21) >= threshold, ]

## To strengthen the correlations, hierarchical clustering was used and VIF values checked 

    cormatrix_refined[abs(cormatrix_refined) == 1] <- 0
    # Compute the distance matrix using Euclidean distance
    dist_matrix <- dist(cormatrix_refined, method = "euclidean")
    # Perform hierarchical clustering
    hc <- hclust(dist_matrix, method = "average")
    # Plot the dendrogram
    plot(hc, labels = rownames(cormatrix_refined), main = "Hierarchical Clustering
    Dendrogram", sub = "", xlab = "")
    
    number_of_clusters_in_dataset <- max(features$cluster)
    # Iterate over clusters to split the data, run regressions, and get the vifs
    for (cluster_number in 1:number_of_clusters_in_dataset) {
    
     # Find the right indeces to split data by cluster and add the major
    indicator, if it is not already included in the cluster
     cluster_indices <- which(features$cluster == cluster_number)
     target_variable_index <- which(features[,1] == target_variable)
     logical <- 0
     # Flag will become 1 for the one cluster that included the major indicators
    so that we won't add it again
     for (index in 1:length(cluster_indices)) {
     if (cluster_indices[index] == target_variable_index) {
     logical <- logical + 1
     }
     }

## Consequently Missing values were handled 
    # Search each column for it's skewness, and then for being an integer
    variable
    for (i in 1:ncol(data)) {
     # To avoid errors for variables with 0 skewness fill in 0 manually
     skewness_current <- skewness(data[,i], na.rm = TRUE)
     if (is.na(skewness_current) == TRUE) {
     skewness_current <- 0
     }
     if (abs(skewness_current) < 0.5) { # Low skewness
     if (are_all_integers(data[,i]) == TRUE) { # Integer values
     # Replace NAs with integer mean
     data_clean[,i][is.na(data[,i])] <- round(mean(data[,I]))
     } else {
     # Replace NAs with mean
     data_clean[,i][is.na(data[,i])] <- mean(data[,i], na.rm = TRUE)
     }
    
     } else if (abs(skewness_current) < 1) { # Medium skewness
     z <- as.numeric(i) # corret variable index type to avoid error
     if (are_all_integers(data[,i]) == TRUE) {
     # Replace NAs with integer 5-NN mean
     data_clean[,i] <- round(kNN(data, variable = z, k = 5)[,i])
     } else {
     # Replace NAs with 5-NN mean
     data_clean[,i] <- kNN(data, variable = z, k = 5)[,i]
     }
     } else {
     if (are_all_integers(data[,i]) == TRUE) { # High skewness
     # Replace NAs with integer medium
     data_clean[,i][is.na(data[,i])] <- round(median(data[,i], na.rm =
    TRUE))
     } else {
     # Replace NAs with medium
     data_clean[,i][is.na(data[,i])] <- median(data[,i], na.rm = TRUE)
     }
     }
    }
## Distribution was checked

    data_clean <-read.xlsx(savepath, rowNames = TRUE)
    # Summary statistics for numerical data
    summary(data_clean$Social_cohesion_21)
    ## Min. 1st Qu. Median Mean 3rd Qu. Max.
    ## 4.800 5.500 5.931 5.931 6.425 7.100
    # Histogram target variable
    hist(data_clean$Social_cohesion_21, breaks = 20, main = "Major Indicator",
    xlab = "Target Variable", col = "gray", xlim = c(0, 10)) # Adjust xlim with
    both scale edges




















