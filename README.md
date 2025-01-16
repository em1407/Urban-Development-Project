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

## Random Forest for feature selection and metrics
        # Fit a Random Forest model
         rffeatures <- randomForest(train_data$Social_cohesion_21 ~ ., data =
        train_data, ntree = 500, mtry = 5, importance = TRUE)
        
         if (i == 1) { # Initialize importance matrix
         importance_matrix <- as.numeric(matrix(NA, nrow = ncol(data_fixed) - 1,
        ncol = 1))
         # Store feature importance
         importance_matrix <- importance(rffeatures)[,1]
         } else {
         # Store recurrent feature importances
         importance_matrix <- cbind(importance_matrix, importance(rffeatures)[,1])
         }
        
         # Make predictions on train dataset
         predictionsrf[i] <- predict(rffeatures, test_data, type = "response")
        
         # MSE
         mserf[i] <- (predictionsrf[i] - test_data$Social_cohesion_21)^2
        
         # MSE
         maerf[i] <- MAE(predictionsrf[i], test_data$Social_cohesion_21)
         # MAPE
         maperf[i] <- mean(abs((test_data$Social_cohesion_21 - predictionsrf[i]) /
        test_data$Social_cohesion_21) * 100)
        
         # Accuracy on train set
         accuraciesrf[i] <- Accuracy(round(predictionsrf[i],1),
        round(test_data$Social_cohesion_21, 1))
        }
## Subset data - take the features that you used in the model. When you rerun the code you will gradually lower the number of features and keep the top picks based on the results
        column_refinement <- match(rownames(importances), names(datafull))
        data_fixed <- datafull[, column_refinement]
        # Add target variable to the set
        data_fixed <- cbind(datafull$Social_cohesion_21, datafull[,
        column_refinement])
        colnames(data_fixed)[1] <- "Social_cohesion_21"

## Using RFE to add additional meaningful (urban) features in our feature selection
        # Creating bins
        quantile_breaks <- quantile(indicator, probs = seq(0, 1, by = 0.2))
        # Cutting the data into these quantiles
        indicator_categories <- cut(indicator, breaks = quantile_breaks,
        include.lowest = TRUE, labels = FALSE)
        data <- data %>%
         # Save categorical features as factors
         mutate_at(colnames(data),
         as.factor) %>%
         # Center and scale numeric features
         mutate_if(is.numeric, scale)
        # Define the control using a random forest selection function
        control <- rfeControl(functions = rfFuncs, # random forest
         method = "repeatedcv", # repeated cv
## Lastly, from the Variables the model automatically suggests we chose manually the top 50

## We then tried different ML models
    #RANDOM FOREST
        predictionsrf <- vector("numeric", length = nrow(data_fixed))
        accuraciesrf <- vector("numeric", length = nrow(data_fixed)) # will not be
        taken into account
        mserf <- vector("numeric", length = nrow(data_fixed))
        maerf <- vector("numeric", length = nrow(data_fixed))
        maperf <- vector("numeric", length = nrow(data_fixed))
        # Perform LOOCV
        for (i in 1:nrow(data_fixed)) {
         # Create the training set by excluding the ith observation
         train_data <- data_fixed[-i, ]
        
         # Create the test set with only the ith observation
         test_data <- data_fixed[i, ]
        
         # Fit a Random Forest model
         rffeatures <- randomForest(train_data$Social_cohesion_21 ~ ., data =
        train_data, ntree = 1000, mtry = 10, importance = TRUE)
        
         if (i == 1) {
         # Initialize importances table
         importance_matrix <- as.numeric(matrix(NA, nrow = ncol(data_fixed) - 1,
        ncol = 2))
         # Store feature importance
         importance_matrix <- importance(rffeatures)[,1]
         } else {
         # Store recurrent feature importances
        importance_matrix <- cbind(importance_matrix, importance(rffeatures)[,1])
         }
        
        31
         # Make predictions on train dataset
         predictionsrf[i] <- predict(rffeatures, test_data, type = "response")
        
         # MSE
         mserf[i] <- (predictionsrf[i] - test_data$Social_cohesion_21)^2
    #REGRESSION
        predictionsreg <- vector("numeric", length = nrow(data_fixed))
        accuraciesreg <- vector("numeric", length = nrow(data_fixed))
        msereg <- vector("numeric", length = nrow(data_fixed))
        maereg <- vector("numeric", length = nrow(data_fixed))
        mapereg <- vector("numeric", length = nrow(data_fixed))
        # Perform LOOCV
        for (i in 1:nrow(data_fixed)) {
         train_data <- data_fixed[-i, ]
         test_data <- data_fixed[i, ]
         # Fit a Random Forest model
         regfeatures <- lm(train_data$Social_cohesion_21 ~ ., data = train_data)
         predictionsreg[i] <- predict(regfeatures, test_data, type = "response")
         # Metrics
         msereg[i] <- (predictionsreg[i] - test_data$Social_cohesion_21)^2
         maereg[i] <- MAE(predictionsreg[i], test_data$Social_cohesion_21)
         mapereg[i] <- mean(abs((test_data$Social_cohesion_21 - predictionsreg[i]) /
        test_data$Social_cohesion_21) * 100)
         accuraciesreg[i] <- Accuracy(round(predictionsreg[i], 1), round(test_data$Social_cohesion_21, 1))
        }
        
    #SVR
        predictionssvr <- vector("numeric", length = nrow(data_fixed))
        accuraciessvr <- vector("numeric", length = nrow(data_fixed))
        msesvr <- vector("numeric", length = nrow(data_fixed))
        maesvr <- vector("numeric", length = nrow(data_fixed))
        mapesvr <- vector("numeric", length = nrow(data_fixed))
        # Perform LOOCV
        for (i in 1:nrow(data_fixed)) {
         train_data <- data_fixed[-i, ]
         test_data <- data_fixed[i, ]
         # Fit an svr model
         svrfeatures <- svm(Social_cohesion_21 ~ . -Social_cohesion_21, data =
        train_data, type = 'eps-regression', kernel = 'radial', epsilon = 0.1)
         predictionssvr[i] <- predict(svrfeatures, test_data, type = "response") 
         
    # Ridge Regression 
        ridge_regression <- glmnet(x = train_x, y = train_y, family = "gaussian",
        alpha = 0, lambda = lambda_best)

    # GBM 
        predictionsgbm <- vector("numeric", length = nrow(data_fixed))
        accuraciesgbm <- vector("numeric", length = nrow(data_fixed))
        msegbm <- vector("numeric", length = nrow(data_fixed))
        maegbm <- vector("numeric", length = nrow(data_fixed))
        mapegbm <- vector("numeric", length = nrow(data_fixed))
        # Perform LOOCV
        for (i in 1:nrow(data_fixed)) {
         train_data <- data_fixed[-i, ]
         test_data <- data_fixed[i, ]
         # Fit the model
         set.seed(42) # for reproducibility
         gbmfeatures = gbm(train_data$Social_cohesion_21 ~ ., data = train_data,
        distribution = "gaussian", n.trees = 1000, interaction.depth = 4, shrinkage =
        0.01, cv.folds = 5, n.minobsinnode = 5)
         # Make predictions on train dataset
         predictionsgbm[i] <- predict(gbmfeatures, test_data, type = "response")
         # Metrics
         msegbm[i] <- (predictionsgbm[i] - test_data$Social_cohesion_21)^2
         maegbm[i] <- MAE(predictionsgbm[i], test_data$Social_cohesion_21)
         mapegbm[i] <- mean(abs((test_data$Social_cohesion_21 - predictionsgbm[i]) /
        test_data$Social_cohesion_21) * 100)
         accuraciesgbm[i] <- Accuracy(round(predictionsgbm[i], 1),
        round(test_data$Social_cohesion_21, 1))
        }

## Lastly we summarised the metrics of all the models to choose the best performing one

        results <- data.frame(
         MAE = c(round(maerf, 3), round(maereg, 3), round(maesvr, 3),
        round(maerreg_mean, 3), round(maepreg, 3), round(maegbm, 5)),
         MSE = c(round(mserf, 5), round(msereg, 5), round(msesvr, 5),
        round(mserreg_mean, 5), round(msepreg, 5), round(msegbm, 5)),
         MAPE = c(round(maperf, 5), round(mapereg, 5), round(mapesvr, 5),
        round(maperreg_mean, 5), round(mapepreg, 5), round(mapegbm, 5)),
         R_squared = c(round(rsquaredrf, 3), round(rsquaredreg, 3),
        round(rsquaredsvr, 3), round(rsquaredrreg, 3), round(rsquaredpreg, 3),
        round(rsquaredgbm, 3)),
         R_squared_adjusted = c(round(rsquaredrfAdj, 3), round(rsquaredregAdj, 3),
        round(rsquaredsvrAdj, 3), round(rsquaredrregAdj, 3), round(rsquaredpregAdj,
        3), round(rsquaredgbmAdj, 3)),
         row.names = c("RF", "Reg", "SVR", "Ridge reg", "Poly reg", "GBM")
        )

## Eventually a Shiny App was created to allow to intuitively leverage the predictive algorithm 



















