## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------

# USEFUL LIBRAIRES (INSTALLING AND) LOADING
#install.packages("ggcorrplot")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("reshape2")
#install.packages("stats")
#install.packages("pROC")
#install.packages("class")
#install.packages("caret")
#install.packages("randomForest")
#install.packages("factoextra")
#install.packages("cluster")
#install.packages("MASS")
#install.packages("glmnet")
#install.packages("gam")
#install.packages("splines")
#install.packages("e1071")
#install.packages("tree")
#install.packages("xgboost")
#install.packages("ggpubr")
#install.packages("gridExtra")

library(gridExtra)

library(cluster)

library(factoextra)

library(randomForest)

library(dplyr) # imports library to clean "Unknown" replacing with NA

library(ggplot2) # imports library to visualize graphs

library(reshape2) #used for plot of numerical_vars

library(stats) #necessary package for logistic regression

library(pROC)  # For ROC-AUC score

library(class) # For KNN

library(caret)  # For accuracy, sensitivity, specificity

library(ggcorrplot) # For correlation matrix

library(MASS)

library(glmnet)

library(gam)

library(splines)

library(tree)

library(e1071)

library(xgboost)

library(ggpubr)




## -------------------------------------------------------------------------------------------------------------------------------
# IMPORTING DATASET
df = read.csv("/Users/kevinwang/Documents/LUISS/data analysis and business/Final project/BrazHousesRent.csv", sep = ",", dec = ".", header = T, colClasses = "character")
head(df)
## -------------------------------------------------------------------------------------------------------------------------------

# CLEANING DATASET
cleaned_df <- df
# Identify and handle duplicates

anyDuplicated(cleaned_df)

# Count the number of duplicates
num_duplicates <- sum(duplicated(cleaned_df))
num_duplicates

cleaned_df <- distinct(cleaned_df)
num_rows <- nrow(df)
num_rows_new <- nrow(cleaned_df)
print(paste("The number of duplicate rows deleted are", num_rows - num_rows_new))

## Handling missing values
anyNA(df)
## We noticed that there's no NA values in the data but there is a "-" value in 'floor' meaning number of floors, which could either represent a missing value or it could suggest that '-' symbol represents properties with no floors such as a house not a part of a building or apartment. This could also indicate the ground floor as in brazil they do not follow the US naming conventions for floors where ground floor is labeled as level 1 but in Brazil ground floor is labeled like in europe simply by describing it as the ground floor or "T" for "Térreo". 
count<- sum(df$floor == "-")
print(paste("Count for floor = '-':", count))
# From the count we understand there are 2369 "-" values. We proceed by handling these '-' values first substituting them with 'NA' and then analyzing them to understand whether we can distinguish what they actually represent.
### Replace "-" with NA in all columns of cleaned_df
for (col_name in colnames(cleaned_df)) {
  cleaned_df[[col_name]][cleaned_df[[col_name]] == "-"] <- NA
}
### visualizing number of NA values count for each variable
colSums(is.na(cleaned_df)) # outputs a visualization of the NA values count for each variable
# From the results given we can clearly observe that floor has 2369 NA values which is the same number of '-' values and we can deduce that the '-' values were subsituted correctly.
# To understand if the "-" represents a house that is not a part of the condominium, one clear feature stands out which could help deduce this which is the 'hoa'. 'hoa' represents the Monthly Homeowners Association Tax which means if a house is no a part of a condominium the value 'hoa' will be 0.
# To identify this relationship we firstly identify rows with NA in 'floor' feature and then count zeroes in 'hoa' for the NA rows.
na_floor_rows <- cleaned_df[is.na(cleaned_df$floor), ]
num_zeroes_in_hoa <- sum(na_floor_rows$hoa == 0)
print(paste("Number of 0s in hoa for rows with NA in floor:", num_zeroes_in_hoa))
# The results were indeed significant as 2015 values out of 2369 which had '-' as their 'floor' also had 0 as thier 'hoa' value. This is a significant amount, around 85%. From this we can assume that indeed the '-' represents house without a condomiunium or 0 floors as there are still a percentage of houses who could possibly be a part of a condominium but just at floor 0.
# Changing NA values to 0
cleaned_df$floor[is.na(cleaned_df$floor)] <- 0
cleaned_df

# Iterating through the list of numerical variables saved as characters strings to transform them in numerical values

numerical_vars <- c('area', 'rooms', 'bathroom', 'parking.spaces', 'floor', 'hoa..R..', 'rent.amount..R..','property.tax..R..', 'fire.insurance..R..')
for (var in numerical_vars) {
  cleaned_df[[var]] <- as.numeric(cleaned_df[[var]])
}

## Encoding Categorical Variables by converting all cat_vars into factors

categorical_vars <- c("city", "animal", "furniture")

cleaned_df[categorical_vars] <- lapply(cleaned_df[categorical_vars], factor)

## Data Pre-processing

### Summary statistics for numerical variables
numeric_columns <- sapply(cleaned_df, is.numeric)
numeric_df <- cleaned_df[, numeric_columns]
summary(numeric_df)

# The summary statistics reveal several points in the data set. The ones that stand out the most are the outliers in features such as hoa..R.. where the 3rd quantile is 1289 while the max is 1117000 or in the property.tax..R.. feature where the 3rd quantile is 390 but the max value is 313700. It clear that these maximum values can be seen as outliers in our dataset or interpreted as such. Another interesting point that canbe brought up later is the fact that the bathroom feature is mostly composed of houses with 1 bathroom as shown from median, min, 1st quantile, and 3rd quantile, but it seems like there are houses which reach maximum of 9 bathrooms, and so it seems like those could be outliers too and if thats the case bathroom could be considered a feature to drop in our analysis as it is mostly composed of 1. To visualize better the distribution of features and the outliers we create histograms and boxplots.
# Histogram of numerical variables 
ggplot(reshape2::melt(cleaned_df[, numerical_vars]), aes(x = value)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  facet_wrap(~variable, scales = "free_x") +
  theme_minimal() +
  labs(x = "Value", y = "Frequency", title = "Histograms of Numeric Variables")
# Boxplot of numerical variables
ggplot(reshape2::melt(cleaned_df[, numerical_vars]), aes(x = variable, y = value)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  labs(x = NULL, y = "Value", title = "Boxplots of Numerical Variables")

# Histogram for rent.amount..R..
histogram_plot <- ggplot(cleaned_df, aes(x = rent.amount..R..)) +
  geom_histogram(binwidth = 100, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(x = "Rent Amount (R$)", y = "Frequency", title = "Histogram of Rent Amount")

# Boxplot for rent.amount..R..
boxplot_plot <- ggplot(cleaned_df, aes(y = rent.amount..R..)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(x = NULL, y = "Rent Amount (R$)", title = "Boxplot of Rent Amount")

# From both the histogram and the boxplot many outliers can be visualized suggested by the distribution of points how most graphs are extremely skewed to the left. These can be seen as very expensive houses or a possible error or inconsistency in the data as the data was gathered through a web-crawler. In order to obtain more accurate data we choose to remove some of these outliers. 
# Removing values foud standard deviation away from their mean
for (col in names(cleaned_df)[numeric_columns]) {
  mean_val <- mean(cleaned_df[[col]], na.rm = TRUE)
  sd_val <- sd(cleaned_df[[col]], na.rm = TRUE)
  lower_bound <- mean_val - 4 * sd_val
  upper_bound <- mean_val + 4 * sd_val
  
  cleaned_df <- cleaned_df %>%
    filter(cleaned_df[[col]] >= lower_bound & cleaned_df[[col]] <= upper_bound)
}
# We chose to remove the outliers with value that were four standard deviation away from their mean as we did not want to remove too many values and drastically affect the data, and we opted for four st deviations away from the mean which only removed 187 values out of 10329 which is 1.81% of the data.


# Distribution of variable used as response variable

# Plot the correlation matrix for numeric values to gain a better more general understanding
ggcorrplot(cor(cleaned_df[, numeric_columns]), method = "square", type = "lower", 
           lab = TRUE, lab_size = 3, colors = c("blue", "white", "red"))

# In the correlation matrix we can view the correlations of each feature with eachother, and understand key relationships. Seeing that the objective of our first task is to build a predictive model and find out the rent amount according to the house specifics, we mainly focus on the rent amount feature as our response variable and how it interacts with the other features.
# Plotting only the correlation of rent amount with the rest of the features
cor_matrix <- cor(cleaned_df[, numeric_columns])
cor_matrix['rent.amount..R..', ]

# From the correlation matrix we can see clear relationships between rent amount and area of the house as well rent amount with rooms and with property tax. The most distinct correlation we see is with rent amount and fire insurance with around a 0.988 correlation depicting a very strong linear relationship. We can also note a few features which have no correlation with any other feature or very poor correlation such as floor and bathroom which we could look to drop as features. Parking spaces and hoa, also do not reach a correlation of above 0.5 with rent amount and could be dropped during feature selection. 

# Categorical feature analysis 

par(mfrow=c(1,3)) # Adjust depending on the number of categorical variables
# Iterate over each categorical variable
for (var in categorical_vars) {
  barplot(table(cleaned_df[[var]]),
          main=paste("Distribution of", var),
          xlab=var,
          ylab="Frequency",
          col="skyblue") # You can adjust the color as desired
}

# From the distribution of each categorical variable we are able to deduce various points. Firstly the city with the most houses registered in the data frame is São Paulo by a large margin which will be taken into account in future analysis as data is bias towards houses located in Sao Paulo. Furthermore, we are able to note that most houses do accept animals, around 8000, with around 2000 who don't. The same goes for the furniture distribution where their is clear bias for non furnished houses in the data frame over furnished ones. 

## Since rent amount is our response variable, we will visualize the relationship of rent amount with other categorical features to understand whether rent amount is affected by any of them. 

## Rent amount distribution by city

mean_data <- aggregate(rent.amount..R.. ~ city, data = cleaned_df, mean)

mean_data

ggplot(cleaned_df, aes(x = rent.amount..R..)) +
  geom_histogram(binwidth = 100, fill = "blue", color = "black", alpha = 0.7) +
  facet_wrap(~ city, scales = "free_y") +
  theme_minimal() +
  labs(title = "Distribution of Rent Amount by City with Mean",
       x = "Rent Amount",
       y = "Frequency") +
  geom_vline(data = mean_data, aes(xintercept = rent.amount..R..), color = "red", linetype = "dashed", size = 1) +
  geom_text(data = mean_data, aes(x = rent.amount..R.., label = round(rent.amount..R.., 2), y = 20), color = "red", angle = 90, vjust = -0.5)

# From the data shown above, we are able to deduce multiple points. São Paulo has the highest mean rent, possibly due to its status as Brazil's largest city and financial hub, attracting significant business and professional demand. Other cities like Rio de Janeiro and Belo Horizonte also show high rents possibly due to economic activity, population density, and quality of life. Campinas and Porto Alegre have lower mean rents possibly due to their smaller economic scale, lower cost of living, and local economic conditions compared to larger cities like São Paulo and Rio de Janeiro.
# We can verify whether the difference in mean is significant or not through an one-way anova test:

# Perform one-way ANOVA

welch_anova_result <- oneway.test(rent.amount..R.. ~ city, data = cleaned_df, var.equal = FALSE)
welch_anova_result

# Based on the ANOVA results, there are significant differences in mean rent amounts between the cities as p-value is lower than 0.05.
cities <- unique(cleaned_df$city)

# Create a data frame to store t-test results
t_test_results <- data.frame(city1 = character(), city2 = character(), p_value = numeric(), stringsAsFactors = FALSE)

# Loop through each pair of cities
for (i in 1:(length(cities)-1)) {
  for (j in (i+1):length(cities)) {
    city1 <- cities[i]
    city2 <- cities[j]
    
    # Subset data for the two cities
    data_city1 <- cleaned_df %>% filter(city == city1)
    data_city2 <- cleaned_df %>% filter(city == city2)
    
    # Perform t-test
    t_test_result <- t.test(data_city1$rent.amount..R.., data_city2$rent.amount..R..)
    
    # Store results
    t_test_results <- rbind(t_test_results, data.frame(city1 = city1, city2 = city2, p_value = t_test_result$p.value))
  }
}

# Print t-test results
print(t_test_results)

# Optionally adjust for multiple comparisons (Bonferroni correction)
t_test_results$p_adjusted <- p.adjust(t_test_results$p_value, method = "bonferroni")

# Print adjusted results
print(t_test_results)
## Rent amount distribution by animal
mean_data1 <- aggregate(rent.amount..R.. ~ animal, data = cleaned_df, mean)

mean_data1

ggplot(cleaned_df, aes(x = rent.amount..R..)) +
  geom_histogram(binwidth = 100, fill = "blue", color = "black", alpha = 0.7) +
  facet_wrap(~ animal, scales = "free_y") +
  theme_minimal() +
  labs(title = "Distribution of Rent Amount by animal acceptance with Mean",
       x = "Rent Amount",
       y = "Frequency") +
  geom_vline(data = mean_data1, aes(xintercept = rent.amount..R..), color = "red", linetype = "dashed", size = 1) +
  geom_text(data = mean_data1, aes(x = rent.amount..R.., label = round(rent.amount..R.., 2), y = 20), color = "red", angle = 90, vjust = -0.5)

# Seems like houses that do accept animals have a higher mean than houses that do not which could make sense as higher flexibility is awarded with higher mean rent. We can also observe if this difference in mean is significant through a t-test. 
# T-test
t_test_result <- t.test(rent.amount..R.. ~ animal, data = cleaned_df, var.equal = FALSE)
t_test_result
# Based on the T-test results, there is a significant difference in mean rent amounts between whether a house accepts or does not accept animals as seen from p-value lower than 0.05.
## Rent amount distribution by furniture 
mean_data2 <- aggregate(rent.amount..R.. ~ furniture, data = cleaned_df, mean)

mean_data2

ggplot(cleaned_df, aes(x = rent.amount..R..)) +
  geom_histogram(binwidth = 100, fill = "blue", color = "black", alpha = 0.7) +
  facet_wrap(~ furniture, scales = "free_y") +
  theme_minimal() +
  labs(title = "Distribution of Rent Amount by furniture acceptance with Mean",
       x = "Rent Amount",
       y = "Frequency") +
  geom_vline(data = mean_data2, aes(xintercept = rent.amount..R..), color = "red", linetype = "dashed", size = 1) +
  geom_text(data = mean_data2, aes(x = rent.amount..R.., label = round(rent.amount..R.., 2), y = 20), color = "red", angle = 90, vjust = -0.5)

# Seems like houses that are furnished have a higher mean than houses that are not which could make sense as furniture can play a big part in a house's demand as well as appealability. We can also observe if this difference in mean is significant through a t-test. 
# T-test
t_test_result <- t.test(rent.amount..R.. ~ furniture, data = cleaned_df, var.equal = FALSE)
t_test_result
# Based on the T-test results, there is a significant difference in mean rent amounts between whether a house is furnished or not as seen from p-value lower than 0.05. 

## Point 3: Task 1

# The objective of our first task is to build a predictive model to estimate the rent amount based on house specifics, with rent.amount..R.. as the response variable applied to regression models.
# Practical Relevance
# In practical terms, predicting the rent amount accurately is valuable for both renters and landlords. Renters can make informed decisions based on their budget and preferences, while landlords can set competitive prices to maximize occupancy and revenue. This model can also assist real estate agencies in providing data-driven advice to their clients.

# POINT 4: Lower Dimensional Models

## Let's investigate the relationship between the response variable (rent.amount..R..) and a few selected variables (area, rooms, bathroom, furniture) to understand their importance in predicting rent. we will use a Multiple Linear Regression model and a Random Forest model to explore these relationships.


## Multiple Linear Regression

fit2 <- lm(rent.amount..R.. ~ area + rooms + bathroom + furniture, data = cleaned_df)
summary(fit2)$coefficients

## Results: The intercept represents the expected rent amount when all predictors (area, rooms, bathroom, and furniture) are zero. In this context, an intercept of 2030.0066 suggests that even with no area, rooms, bathrooms, or furnishings, the base rent amount starts at approximately 2030.01 (currency units, likely Brazilian Reais). The extremely low p-value indicates this estimate is highly significant.


##Random Forest

rf_model <- randomForest(rent.amount..R.. ~ area + rooms + bathroom + furniture, data = cleaned_df, ntree = 100)
importance(rf_model)

## Results: These numbers represent the total increase in node purity contributed by each variable, averaged across all trees in the forest. A higher IncNodePurity score indicates a greater contribution to reducing impurity, making the variable more important for predicting the target variable, which in our case is the rent amount.

# ⁠area has the highest score, indicating it's the most important predictor.
#⁠  ⁠rooms is also highly important but less so than area.
#⁠  ⁠bathroom and furniture contribute less to the model's predictions compared to area and rooms.

## Plotting Variable Importance

varImpPlot(rf_model)

# Split the data into training, validation, and test sets
set.seed(444) # for reproducibility
trainIndex <- createDataPartition(cleaned_df$rent.amount..R.., p = 0.6, list = FALSE)
train_df <- cleaned_df[trainIndex,]
temp_df <- cleaned_df[-trainIndex,]
valIndex <- createDataPartition(temp_df$rent.amount..R.., p = 0.5, list = FALSE)
val_df <- temp_df[valIndex,]
test_df <- temp_df[-valIndex,]


### Linear Model with Stepwise Selection (AIC and BIC)
full.model <- lm(rent.amount..R.. ~ ., data = train_df)
step.model.aic <- stepAIC(full.model, direction = "both", trace = 0)
step.model.bic <- stepAIC(full.model, direction = "both", trace = 0, k = log1p(nrow(train_df)))

predictions.aic <- predict(step.model.aic, newdata = val_df)
predictions.bic <- predict(step.model.bic, newdata = val_df)

aic_mse <- mean((predictions.aic - val_df$rent.amount..R..)^2)
bic_mse <- mean((predictions.bic - val_df$rent.amount..R..)^2)
aic_rmse <- sqrt(aic_mse)
bic_rmse <- sqrt(bic_mse)
aic_rsquared <- summary(step.model.aic)$r.squared
bic_rsquared <- summary(step.model.bic)$r.squared

# Print the results
cat("AIC Stepwise Selection:\n")
cat("MSE:", round(aic_mse, 5), "\n")
cat("RMSE:", round(aic_rmse, 5), "\n")
cat("R-squared:", round(aic_rsquared, 5), "\n\n")

cat("BIC Stepwise Selection:\n")
cat("MSE:", round(bic_mse, 5), "\n")
cat("RMSE:", round(bic_rmse, 5), "\n")
cat("R-squared:", round(bic_rsquared, 5), "\n")

##The comparison between AIC and BIC results shows minimal differences, with BIC yielding a marginally higher MSE and RMSE, and a very slightly lower R-squared value. 
##This slight difference suggests that both criteria lead to similar models in terms of performance, but BIC, which penalizes model complexity more strongly, might result in a more parsimonious model. 
##Overall, both stepwise selection methods provide robust models with high explanatory power and reasonable predictive accuracy for the rent amount.


### Penalized Approache (Lasso)
X = model.matrix(rent.amount..R.. ~ ., data=train_df)[,-1]
y = train_df$rent.amount..R..
cvlasso = cv.glmnet(x = X, y = y, nfolds = 10)


pen_val <- model.matrix(rent.amount..R.. ~ ., data=val_df)[,-1]
lassopredmin <- predict(cvlasso, pen_val, s = "lambda.min")
lmin_mse <- mean((lassopredmin - val_df$rent.amount..R..)^2)
lmin_rmse <- sqrt(lmin_mse)
lmin_Rsq <- cor(lassopredmin, val_df$rent.amount..R..)^2
lassopred1se <- predict(cvlasso, pen_val, s = "lambda.1se")
lse_mse <- mean((lassopred1se - val_df$rent.amount..R..)^2)


# Print results
cat("Lasso (lambda.min) MSE:", lmin_mse, "\n")
cat("Lasso (lambda.min) RMSE:", lmin_rmse, "\n")
cat("Lasso (lambda.min) R-squared:", lmin_Rsq, "\n")
cat("Lasso (lambda.1se) MSE:", lse_mse, "\n")


##For the Lasso regression using the lambda.min value, the Mean Squared Error (MSE) is 119376.7, the Root Mean Squared Error (RMSE) is 345.5094, and the R-squared value is 0.9885972.
##These results indicate that the Lasso model explains approximately 98.86% of the variance in the rent amount, showcasing a high level of accuracy and a good fit.
##When using the lambda.1se value for the Lasso regression, the MSE increases to 135379.4, suggesting that this model, which is simpler due to stronger regularization, does not perform as well as the model using lambda.min.



### Non-linear Models (KNN, Splines, XGBoost)

# k-Nearest Neighbors
knn_model <- train(rent.amount..R.. ~ ., data = train_df, method = "knn", tuneLength = 10)
pred_knn <- predict(knn_model, newdata = val_df)
knn_mse <- mean((pred_knn - val_df$rent.amount..R..)^2)
knn_rmse <- sqrt(knn_mse)
knn_rsquared <- cor(pred_knn, val_df$rent.amount..R..)^2

# Print KNN results
cat("k-Nearest Neighbors:\n")
cat("MSE:", round(knn_mse, 5), "\n")
cat("RMSE:", round(knn_rmse, 5), "\n")
cat("R-squared:", round(knn_rsquared, 5), "\n\n")

##These metrics suggest that the KNN model explains approximately 76.75% of the variance in the rent amount, which is relatively moderate. 
##However, the high MSE and RMSE values indicate that the model's predictions have a substantial error margin, implying that KNN may not be the best model for this dataset.

# Splines
spline_model <- lm(rent.amount..R.. ~ bs(area, degree = 3) + bs(rooms, degree = 3) + bs(bathroom, degree = 3) + bs(fire.insurance..R.., degree = 3) + bs(hoa..R.., degree = 3) + furniture + city, data = train_df)
pred_spline <- predict(spline_model, newdata = val_df)
spline_mse <- mean((pred_spline - val_df$rent.amount..R..)^2)
spline_rmse <- sqrt(spline_mse)
spline_rsquared <- cor(pred_spline, val_df$rent.amount..R..)^2

# Print Splines results
cat("Splines:\n")
cat("MSE:", round(spline_mse, 5), "\n")
cat("RMSE:", round(spline_rmse, 5), "\n")
cat("R-squared:", round(spline_rsquared, 5), "\n\n")

##the Splines model explains approximately 98.92% of the variance in the rent amount, demonstrating a high level of accuracy. 
##The lower MSE and RMSE compared to the KNN model suggest that the Splines model has a much smaller error margin, making it a more suitable choice for predicting rent amounts in this dataset.

# XGBoost
X_train <- train_df[, !(colnames(train_df) %in% c("rent.amount..R.."))]
y_train <- as.numeric(train_df$rent.amount..R..)
X_val <- val_df[, !(colnames(val_df) %in% c("rent.amount..R.."))]
y_val <- as.numeric(val_df$rent.amount..R..)
for(i in 1:ncol(X_train)) {
  X_train[,i] = as.numeric(X_train[,i])
}
for(i in 1:ncol(X_val)) {
  X_val[,i] = as.numeric(X_val[,i])
}
xgb_train <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
xgb_val <- xgb.DMatrix(data = as.matrix(X_val))

param_grid <- expand.grid(
  nrounds = c(100, 200, 300),
  max_depth = c(3, 4, 5),             
  eta = c(0.01, 0.05, 0.1),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,  
  subsample = 1   
)

xgb_model <- train(
  X_train, y_train, 
  method = "xgbTree",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = param_grid,
  metric = 'RMSE',
  verbosity = 0
)

final_xgb_model <- xgboost(
  data = xgb_train, 
  nrounds = xgb_model$bestTune$nrounds,
  max_depth = xgb_model$bestTune$max_depth,
  eta = xgb_model$bestTune$eta,
  verbose = 0
)

pred_xgb <- predict(final_xgb_model, newdata = xgb_val)
xgb_mse <- mean((pred_xgb - y_val)^2)
xgb_rmse <- sqrt(xgb_mse)
xgb_rsquared <- cor(pred_xgb, y_val)^2

# Print XGBoost results
cat("XGBoost:\n")
cat("MSE:", round(xgb_mse, 5), "\n")
cat("RMSE:", round(xgb_rmse, 5), "\n")
cat("R-squared:", round(xgb_rsquared, 5), "\n")

## the XGBoost model explains approximately 99.56% of the variance in the rent amount, signifying a very strong fit. 
## The relatively low MSE and RMSE values suggest that the model's predictions are highly accurate


### Overall Models Comparison
table <- data.frame(
  Model = c("AIC", "BIC", "Lasso", "KNN", "Splines", "XGBoost"),
  MSE = c(aic_mse, bic_mse, lmin_mse, knn_mse, spline_mse, xgb_mse),
  RMSE = c(aic_rmse, bic_rmse, lmin_rmse, knn_rmse, spline_rmse, xgb_rmse),
  R_squared = c(aic_rsquared, bic_rsquared, lmin_Rsq, knn_rsquared, spline_rsquared, xgb_rsquared)
)
print(table)


### Final Model Implementation on Test Set
final_train_df <- rbind(train_df, val_df)
X_final_train <- final_train_df[, !(colnames(final_train_df) %in% c("rent.amount..R.."))]
y_final_train <- as.numeric(final_train_df$rent.amount..R..)
X_test <- test_df[, !(colnames(test_df) %in% c("rent.amount..R.."))]
y_test <- as.numeric(test_df$rent.amount..R..)
for(i in 1:ncol(X_final_train)) {
  X_final_train[,i] = as.numeric(X_final_train[,i])
}
for(i in 1:ncol(X_test)) {
  X_test[,i] = as.numeric(X_test[,i])
}
xgb_final_train <- xgb.DMatrix(data = as.matrix(X_final_train), label = y_final_train)
xgb_test <- xgb.DMatrix(data = as.matrix(X_test))

final_xgb_model <- xgboost(
  data = xgb_final_train, 
  nrounds = xgb_model$bestTune$nrounds,
  max_depth = xgb_model$bestTune$max_depth,
  eta = xgb_model$bestTune$eta,
  verbose = 0
)

final_predictions <- predict(final_xgb_model, newdata = xgb_test)
test_mse <- mean((final_predictions - y_test)^2)
test_rmse <- sqrt(test_mse)
test_rsquared <- cor(final_predictions, y_test)^2
cat("Test Set Performance:\n")
cat("MSE:", test_mse, "\n")
cat("RMSE:", test_rmse, "\n")
cat("R-squared:", test_rsquared, "\n")

##These metrics indicate that the model maintains a high level of accuracy on the test set, explaining approximately 99.55% of the variance in the rent amount. 
##The relatively low MSE and RMSE values suggest that the model's predictions are close to the actual values, confirming its robustness and generalizability to unseen data.


# Calculate prediction errors and confidence intervals
prediction_errors <- final_predictions - y_test
mean_error <- mean(prediction_errors)
std_error <- sd(prediction_errors)
confidence <- 0.95
z_value <- qnorm(1 - (1 - confidence) / 2)
lower_bound <- mean_error - z_value * std_error 
upper_bound <- mean_error + z_value * std_error 

cat("Confidence Interval (", confidence * 100, "%): [", round(lower_bound, 2), ", ", round(upper_bound, 2), "]\n")


## By subtracting the actual rent amounts in the test set from the predicted values, we obtain the prediction errors. 
## The mean and standard deviation of these errors are then used to construct a 95% confidence interval for the error. 
## This interval, which ranges from -412.57 to 442.02, indicates that the predicted rent prices are expected to deviate from the actual rent prices by this range on average.


# Feature Importance Analysis
importance_scores <- xgb.importance(
  feature_names = colnames(X_final_train),
  model = final_xgb_model
)
importance_data <- as.data.frame(importance_scores)
importance_data$Feature <- factor(importance_data$Feature, levels = importance_data$Feature[order(-importance_data$Gain)])

# Plotting Feature Importance

ggplot(importance_data, aes(x = Feature, y = Gain, fill = Gain)) +
  geom_col(show.legend = FALSE) +  
  coord_flip() +  
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  
  labs(
    title = "Feature Importance for Rental Price Prediction",
    x = "Importance",
    y = "Features"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  
    axis.title = element_text(size = 12),  
    axis.text = element_text(size = 10)    
  )

## The most striking observation from the plot is that the feature fire.insurance..R.. is overwhelmingly the most influential factor
## Other features such as floor, city, hoa..R.. have negligible importance in comparison, with their importance scores being close to zero. 
## This suggests that while these features contribute to the model, their impact is minimal relative to fire.insurance..R... 
## The dominance of fire.insurance..R.. underscores its critical role in determining rental prices, possibly reflecting the higher replacement costs associated with more valuable properties, which in turn command higher rents.


# Further Analysis on Fire Insurance
ggplot(final_train_df, aes(x = rent.amount..R.., y = fire.insurance..R..)) +
  geom_point(alpha = 0.6, color = "blue") + 
  labs(
    title = "Rent Amount vs Fire Insurance",
    x = "Rent Amount (R$)",
    y = "Fire Insurance (R$)"
  ) +
  theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5), 
    axis.title.x = element_text(face = "bold", color = "darkblue"),  
    axis.title.y = element_text(face = "bold", color = "darkblue")   
  ) +
  geom_smooth(method = "lm", color = "red", se = FALSE) 

## The scatter plot depicts the relationship between rent amount and fire insurance costs, showing a clear linear trend.
## The plot visually supports the conclusion that properties commanding higher rents are associated with higher fire insurance costs, likely due to their greater value and replacement costs.


# Scatter plot with cities
ggplot(final_train_df, aes(x = rent.amount..R.., y = fire.insurance..R.., color = city)) +
  geom_point(alpha = 0.6) +  
  labs(
    title = "Rent Amount vs Fire Insurance by City",
    x = "Rent Amount (R$)",
    y = "Fire Insurance (R$)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(face = "bold", color = "darkblue"),
    axis.title.y = element_text(face = "bold", color = "darkblue")
  ) +
  geom_smooth(method = "lm", se = FALSE) 

## The plot also highlights that, while the overall trend is similar, some cities have a wider spread of data points, suggesting greater variability in fire insurance costs for similar rent amounts. 
## This analysis reinforces the critical role of location in determining both rent and associated insurance costs, and it underscores the importance of considering city-specific factors when predicting rental prices.



# POINT 6 - DRAWING CONCLUSIONS

## In conclusion, our comprehensive analysis and modeling efforts have provided valuable insights into the rental market dynamics, highlighting the critical factors influencing rent prices. 
## The XGBoost model, with its high R-squared value and low prediction errors, has proven to be an exceptionally accurate tool for predicting rental prices, effectively capturing the nuances of the data.

## Our findings underscore the significant impact of fire insurance costs on rental prices, as evidenced by the feature importance analysis. This strong correlation suggests that properties with higher fire insurance premiums generally command higher rents, reflecting their increased value and associated risks. 
## Additionally, the variation in the relationship between rent and fire insurance costs across different cities indicates the importance of location-specific factors in rental pricing.

## The detailed analysis of rental prices in relation to various features, including the number of rooms, area, and presence of amenities such as parking spaces and furniture, provides a nuanced understanding of the market. 
## For instance, while traditional factors like the number of rooms and area are important, our model suggests that fire insurance costs and city-specific characteristics play a more crucial role in determining rental prices.

### CLUSTERING TASK 2

# One-hot encoding categorical variables using model.matrix
one_hot_encoded_df <- model.matrix(~ city + animal + furniture - 1, data = cleaned_df)
one_hot_encoded_df <- as.data.frame(one_hot_encoded_df)

# Combine one-hot encoded variables with the cleaned_df, excluding original categorical columns
encoded_df <- cbind(cleaned_df[, !names(cleaned_df) %in% categorical_vars], one_hot_encoded_df)


Data_sc <- encoded_df %>% select_if(is.numeric)

# Removing features we deem have no effect on data and are worth dropping
Data_sc <- dplyr::select(Data_sc, -bathroom, -floor, -parking.spaces, -'cityBelo Horizonte', -cityCampinas, -'cityPorto Alegre', -'cityRio de Janeiro', -'citySão Paulo', -'animalnot acept')

# Scaling numerical variables
Data_sc <- as.data.frame(scale(Data_sc))

# Dimensionality Reduction using PCA
pca_result <- prcomp(Data_sc, center = TRUE, scale. = TRUE)
pca_data <- as.data.frame(pca_result$x)

summary(pca_result)

# Select the first 3 principal components
pca_data <- as.data.frame(pca_result$x[, 1:3])

loadings <- pca_result$rotation[, 1:3]

# Function to get top contributing features for each PC
top_contributors <- function(loadings, n = 5) {
  apply(loadings, 2, function(x) {
    sorted <- sort(abs(x), decreasing = TRUE)
    names(sorted)[1:n]
  })
}

# Get top 5 contributors for all PCs
top_features <- top_contributors(loadings, n = 5)
print(top_features)

# Calculate distance matrices using Pearson and Euclidean methods
dist_p <- factoextra::get_dist(pca_data, method = "pearson")
dist_e <- factoextra::get_dist(pca_data, method = "euclidean")

# Set seed for reproducibility
set.seed(123)

# Determine the optimal number of clusters using silhouette method
sil_k <- fviz_nbclust(
  pca_data,
  FUNcluster = kmeans,
  diss = dist_e,
  method = "silhouette",
  print.summary = TRUE,
  k.max = 10,
)

sil_h <- fviz_nbclust(
  pca_data,
  FUNcluster = factoextra::hcut,
  diss = dist_e,
  method = "silhouette",
  print.summary = TRUE,
  k.max = 10,
)



# Print the best number of clusters for K-Means and Hierarchical Clustering
best_k_kmeans <- which.max(sil_k$data$y)
best_k_hierarchical <- which.max(sil_h$data$y)
print(paste("Best k for K-Means:", best_k_kmeans,
            ", Best k for Hierarchical Clustering:", best_k_hierarchical))

# Visualize the partitions for K-Means
km <- kmeans(pca_data, best_k_kmeans, nstart =100, iter.max = 2000)
kmv <- fviz_cluster(km, data = pca_data, geom = "point", 
                    ggtheme = theme_minimal(), main = "K-Means")
print(kmv)

# Visualize the partitions for Hierarchical Clustering
hcc <- factoextra::hcut(x = dist_e, k = best_k_hierarchical, hc_method = "ward.D2")
ec <- factoextra::fviz_cluster(list(data = pca_data, cluster = hcc$cluster), main = "Hierarchical", labelsize = 0)
print(ec)

# Calculate and print the average silhouette width for K-Means
sk <- silhouette(km$cluster, dist = dist_e)
skv <- fviz_silhouette(sk, print.summary = FALSE)
print(paste("K-Means Average Silhouette width:", mean(skv$data$sil_width)))

# Calculate and print the average silhouette width for Hierarchical Clustering
hcc <- factoextra::hcut(x = dist_e, k = best_k_hierarchical, hc_method = "ward.D2")
print(paste("Hierarchical Clustering Average Silhouette width:", hcc$silinfo$avg.width))



# Function to calculate statistics for each cluster, excluding 'bathrooms', 'rooms', and 'fire.insurance'
cluster_stats <- function(cluster_data, cluster_label) {
  stats <- sapply(cluster_data, function(x) round(mean(x, na.rm = TRUE), 3))
  data.frame(
    Cluster = cluster_label,
    t(stats)
  )
}

# Extract clusters and calculate statistics for K-Means
km_clust_stats <- lapply(1:best_k_kmeans, function(i) {
  cluster_data <- pca_data[which(km$cluster == i), ]
  cluster_stats(cluster_data, paste("K-Means Cluster", i))
})

km_clust_stats_df <- do.call(rbind, km_clust_stats)
print(km_clust_stats_df)

# Extract clusters and calculate statistics for Hierarchical Clustering
hc_clust_stats <- lapply(1:best_k_hierarchical, function(i) {
  cluster_data <- pca_data[which(hcc$cluster == i), ]
  cluster_stats(cluster_data, paste("Hierarchical Cluster", i))
})

hc_clust_stats_df <- do.call(rbind, hc_clust_stats)
print(hc_clust_stats_df)

# Due to higher silhuoette score, K-means is chosen to provide further analysis

#Looking at properties of each cluster
cluster_1 <- encoded_df[which(km$cluster == 1),]
cluster_2 <- encoded_df[which(km$cluster == 2),]

avreage_cluster <- data.frame(
  Cluster = c("Low Value houses", "High Value Houses"),
  Average_Rent_Amount = c(round(mean(cluster_1$rent.amount..R..), 3), round(mean(cluster_2$rent.amount..R..), 3)),
  Average_Area = c(round(mean(cluster_1$area), 3), round(mean(cluster_2$area), 3)),
  Average_Furniture_not_Furnished = c(round(mean(cluster_1$'furniturenot furnished'), 3), round(mean(cluster_2$'furniturenot furnished'), 3))
)

avreage_cluster

#Pie-Chart for each cluser for City
display_city_pie_chart <- function(Data, data_name){  
  
  city_df <- as.data.frame(table(Data$city))
  colnames(city_df) <- c("city", "count")
  
  city_df$percentage <- city_df$count / sum(city_df$count)
  
  ggplot(city_df, aes(x = "", y = percentage, fill = city)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) + 
    scale_fill_brewer(palette = "Set1") +
    theme_void() +
    labs(title = paste("Percentage of houses in each city in", data_name), fill = "City")
}


city_pie1 <- display_city_pie_chart(cluster_1,"Cluster1")
city_pie2 <- display_city_pie_chart(cluster_2,"Cluster2")
ggarrange(pie1,pie2,nrow = 1,ncol = 2)

low_value <- cleaned_df[which(km$cluster == 1),]
high_value <- cleaned_df[which(km$cluster == 2),]


display_city_summary <- function(Data, data_name){
  city_df <- as.data.frame(table(Data$city))
  colnames(city_df) <- c("City", "Count")
  
  city_df$Percentage <- round((city_df$Count / sum(city_df$Count)) * 100, 2)
  
  print(paste("Summary of houses in each city for", data_name))
  print(city_df)
}

summary_cluster_1 <- display_city_summary(low_value, "Cluster 1")
summary_cluster_2 <- display_city_summary(high_value, "Cluster 2")

animal_summary <- function(Data, data_name){
  animal_df <- as.data.frame(table(Data$animal))
  colnames(animal_df) <- c("Animal", "Count")
  
  animal_df$Percentage <- round((animal_df$Count / sum(animal_df$Count)) * 100, 2)
  
  print(paste("Summary of houses in each city for", data_name))
  print(animal_df)
}

animal_summary_cluster_1 <- animal_summary(low_value, "Cluster 1")
animal_summary_cluster_2 <- animal_summary(high_value, "Cluster 2")

