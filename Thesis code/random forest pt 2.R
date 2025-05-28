# Load necessary libraries
library(tidyverse)
library(dplyr)
library(caret)
library(randomForest)
library(pROC)
library(ROSE)
library(themis)
library(recipes)
library(doParallel)
library(e1071)  # For skewness calculation
library(rio)
library(lubridate)
library(readr)
library(ggplot2)

# Import data
dta <- read_csv("~/usagov_combined.csv")
dta$is_excluded_vendor <- factor(dta$is_excluded_vendor, levels = c(FALSE, TRUE), labels = c("0", "1"))

# Define BPTW threshold
bptw_threshold <- quantile(dta$"X2020_score_normalized", 0.25, na.rm = TRUE)

# Function to create model indicators
model_indicators <- function(dta) {
  dta %>%
    mutate(
      single_bidder = ifelse(number_of_offers_received == 1, 1, 0),
      num_offers = number_of_offers_received,
      unpublished_call = ifelse(is.na(fed_biz_opps_code) | is.na(solicitation_identifier), 1, 0),
      extent_competed = extent_competed_code,
      solicitation_procedure = solicitation_procedures_code,
      limited_sources = fair_opportunity_limited_sources_code,
      solicitation_date = ymd(solicitation_date),
      action_date = ymd(action_date),
      performance_start = ymd(period_of_performance_start_date),
      submission_days = as.numeric(action_date - solicitation_date),
      shortened_submission = ifelse(submission_days < 7, 1, 0),
      accelerated = ifelse(
        limited_sources %in% c("A", "B", "C") |
          other_than_full_and_open_competition_code != "N", 1, 0
      ),
      is_modified = ifelse(num_modifications > 0, 1, 0),
      value_increase = ifelse(
        as.numeric(current_total_value_of_award) > 
          as.numeric(potential_total_value_of_award), 1, 0
      ),
      current_end = suppressWarnings(ymd(as.character(period_of_performance_current_end_date))),
      potential_end = suppressWarnings(ymd(as.character(period_of_performance_potential_end_date))),
      contract_length_extended = ifelse(!is.na(current_end) & !is.na(potential_end) & 
                                          current_end < potential_end, 1, 0),
      scope_change = ifelse(value_increase == 1 | contract_length_extended == 1, 1, 0),
      missing_data_ratio = rowMeans(is.na(.)),
      bptw_score = X2020_score_normalized,
      low_bptw_flag = ifelse(X2020_score_normalized < bptw_threshold, 1, 0)
    )
}

# Apply model_indicators function
usagov_features <- model_indicators(dta)



# Handle missing values - replace numeric columns with mean and categorical columns with "Unknown"
preprocess_data <- function(data) {
  # Impute missing numeric data with mean
  numeric_cols <- select(data, where(is.numeric)) %>% names()
  for (col in numeric_cols) {
    data[[col]] <- ifelse(is.na(data[[col]]), mean(data[[col]], na.rm = TRUE), data[[col]])
  }
  
  # Impute missing categorical data with "Unknown"
  categorical_cols <- select(data, where(is.character)) %>% names()
  for (col in categorical_cols) {
    data[[col]] <- ifelse(is.na(data[[col]]), "Unknown", data[[col]])
  }
  
  return(data)
}

# Preprocess the dataset
usagov_features <- preprocess_data(usagov_features)

# Select relevant features for model
model_data <- usagov_features %>%
  select(
    is_excluded_vendor,
    single_bidder,
    # num_offers,
    unpublished_call,
    # shortened_submission,
    # accelerated,
    # is_modified,
    value_increase,
    contract_length_extended,
    scope_change,
    extent_competed_code,
    bptw_score,
    # missing_data_ratio,
    # award_type_code,
    # award_or_idv_flag,
    # contract_financing_code,
    # contract_award_unique_key,
    # type_of_contract_pricing_code,
    potential_total_value_of_award,
    # current_total_value_of_award
  )

# Convert categorical variables to factors
model_data <- model_data %>%
  mutate_if(is.character, as.factor) %>%
  mutate(is_excluded_vendor = as.factor(is_excluded_vendor))

model_data$is_excluded_vendor <- factor(model_data$is_excluded_vendor, levels = c(0, 1), labels = c("class0", "class1"))

set.seed(123)  # For reproducibility
cat_cols <- names(model_data)[sapply(model_data, is.factor)]
levels_count <- sapply(model_data[cat_cols], function(x) length(levels(x)))

# Step 3: Create a training and testing dataset
set.seed(123)
train_index <- createDataPartition(model_data$is_excluded_vendor, p = 0.8, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Display original class distribution

displayProgress <- function(x) {
  cat(paste0("\rIteration: ", x, "   "))
}

# Calculate class weights to handle imbalance
class_counts <- table(train_data$is_excluded_vendor)
print("Class distribution in training data:")
print(class_counts)

# Calculate class weights (inverse of frequency)
class_weights <- 1 / (class_counts / sum(class_counts))
cat("Using class weights:", class_weights[1], "for class0 and", class_weights[2], "for class1\n")

# Create case weights vector for model training
case_weights <- ifelse(train_data$is_excluded_vendor == "class1", 
                       class_weights["class1"], 
                       class_weights["class0"])

# Set up cross-validation with progress tracking and handling class imbalance
ctrl <- trainControl(
  method = "cv",
  number = 5,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  savePredictions = "final",
  verboseIter = TRUE,      # This will show progress during training
  allowParallel = TRUE,
  sampling = "down"        # Use downsampling instead of SMOTE
)

# Define hyperparameter grid with more options for imbalanced data
cat("Setting up random forest model training for imbalanced classification...\n")
rf_grid <- expand.grid(
  mtry = c(2, 3, 4, 5)  # Number of variables randomly sampled at each split
)

cat("Will test", nrow(rf_grid), "hyperparameter combinations with 5-fold cross-validation\n")

ntree_values <- c(100, 300, 500)
nodesize_values <- c(1, 5, 10)

best_models <- list()
best_aucs <- c()

for (ntree in ntree_values) {
  for (nodesize in nodesize_values) {
    cat("Training model with ntree =", ntree, ", nodesize =", nodesize, "\n")
    
    rf_model <- train(
      is_excluded_vendor ~ .,
      data = train_data,
      method = "rf",
      trControl = ctrl,
      tuneGrid = rf_grid,
      metric = "ROC",
      importance = TRUE,
      ntree = ntree,
      nodesize = nodesize,
      weights = case_weights
    )
    
    best_models[[paste(ntree, nodesize, sep = "_")]] <- rf_model
    best_aucs[[paste(ntree, nodesize, sep = "_")]] <- max(rf_model$results$ROC)
  }
}

# Select best model by AUC
best_setting <- names(which.max(best_aucs))
cat("Best hyperparameter combo:", best_setting, "with AUC =", best_aucs[[best_setting]], "\n")
best_model <- best_models[[best_setting]]


# Create a progress bar for model training
cat("\n====== MODEL TRAINING ======\n")
cat("Starting random forest model training (this may take some time)...\n")

# Train the random forest model with caret, handling class imbalance
set.seed(123)
rf_model <- train(
  is_excluded_vendor ~ .,
  data = train_data,
  method = "rf",
  trControl = ctrl,
  tuneGrid = rf_grid,
  metric = "ROC",        # ROC is better than accuracy for imbalanced data
  importance = TRUE,
  ntree = 500,           # Number of trees
  nodesize = 5,          # Minimum size of terminal nodes
  weights = case_weights # Apply class weights
)

cat("\nModel training completed!\n")

# Stop the cluster
stopCluster(cl)

# Print model results
cat("\n====== MODEL RESULTS ======\n")
print(rf_model)
print(rf_model$bestTune)

# Variable importance
cat("\n====== VARIABLE IMPORTANCE ======\n")
var_imp <- varImp(rf_model, scale = TRUE)
print(var_imp)
plot(var_imp)

# Visualizing feature importance
feature_mapping <- c(
  "single_bidder" = "Single Bidder",
  "unpublished_call" = "Unpublished Call",
  "value_increase" = "Value Increase",
  "extent_competed_codeUnknown" = "Extent Competed Unknown",
  "scope_change" = "Scope Change",
  "potential_total_value_of_award" = "Potential Total Value of Contracts",
  "extent_competed_codef" = "Comepted under Simplified Acquisition Threshold",
  "contract_length_extended" = "Contract Length Extended",
  "extent_competed_codeg" = "Not Comepted under Simplified Acquisition Threshold",
  "extent_competed_coded" = "Full and Open Competition After Exlcusion of Resources",
  "extent_competed_codec" = "Not Competed",
  "extent_competed_codeb" = "Not Availabale for Competition",
  "bptw_score" = "BPTW Score"
)
importance_df <- data.frame(
  Feature = rownames(var_imp$importance),
  Importance = var_imp$importance$class0  # or class1, since they're equal
)

importance_df$Feature <- feature_mapping[importance_df$Feature]

importance_df <- importance_df[importance_df$Importance > 0, ]

ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  coord_flip() +
  labs(title = "Random Forest Variable Importance",
       x = "Feature",
       y = "Importance Score") +
  theme_minimal()


# Make predictions with progress tracking
cat("\n====== MODEL EVALUATION ======\n")
pb <- progress_bar$new(
  format = "Making predictions [:bar] :percent eta: :eta",
  total = 2,
  clear = FALSE,
  width = 60
)

# Make predictions on test set
pb$tick()
test_pred <- predict(rf_model, newdata = test_data, type = "prob")
pb$tick()

# Use different threshold for class prediction (instead of default 0.5)
# This can help with imbalanced datasets
threshold <- 0.2  # Lower threshold to catch more minority class instances
test_pred_class <- factor(ifelse(test_pred[, "class1"] > threshold, "class1", "class0"), 
                          levels = c("class0", "class1"))
cat("Predictions completed using threshold of", threshold, "for class1\n\n")

# Evaluate model performance with more focus on imbalanced metrics
cat("Confusion Matrix:\n")
cm <- confusionMatrix(test_pred_class, test_data$is_excluded_vendor)
print(cm)

# Additionally calculate metrics that are better for imbalanced datasets
TP <- cm$table[2, 2]  # True Positives (correctly predicted class1)
FP <- cm$table[2, 1]  # False Positives (predicted class1 but actually class0)
TN <- cm$table[1, 1]  # True Negatives (correctly predicted class0)
FN <- cm$table[1, 2]  # False Negatives (predicted class0 but actually class1)

precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1_score <- 2 * precision * recall / (precision + recall)
specificity <- TN / (TN + FP)
balanced_accuracy <- (recall + specificity) / 2
g_mean <- sqrt(recall * specificity)

cat("\nImbalanced Classification Metrics:\n")
cat("Precision (Positive Predictive Value):", precision, "\n")
cat("Recall (Sensitivity):", recall, "\n")
cat("Specificity:", specificity, "\n")
cat("F1 Score:", f1_score, "\n")
cat("Balanced Accuracy:", balanced_accuracy, "\n")
cat("Geometric Mean:", g_mean, "\n")

# Calculate ROC curve with progress tracking
pb <- progress_bar$new(
  format = "Calculating performance metrics [:bar] :percent eta: :eta",
  total = 4,
  clear = FALSE,
  width = 60
)

pb$tick()
roc_obj <- roc(test_data$is_excluded_vendor, test_pred[, "class1"])
pb$tick()
auc_value <- auc(roc_obj)
cat("\nAUC:", auc_value, "\n")

# Plot ROC curve only
cat("\n====== ROC CURVE ======\n")
plot(roc_obj, main = "ROC Curve for Random Forest Model")

#Balanced model evaluation 
results <- data.frame(
  truth = test_data$is_excluded_vendor,
  prediction = test_pred_class,
  weight = ifelse(test_data$is_excluded_vendor == "class1", 
                  class_weights["class1"], 
                  class_weights["class0"])
)

# Now calculate weighted confusion components
TP <- sum(results$weight[results$truth == "class1" & results$prediction == "class1"])
FP <- sum(results$weight[results$truth == "class0" & results$prediction == "class1"])
TN <- sum(results$weight[results$truth == "class0" & results$prediction == "class0"])
FN <- sum(results$weight[results$truth == "class1" & results$prediction == "class0"])

# Then compute weighted metrics
precision_weighted <- TP / (TP + FP)
recall_weighted <- TP / (TP + FN)
specificity_weighted <- TN / (TN + FP)
f1_weighted <- 2 * precision_weighted * recall_weighted / (precision_weighted + recall_weighted)
balanced_accuracy_weighted <- (recall_weighted + specificity_weighted) / 2
g_mean_weighted <- sqrt(recall_weighted * specificity_weighted)

print(precision_weighted)
print(recall_weighted)
print(f1_weighted)
print(balanced_accuracy_weighted)
print(g_mean_weighted)


weighted_cm <- results %>%
  group_by(truth, prediction) %>%
  summarise(weighted_count = sum(weight), .groups = "drop") %>%
  tidyr::pivot_wider(
    names_from = prediction,
    values_from = weighted_count,
    values_fill = 0
  )

print(weighted_cm)

#cost-sensitive analysis 
# Identify predicted and actual classes
actual <- test_data$is_excluded_vendor
predicted <- test_pred_class  # from earlier threshold-based predictions

# Add predictions to test data
test_data$actual <- actual
test_data$predicted <- predicted

# Calculate FP and FN
false_positives <- test_data[test_data$actual == "class0" & test_data$predicted == "class1", ]
false_negatives <- test_data[test_data$actual == "class1" & test_data$predicted == "class0", ]

# Use potential_total_value_of_award as a proxy for cost
fp_cost <- mean(false_positives$potential_total_value_of_award, na.rm = TRUE)
fn_cost <- mean(false_negatives$potential_total_value_of_award, na.rm = TRUE)

# Print results
cat("Estimated Cost of False Positive (FP):", round(fp_cost, 2), "\n")
cat("Estimated Cost of False Negative (FN):", round(fn_cost, 2), "\n")

# Load required library
library(ggplot2)

# Create a data frame with the cost values
cost_data <- data.frame(
  ErrorType = c("False Positive (FP)", "False Negative (FN)"),
  Cost = c(20878718, 33963096)  # Replace these values with your actual estimates if different
)

# Plot
ggplot(cost_data, aes(x = ErrorType, y = Cost, fill = ErrorType)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  scale_fill_manual(values = c("skyblue", "tomato")) +
  labs(
    title = "Estimated Costs of False Positives and False Negatives",
    x = "Error Type",
    y = "Estimated Cost (in USD)",
    fill = "Error Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  ) +
  scale_y_continuous(labels = scales::comma)



# View all CV results
print(rf_model$results)
best_cv_auc <- max(rf_model$results$ROC)

roc_obj <- roc(test_data$is_excluded_vendor, test_pred[, "class1"])
auc_value <- auc(roc_obj)
cat("Test AUC:", auc_value, "\n")




