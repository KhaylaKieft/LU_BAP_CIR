library(httr)
library(stringr)
library(readr)
library(progress) 

send_to_gpt <- function(text, api_key, model = "gpt-4o-mini") {
  
  url <- "https://api.openai.com/v1/chat/completions"
  
  # Send API request
  response <- POST(
    url = url,
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    encode = "json",
    body = list(
      model = model,  # Use "gpt-4o-mini" (or change to "gpt-4o")
      messages = list(list(role = "user", content = text))
    )
  )
  
  # Parse API response
  parsed_response <- content(response, as = "parsed")
  
  # Extract message safely
  if (!is.null(parsed_response$choices) && length(parsed_response$choices) > 0) {
    return(parsed_response$choices[[1]]$message$content)
  } else {
    print("Error: No valid response from API.")
    return(NULL)
  }
}

# Load CSV file containing sms data 
smsData <- read.csv("https://raw.githubusercontent.com/babakrezaee/MethodsCourses/master/DataSets/sms_spam.csv", 
                    stringsAsFactors = FALSE, encoding="UTF-8")
# Check structure of dataset
smsData <- smsData[1:30, ] #creates the subset 
head(smsData)
colnames(smsData)
nrow(smsData)


# Define API Key (Ensure you use your API key, as this one will be disabled soon!)
api_key <- # insert api key here
# Function to get sentiment & tone from GPT
analyze_tweet <- function(text) {
  prompt <- paste(
    "Analyze the following text messages",
    "Classify whether the sms is Spam or Ham",
    "Provide the response in the following format:",
    "'Type: [type]'",
    "\n\ntext:", text #\n\n says to go to next paragraph 
  )
  
  response <- send_to_gpt(prompt, api_key)
  
  if (!is.null(response)) {
    # Extract text type using regex
    type_match <- str_extract(response, "Type:\\s*(\\w+)")
    
    type <- ifelse(!is.na(type_match), gsub("Type:\\s*", "", type_match), NA)
    
    return(c(type))
  } else {
    return(c(NA, NA))
  }
}

# Define an empty matrix to store results
results <- matrix(NA, nrow = nrow(smsData), ncol = 1)

# Set column names
colnames(results) <- c("Text Type")

# Initialize progress bar
pb <- txtProgressBar(min = 0, max = nrow(smsData), style = 3)

# Loop through each tweet
for (i in 1:nrow(smsData)) {
  results[i, ] <- analyze_tweet(smsData$text[i])  # Analyze tweet and store result
  
  setTxtProgressBar(pb, i)  # Update progress bar
}
close(pb)  # Close progress bar

# Convert results to a dataframe
results_df <- as.data.frame(results, stringsAsFactors = FALSE)
colnames(results_df) <- c("Text Type")

# Merge results with original dataset
smsData <- cbind(smsData, results_df)

# Save to CSV
write_csv(smsData, "sms_data_spam_ham.csv")

# View results
head(smsData)

smsData$predicted <- tolower(smsData$`Text Type`)
smsData$actual <- tolower(smsData$type)

# Create the confusion matrix
conf_matrix <- table(
  Predicted = smsData$predicted,
  Actual = smsData$actual
)

# Print the confusion matrix
print("Confusion Matrix:")
print(conf_matrix)

# Calculate performance metrics
total <- sum(conf_matrix)
accuracy <- sum(diag(conf_matrix)) / total
precision <- conf_matrix["spam", "spam"] / sum(conf_matrix["spam", ])
recall <- conf_matrix["spam", "spam"] / sum(conf_matrix[, "spam"])
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print performance metrics
cat("\nPerformance Metrics:\n")
cat("Accuracy:", round(accuracy, 3), "\n")
cat("Precision:", round(precision, 3), "\n")
cat("Recall:", round(recall, 3), "\n")
cat("F1 Score:", round(f1_score, 3), "\n")

