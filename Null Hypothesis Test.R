# Step 1: Read the data from the text file
data <- read.table("~/OneDrive/Documents/MBA Data.Finance Material/Quantitative Finance/Analyzing_Fin_Time_Series/d-3stocks9908/d-3stocks9908.txt", sep = "", header = TRUE)

# Step 2: Extract price data (excluding the date column)
prices <- data[, -1]

# Step 3: Remove rows with zero values
prices_clean <- prices[rowSums(prices == 0) == 0, ]

# Step 4: Calculate log returns
log_returns <- as.data.frame(lapply(prices_clean, function(x) diff(log(x))))

# Step 5: Convert log returns to percentages
log_returns_percent <- log_returns * 100

# Step 6: Remove rows with missing values in log returns
log_returns_percent_clean <- na.omit(log_returns_percent)

# Step 7: Set significance level
alpha <- 0.05

# Define the names of the stocks you want to test
stocks <- colnames(log_returns_percent_clean)

# Perform t-tests for each stock
test_results <- list()

for (stock in stocks) {
  stock_data <- log_returns_percent_clean[[stock]]
  
  # Perform one-sample t-test
  test <- t.test(stock_data, mu = 0)
  
  # Store results
  test_results[[stock]] <- list(
    t_statistic = test$statistic,
    p_value = test$p.value,
    reject_null = test$p.value < alpha
  )
}

# Print the results for each stock
for (stock in stocks) {
  cat("\nResults for", stock, ":\n")
  cat("  T-Statistic:", test_results[[stock]]$t_statistic, "\n")
  cat("  P-Value:", test_results[[stock]]$p_value, "\n")
  if (test_results[[stock]]$reject_null) {
    cat("  Conclusion: Reject the null hypothesis (mean is significantly different from zero)\n")
  } else {
    cat("  Conclusion: Fail to reject the null hypothesis (mean is not significantly different from zero)\n")
  }
}
