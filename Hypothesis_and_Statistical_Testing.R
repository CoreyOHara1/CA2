# SECTION 1: Loading the prepared data and defining hypotheses --------

# Starting from the datasets exported by Data-Preperation.R.
# This script tests the five research questions and saves the results.

# Checking that the CA2 project folder has been set up
project_dir <- "/Users/corey/Documents/Education/Data Analytics/ATU - Big Data Analytics/Data Science/CA 2/CA2"

stopifnot(
  dir.exists(project_dir)
)

# Identifying the folders created during data preparation
data_dir <- file.path(project_dir, "data")
output_dir <- file.path(project_dir, "output")
figures_dir <- file.path(project_dir, "figures")

# Creating the output and figures folders if they do not already exist
dir.create(output_dir, showWarnings = FALSE)
dir.create(figures_dir, showWarnings = FALSE)

# Checking that the prepared RDS files are available
stopifnot(
  file.exists(file.path(data_dir, "price_data.rds")),
  file.exists(file.path(data_dir, "quarter_data.rds")),
  file.exists(file.path(data_dir, "sligo_clean.rds")),
  file.exists(file.path(data_dir, "price_data_all_records.rds")),
  file.exists(file.path(data_dir, "quarter_data_all_records.rds")),
  file.exists(file.path(data_dir, "sligo_all_records.rds"))
)

# Loading the main datasets for the five research questions
price_data <- readRDS(
  file.path(data_dir, "price_data.rds")
)

quarter_data <- readRDS(
  file.path(data_dir, "quarter_data.rds")
)

categorical_data <- readRDS(
  file.path(data_dir, "sligo_clean.rds")
)

# Loading the datasets containing all formatted records
# These will be used later for the duplicate-policy sensitivity check
price_data_all_records <- readRDS(
  file.path(data_dir, "price_data_all_records.rds")
)

quarter_data_all_records <- readRDS(
  file.path(data_dir, "quarter_data_all_records.rds")
)

categorical_data_all_records <- readRDS(
  file.path(data_dir, "sligo_all_records.rds")
)

# Checking the main-analysis datasets before testing
stopifnot(
  nrow(price_data) == 5070,
  nrow(quarter_data) == 675,
  nrow(categorical_data) == 5957,
  inherits(price_data$sale_date, "Date"),
  is.numeric(price_data$price_eur),
  is.factor(price_data$period),
  is.factor(quarter_data$quarter),
  is.factor(categorical_data$period),
  is.factor(categorical_data$property_status),
  is.factor(categorical_data$not_full_market_price)
)

# Checking the sensitivity-check datasets before testing
stopifnot(
  nrow(price_data_all_records) == 5074,
  nrow(quarter_data_all_records) == 676,
  nrow(categorical_data_all_records) == 5964
)

# Checking that the comparison groups contain records
stopifnot(
  all(
    table(
      factor(
        price_data$period,
        levels = c("2019", "2020-2021", "2022-2025")
      )
    ) > 0
  ),
  all(
    table(
      factor(
        quarter_data$quarter,
        levels = c("Q1", "Q2", "Q3", "Q4")
      )
    ) > 0
  ),
  all(
    table(
      factor(
        categorical_data$period,
        levels = c("2019", "2020-2021", "2022-2025")
      )
    ) > 0
  )
)

# Setting the significance level for the five primary tests
alpha <- 0.05

# # CA2 research questions and hypotheses 
#
# F_g(x) means the cumulative recorded-price distribution in group g.
# rho_s means the population Spearman rank correlation.
# p_new,g means the proportion of new records in period g.
# p_nonmarket,g means the proportion flagged Yes for
# not-full-market price in period g.
#
# ## RQ1: Recorded prices across analytical periods
# Dataset: price_data
# Variables: price_eur and period
# H0: F_2019(x) = F_2020-2021(x) = F_2022-2025(x), for all x.
# H1: At least one period has a different price distribution.
# Test: Kruskal-Wallis rank sum test.
#
# ## RQ2: Recorded prices across quarters within 2025
# Dataset: quarter_data
# Variables: price_eur and quarter
# H0: F_Q1(x) = F_Q2(x) = F_Q3(x) = F_Q4(x), for all x.
# H1: At least one 2025 quarter has a different price distribution.
# Test: Kruskal-Wallis rank sum test.
#
# ## RQ3: Sale date and recorded price, 2019-2025
# Dataset: price_data
# Variables: sale_date and price_eur
# H0: rho_s = 0.
# H1: rho_s != 0.
# Test: Spearman rank correlation, two-sided.
#
# ## RQ4: Analytical period and property status
# Dataset: categorical_data
# Variables: period and property_status
# H0: p_new,2019 = p_new,2020-2021 = p_new,2022-2025.
# H1: At least one period has a different proportion of new records.
# Test: Pearson chi-squared test of independence.
#
# ## RQ5: Analytical period and not-full-market-price status
# Dataset: categorical_data
# Variables: period and not_full_market_price
# H0: p_nonmarket,2019 =
#       p_nonmarket,2020-2021 =
#       p_nonmarket,2022-2025.
# H1: At least one period has a different proportion flagged Yes.
# Test: Pearson chi-squared test of independence.
#
# The five primary p-values will be adjusted using Holm's method.
# A Holm-adjusted p-value at or below 0.05 supports rejecting H0.
# A Holm-adjusted p-value above 0.05 means there is insufficient
# evidence to reject H0; it does not prove that H0 is true.
#
# RQ2 concerns quarters within 2025 only. It does not test general
# seasonality across years.
#
# The analysis concerns recorded transactions in the PSRA register.
# It does not measure the changing value of an identical property and
# is not a quality-adjusted property price index.
#
# The tests assume independent observations. Repeat sales, shared
# developments and wider market conditions may create dependence.
# Results are interpreted conditionally on this limitation.

# Displaying the datasets loaded for testing
cat("Prepared datasets loaded successfully.\n")
cat("Price-analysis records:", nrow(price_data), "\n")
cat("Eligible 2025 quarter records:", nrow(quarter_data), "\n")
cat("Categorical-analysis records:", nrow(categorical_data), "\n")
cat("Sensitivity-check records:", nrow(categorical_data_all_records), "\n")
cat("Significance level:", alpha, "\n")








# SECTION 2: Checking price distributions before testing

# Checking that the price datasets and output folders are available
stopifnot(
  exists("price_data"),
  exists("quarter_data"),
  exists("output_dir"),
  exists("figures_dir"),
  exists("alpha")
)

# Defining the coral orange used in the preparation charts
chart_colour <- "#FF7F50"

# Defining a function to check normality within each group
check_normality <- function(data, group_variable, question) {
  
  # Checking that the required variables are available
  stopifnot(
    is.numeric(data$price_eur),
    is.factor(data[[group_variable]]),
    !anyNA(data$price_eur),
    !anyNA(data[[group_variable]])
  )
  
  # Splitting prices into the groups being compared
  grouped_prices <- split(
    data$price_eur,
    data[[group_variable]],
    drop = TRUE
  )
  
  # Checking each group separately
  normality_summary <- do.call(
    rbind,
    lapply(names(grouped_prices), function(group_name) {
      
      group_prices <- grouped_prices[[group_name]]
      
      # Checking that Shapiro-Wilk can be applied
      stopifnot(
        length(group_name) > 0,
        length(group_prices) >= 3,
        length(group_prices) <= 5000,
        !anyNA(group_prices),
        all(is.finite(group_prices)),
        length(unique(group_prices)) > 1
      )
      
      # Running the Shapiro-Wilk normality test
      normality_test <- shapiro.test(group_prices)
      
      data.frame(
        question = question,
        group = group_name,
        records = length(group_prices),
        W = unname(normality_test$statistic),
        p_value = normality_test$p.value,
        row.names = NULL
      )
    })
  )
  
  rownames(normality_summary) <- NULL
  
  return(normality_summary)
}

# Checking normality for the three comparison periods
normality_summary <- check_normality(
  price_data,
  "period",
  "RQ1"
)

cat("\nRQ1: Normality checks by period\n")
print(normality_summary, row.names = FALSE)

# Checking normality across quarters within 2025
quarter_normality_summary <- check_normality(
  quarter_data,
  "quarter",
  "RQ2"
)

cat("\nRQ2: Normality checks by quarter within 2025\n")
print(quarter_normality_summary, row.names = FALSE)

# Checking that every price group was checked
stopifnot(
  sum(normality_summary$records) == nrow(price_data),
  sum(quarter_normality_summary$records) == nrow(quarter_data)
)

# Saving the normality results for the report
write.csv(
  normality_summary,
  file.path(output_dir, "01_RQ1_normality_by_period.csv"),
  row.names = FALSE
)

write.csv(
  quarter_normality_summary,
  file.path(output_dir, "02_RQ2_normality_by_quarter.csv"),
  row.names = FALSE
)


# Creating the RQ1 normality charts 

# Splitting eligible prices into the three periods
period_prices <- split(
  price_data$price_eur,
  price_data$period,
  drop = TRUE
)

# Opening a PNG file for the period diagnostic charts
png(
  file.path(figures_dir, "04_RQ1_period_normality.png"),
  width = 2000,
  height = 650 * length(period_prices),
  res = 180
)

# Arranging one histogram and one Q-Q plot for each period
par(
  mfrow = c(length(period_prices), 2),
  mar = c(4, 4, 3, 1),
  las = 1
)

# Creating a histogram and Q-Q plot for each period
for (period_name in names(period_prices)) {
  
  period_prices_in_eur_thousands <-
    period_prices[[period_name]] / 1000
  
  hist(
    period_prices_in_eur_thousands,
    breaks = 30,
    col = chart_colour,
    border = "white",
    main = paste(
      period_name,
      "prices; n =",
      length(period_prices[[period_name]])
    ),
    xlab = "Recorded price (EUR thousands)",
    ylab = "Records"
  )
  
  qqnorm(
    period_prices_in_eur_thousands,
    main = paste(period_name, "normal Q-Q plot"),
    xlab = "Normal quantile",
    ylab = "Recorded price (EUR thousands)",
    pch = 16,
    col = chart_colour
  )
  
  qqline(
    period_prices_in_eur_thousands,
    col = "black",
    lwd = 2
  )
}

# Closing the PNG file
dev.off()


# Creating the RQ2 normality charts 

# Splitting eligible 2025 prices into the four quarters
quarter_prices <- split(
  quarter_data$price_eur,
  quarter_data$quarter,
  drop = TRUE
)

# Opening a PNG file for the quarter diagnostic charts
png(
  file.path(figures_dir, "05_RQ2_2025_quarter_normality.png"),
  width = 2000,
  height = 650 * length(quarter_prices),
  res = 180
)

# Arranging one histogram and one Q-Q plot for each quarter
par(
  mfrow = c(length(quarter_prices), 2),
  mar = c(4, 4, 3, 1),
  las = 1
)

# Creating a histogram and Q-Q plot for each quarter
for (quarter_name in names(quarter_prices)) {
  
  quarter_prices_in_eur_thousands <-
    quarter_prices[[quarter_name]] / 1000
  
  hist(
    quarter_prices_in_eur_thousands,
    breaks = 30,
    col = chart_colour,
    border = "white",
    main = paste(
      quarter_name,
      "2025 prices; n =",
      length(quarter_prices[[quarter_name]])
    ),
    xlab = "Recorded price (EUR thousands)",
    ylab = "Records"
  )
  
  qqnorm(
    quarter_prices_in_eur_thousands,
    main = paste(quarter_name, "2025 normal Q-Q plot"),
    xlab = "Normal quantile",
    ylab = "Recorded price (EUR thousands)",
    pch = 16,
    col = chart_colour
  )
  
  qqline(
    quarter_prices_in_eur_thousands,
    col = "black",
    lwd = 2
  )
}

# Closing the PNG file
dev.off()


# Checking that both diagnostic charts were created 

normality_charts <- file.path(
  figures_dir,
  c(
    "04_RQ1_period_normality.png",
    "05_RQ2_2025_quarter_normality.png"
  )
)

stopifnot(
  all(file.exists(normality_charts)),
  all(file.info(normality_charts)$size > 0)
)

# Displaying the completed section
cat(
  "\nSection 2 completed.",
  "\nRQ1 price records checked:", nrow(price_data),
  "\nRQ2 2025 price records checked:", nrow(quarter_data),
  "\nNormality diagnostic charts saved:", length(normality_charts),
  "\n"
)

print(normality_charts)







# SECTION 3: Testing recorded prices across periods

# Checking that the required objects from Section 2 are available
stopifnot(
  exists("price_data"),
  exists("output_dir"),
  exists("figures_dir"),
  exists("alpha"),
  exists("normality_summary"),
  nrow(price_data) == 5070
)

# Checking that the RQ1 variables are suitable for testing
stopifnot(
  is.numeric(price_data$price_eur),
  is.factor(price_data$period),
  !anyNA(price_data$price_eur),
  !anyNA(price_data$period)
)

# Confirming the RQ1 hypothesis before testing
cat(
  "\nRQ1 hypothesis",
  "\nH0: The recorded-price distributions are the same across 2019, 2020-2021 and 2022-2025.",
  "\nH1: At least one period has a different recorded-price distribution.",
  "\nTest: Kruskal-Wallis rank sum test.",
  "\n"
)

# Running the Kruskal-Wallis test for RQ1
rq1_test <- kruskal.test(
  price_eur ~ period,
  data = price_data
)

# Displaying the RQ1 test result
cat("\nRQ1: Kruskal-Wallis rank sum test\n")
print(rq1_test)

# Checking that the test returned a valid result
stopifnot(
  inherits(rq1_test, "htest"),
  is.finite(rq1_test$statistic),
  is.finite(rq1_test$p.value)
)

# Calculating epsilon-squared from the Kruskal-Wallis statistic
rq1_epsilon_squared <-
  (rq1_test$statistic - length(unique(price_data$period)) + 1) /
  (nrow(price_data) - length(unique(price_data$period)))

# Displaying the RQ1 effect size
cat(
  "\nRQ1 epsilon-squared effect size:",
  round(unname(rq1_epsilon_squared), 4),
  "\n"
)

# Creating a compact table of RQ1 descriptive statistics
rq1_descriptives <- do.call(
  rbind,
  lapply(
    split(
      price_data$price_eur,
      price_data$period,
      drop = TRUE
    ),
    function(prices) {
      data.frame(
        records = length(prices),
        median_eur = median(prices),
        mean_eur = mean(prices),
        q1_eur = quantile(prices, 0.25),
        q3_eur = quantile(prices, 0.75),
        minimum_eur = min(prices),
        maximum_eur = max(prices),
        row.names = NULL
      )
    }
  )
)

# Adding the period names to the descriptive table
rq1_descriptives$period <- names(
  split(
    price_data$price_eur,
    price_data$period,
    drop = TRUE
  )
)

# Reordering the descriptive table
rq1_descriptives <- rq1_descriptives[
  c("period", "records", "median_eur", "mean_eur", "q1_eur", "q3_eur", "minimum_eur", "maximum_eur")
]

# Displaying the RQ1 descriptive statistics
cat("\nRQ1: Recorded-price descriptive statistics by period\n")
print(rq1_descriptives, row.names = FALSE)

# Saving the RQ1 descriptive statistics
write.csv(
  rq1_descriptives,
  file.path(output_dir, "03_RQ1_price_descriptives_by_period.csv"),
  row.names = FALSE
)

# Creating all pairwise period comparisons
period_pairs <- combn(
  levels(droplevels(price_data$period)),
  2,
  simplify = FALSE
)

# Running a Wilcoxon rank-sum test for each pair
rq1_pairwise_results <- do.call(
  rbind,
  lapply(period_pairs, function(pair) {
    
    # Selecting the two periods being compared
    pairwise_data <- price_data[
      price_data$period %in% pair,
    ]
    
    # Dropping unused factor levels
    pairwise_data$period <-
      droplevels(pairwise_data$period)
    
    # Running the pairwise test
    pairwise_test <- wilcox.test(
      price_eur ~ period,
      data = pairwise_data,
      alternative = "two.sided",
      exact = FALSE
    )
    
    data.frame(
      comparison = paste(pair, collapse = " vs "),
      W = unname(pairwise_test$statistic),
      p_value = pairwise_test$p.value,
      row.names = NULL
    )
  })
)

# Adjusting the pairwise p-values using Holm's method
rq1_pairwise_results$p_value_holm <- p.adjust(
  rq1_pairwise_results$p_value,
  method = "holm"
)

# Identifying comparisons that remain significant after adjustment
rq1_pairwise_results$significant_at_0.05 <-
  rq1_pairwise_results$p_value_holm <= alpha

# Displaying the RQ1 pairwise results
cat("\nRQ1: Pairwise Wilcoxon rank-sum tests with Holm adjustment\n")
print(rq1_pairwise_results, row.names = FALSE)

# Saving the RQ1 test results
rq1_results <- data.frame(
  question = "RQ1",
  test = "Kruskal-Wallis rank sum test",
  statistic = unname(rq1_test$statistic),
  df = unname(rq1_test$parameter),
  p_value = rq1_test$p.value,
  epsilon_squared = unname(rq1_epsilon_squared),
  alpha = alpha,
  decision = ifelse(
    rq1_test$p.value <= alpha,
    "Reject H0",
    "Insufficient evidence to reject H0"
  ),
  row.names = NULL
)

write.csv(
  rq1_results,
  file.path(output_dir, "04_RQ1_kruskal_wallis_result.csv"),
  row.names = FALSE
)

write.csv(
  rq1_pairwise_results,
  file.path(output_dir, "05_RQ1_pairwise_wilcoxon_results.csv"),
  row.names = FALSE
)


# Creating the RQ1 price distribution chart

# Opening a PNG file for the RQ1 chart
png(
  file.path(figures_dir, "06_RQ1_price_distributions_by_period.png"),
  width = 2200,
  height = 1300,
  res = 180
)

# Preparing plotting limits based on all eligible prices
price_limits <- range(price_data$price_eur) / 1000

# Creating a boxplot of prices by period
boxplot(
  price_eur / 1000 ~ period,
  data = price_data,
  col = chart_colour,
  border = "black",
  horizontal = TRUE,
  las = 1,
  main = "Recorded prices by analytical period",
  xlab = "Recorded price (EUR thousands)",
  ylab = "Analytical period",
  ylim = price_limits,
  outline = FALSE
)

# Adding individual observations to show the full distribution
stripchart(
  price_eur / 1000 ~ period,
  data = price_data,
  method = "jitter",
  vertical = FALSE,
  pch = 16,
  cex = 0.35,
  col = adjustcolor("black", alpha.f = 0.15),
  add = TRUE
)

# Closing the PNG file
dev.off()


# Checking that the RQ1 outputs were created 

rq1_outputs <- file.path(
  output_dir,
  c(
    "03_RQ1_price_descriptives_by_period.csv",
    "04_RQ1_kruskal_wallis_result.csv",
    "05_RQ1_pairwise_wilcoxon_results.csv"
  )
)

rq1_chart <- file.path(
  figures_dir,
  "06_RQ1_price_distributions_by_period.png"
)

stopifnot(
  all(file.exists(rq1_outputs)),
  all(file.info(rq1_outputs)$size > 0),
  file.exists(rq1_chart),
  file.info(rq1_chart)$size > 0
)

# Displaying the completed section
cat(
  "\nSection 3 completed.",
  "\nRQ1 Kruskal-Wallis statistic:", round(unname(rq1_test$statistic), 3),
  "\nRQ1 p-value:", format.pval(rq1_test$p.value, digits = 4),
  "\nRQ1 epsilon-squared:", round(unname(rq1_epsilon_squared), 4),
  "\nRQ1 pairwise comparisons:", nrow(rq1_pairwise_results),
  "\nRQ1 chart saved:", rq1_chart,
  "\n"
)

print(rq1_outputs)
print(rq1_chart)