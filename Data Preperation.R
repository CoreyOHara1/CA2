# SECTION 1: Importing and inspecting the CA1 master ------------------
# Starting with the combined dataset from CA1, but keeping the original CSV unchanged.

# Checking project setup has been run
stopifnot(
  exists("project_dir"),
  exists("input_file"),
  file.exists(input_file)
)

# Creating a folder for the prep outputs
output_dir <- file.path(project_dir, "output")
dir.create(output_dir, showWarnings = FALSE)

# Reading master as text before setting the variable types
raw <- read.csv(
  input_file,
  check.names = FALSE,
  colClasses = "character",
  na.strings = c("", "NA"),
  fileEncoding = "UTF-8"
)

# Checking column names
expected_columns <- c(
  "Date of Sale (dd/mm/yyyy)",
  "Address",
  "County",
  "Eircode",
  "Price (\u20ac)",
  "Not Full Market Price",
  "VAT Exclusive",
  "Description of Property",
  "Property Size Description"
)

stopifnot(identical(names(raw), expected_columns))

# Checking the number of records, column names and current types
cat("Records imported:", nrow(raw), "\n")
cat("Variables imported:", ncol(raw), "\n")

names(raw)
str(raw)

# Counting missing values in each column
missing_audit <- data.frame(
  variable = names(raw),
  missing_records = colSums(is.na(raw)),
  missing_percent = round(100 * colMeans(is.na(raw)), 2),
  row.names = NULL
)

print(missing_audit, row.names = FALSE)

# Counting additional exact copies without removing them
duplicate_count <- sum(duplicated(raw))
cat("Additional exact copies:", duplicate_count, "\n")

# Checking the county and the values used in the categorical fields
table(raw[["County"]], useNA = "ifany")
table(raw[["Not Full Market Price"]], useNA = "ifany")
table(raw[["VAT Exclusive"]], useNA = "ifany")
table(raw[["Description of Property"]], useNA = "ifany")

# Saving the missing-value audit for the report
write.csv(
  missing_audit,
  file.path(output_dir, "01_missing_value_audit.csv"),
  row.names = FALSE
)


# SECTION 2: Formatting the variables

# Checking that the imported data and output folder are available
stopifnot(
  exists("raw"),
  exists("output_dir")
)

# Creating a working copy and keeping raw unchanged
sligo <- raw

# Renaming the columns for use in the analysis
names(sligo) <- c(
  "sale_date",
  "address",
  "county",
  "eircode",
  "price_eur",
  "not_full_market_price",
  "vat_exclusive",
  "property_status",
  "property_size_description"
)

# Removing surrounding spaces and treating empty text as missing
sligo[] <- lapply(sligo, function(x) {
  x <- trimws(x)
  x[!is.na(x) & x == ""] <- NA_character_
  x
})

# Keeping the date text for checking the conversion
original_dates <- sligo$sale_date

# Converting the dates from day/month/year
sligo$sale_date <- as.Date(
  original_dates,
  format = "%d/%m/%Y"
)

# Removing currency symbols and commas before converting prices
sligo$price_eur <- as.numeric(
  gsub("[\u20ac,]", "", sligo$price_eur)
)

# Checking for failed conversions and non-positive prices
stopifnot(
  !anyNA(sligo$sale_date),
  !anyNA(sligo$price_eur),
  all(is.finite(sligo$price_eur)),
  all(sligo$price_eur > 0),
  all(
    format(sligo$sale_date, "%d/%m/%Y") == original_dates
  )
)

# Checking the values used in the Yes/No fields
stopifnot(
  !anyNA(sligo$not_full_market_price),
  !anyNA(sligo$vat_exclusive),
  all(sligo$not_full_market_price %in% c("No", "Yes")),
  all(sligo$vat_exclusive %in% c("No", "Yes"))
)

# Converting the Yes/No fields to factors
sligo$not_full_market_price <- factor(
  sligo$not_full_market_price,
  levels = c("No", "Yes")
)

sligo$vat_exclusive <- factor(
  sligo$vat_exclusive,
  levels = c("No", "Yes")
)

# Checking the property descriptions before shortening the labels
property_labels <- c(
  "New Dwelling house /Apartment",
  "Second-Hand Dwelling house /Apartment"
)

stopifnot(
  !anyNA(sligo$property_status),
  all(sligo$property_status %in% property_labels)
)

# Converting property status to a factor
sligo$property_status <- factor(
  sligo$property_status,
  levels = property_labels,
  labels = c("New", "Second-hand")
)

# Checking the county and converting it to a factor
stopifnot(
  !anyNA(sligo$county),
  all(sligo$county == "Sligo")
)

sligo$county <- factor(sligo$county)

# Checking that all imported records are still present
stopifnot(nrow(sligo) == nrow(raw))

# Inspecting the updated structure, date range and prices
str(sligo)
range(sligo$sale_date)
summary(sligo$price_eur)

# Checking the category counts after conversion
table(sligo$property_status, useNA = "ifany")
table(sligo$not_full_market_price, useNA = "ifany")
table(sligo$vat_exclusive, useNA = "ifany")

# Recording the updated variable types
variable_types <- data.frame(
  variable = names(sligo),
  data_type = vapply(
    sligo,
    function(x) paste(class(x), collapse = ", "),
    character(1)
  ),
  row.names = NULL
)

print(variable_types, row.names = FALSE)

# Saving the variable types for the report
write.csv(
  variable_types,
  file.path(output_dir, "02_variable_types.csv"),
  row.names = FALSE
)

cat(
  "\nSection 2 completed. Records retained:",
  nrow(sligo),
  "\n"
)



# SECTION 3: Reviewing the exact copies

# Checking that the original and formatted datasets are available
stopifnot(
  exists("raw"),
  exists("sligo"),
  nrow(raw) == nrow(sligo)
)

# Identifying additional exact copies in the original imported data
duplicate_rows <- duplicated(raw)

# Identifying all matching rows, including their first occurrences
matching_rows <- duplicate_rows |
  duplicated(raw, fromLast = TRUE)

# Creating a review table using the formatted values
# Keeping source positions aligned with the original imported data
duplicate_review <- data.frame(
  source_row = which(matching_rows),
  additional_copy = duplicate_rows[matching_rows],
  sligo[matching_rows, ],
  row.names = NULL
)

# Sorting matching records together for easier inspection
duplicate_review <- duplicate_review[
  order(
    duplicate_review$sale_date,
    duplicate_review$address,
    duplicate_review$price_eur,
    duplicate_review$source_row
  ),
]

# Displaying the fields needed for the initial review
print(
  duplicate_review[
    c(
      "source_row",
      "additional_copy",
      "sale_date",
      "address",
      "price_eur",
      "property_status",
      "not_full_market_price",
      "vat_exclusive"
    )
  ],
  row.names = FALSE
)

# Opening all fields in the RStudio data viewer
View(duplicate_review)

# Summarising the review without removing any records
cat("Additional exact copies:", sum(duplicate_rows), "\n")
cat("Records involved in exact matches:", sum(matching_rows), "\n")
cat("Records currently retained:", nrow(sligo), "\n")



# SECTION 4: Applying the duplicate policy

# Checking that the imported and formatted datasets are available
stopifnot(
  exists("raw"),
  exists("sligo"),
  exists("output_dir"),
  nrow(raw) == nrow(sligo)
)

# Identifying exact copies using all original imported columns
duplicate_rows <- duplicated(raw)

# Keeping all formatted records for the later sensitivity check
sligo_all_records <- sligo

# Retaining the first occurrence of each exact match
# Keeping sligo unchanged so this section can be rerun
sligo_clean <- sligo_all_records[!duplicate_rows, ]

# Resetting row names after subsetting
rownames(sligo_clean) <- NULL

# Checking the retained records against the original duplicate flags
stopifnot(
  nrow(sligo_all_records) == nrow(raw),
  nrow(sligo_clean) == nrow(raw) - sum(duplicate_rows),
  !anyDuplicated(raw[!duplicate_rows, ])
)

# Recording the effect of the duplicate policy
duplicate_audit <- data.frame(
  stage = c(
    "Records before duplicate handling",
    "Additional exact copies set aside",
    "Records retained for main analysis"
  ),
  records = c(
    nrow(sligo_all_records),
    sum(duplicate_rows),
    nrow(sligo_clean)
  )
)

print(duplicate_audit, row.names = FALSE)

# Saving the aggregate counts for the report
write.csv(
  duplicate_audit,
  file.path(output_dir, "03_duplicate_handling_audit.csv"),
  row.names = FALSE
)

# Displaying the sizes of both working datasets
cat(
  "\nRecords retained for the main analysis:",
  nrow(sligo_clean),
  "\n"
)

cat(
  "Records retained for the sensitivity check:",
  nrow(sligo_all_records),
  "\n"
)

# Recording the decision for the report
duplicate_policy <- c(
  "Exact matches were identified across all nine imported columns.",
  "The first occurrence of each exact match was retained for the main analysis.",
  "Seven additional exact copies were set aside under this policy.",
  "The full formatted dataset was retained for a later sensitivity check.",
  "Exact matching does not establish that the source records are erroneous."
)

writeLines(
  duplicate_policy,
  file.path(output_dir, "03_duplicate_policy.txt")
)




# SECTION 5: Creating the calendar variables

# Checking that both datasets and the output folder are available
stopifnot(
  exists("sligo_clean"),
  exists("sligo_all_records"),
  exists("output_dir")
)

# Recording the current row counts for checking afterwards
clean_count <- nrow(sligo_clean)
all_records_count <- nrow(sligo_all_records)

# Defining the calendar variables once for use in both datasets
add_calendar_variables <- function(data) {
  
  # Checking the date format and missing dates
  stopifnot(
    inherits(data$sale_date, "Date"),
    !anyNA(data$sale_date)
  )
  
  # Extracting the sale year and month
  data$year <- as.integer(format(data$sale_date, "%Y"))
  data$month <- as.integer(format(data$sale_date, "%m"))
  
  # Checking that dates fall within the project scope
  stopifnot(
    all(data$year %in% 2019:2025),
    all(data$month %in% 1:12)
  )
  
  # Grouping months into calendar quarters
  # Q1: January-March, Q2: April-June
  # Q3: July-September, Q4: October-December
  data$quarter <- factor(
    (data$month - 1L) %/% 3L + 1L,
    levels = 1:4,
    labels = c("Q1", "Q2", "Q3", "Q4")
  )
  
  # Grouping years into the three comparison periods
  data$period <- factor(
    ifelse(
      data$year == 2019,
      "2019",
      ifelse(data$year <= 2021, "2020-2021", "2022-2025")
    ),
    levels = c("2019", "2020-2021", "2022-2025")
  )
  
  # Checking that every record has a quarter and period
  stopifnot(
    !anyNA(data$quarter),
    !anyNA(data$period)
  )
  
  return(data)
}

# Adding the same variables to both datasets
sligo_clean <- add_calendar_variables(sligo_clean)

sligo_all_records <- add_calendar_variables(
  sligo_all_records
)

# Checking that no records were removed
stopifnot(
  nrow(sligo_clean) == clean_count,
  nrow(sligo_all_records) == all_records_count
)

# Inspecting the new variables
head(
  sligo_clean[
    c("sale_date", "year", "month", "quarter", "period")
  ],
  10
)

str(
  sligo_clean[
    c("sale_date", "year", "month", "quarter", "period")
  ]
)

# Counting the main-analysis records in each year
year_counts <- data.frame(
  year = 2019:2025,
  records = as.integer(
    table(factor(sligo_clean$year, levels = 2019:2025))
  )
)

print(year_counts, row.names = FALSE)

# Counting the records in each comparison period
period_counts <- data.frame(
  period = levels(sligo_clean$period),
  records = as.integer(table(sligo_clean$period))
)

print(period_counts, row.names = FALSE)

# Checking the number of records in each quarter by year
quarter_counts <- with(
  sligo_clean,
  table(
    year = factor(year, levels = 2019:2025),
    quarter = quarter
  )
)

print(quarter_counts)

# Checking monthly coverage without assuming every day has a sale
monthly_counts <- with(
  sligo_clean,
  table(
    year = factor(year, levels = 2019:2025),
    month = factor(month, levels = 1:12)
  )
)

print(monthly_counts)

# Counting year-month combinations with no recorded sales
cat(
  "\nYear-month combinations with no records:",
  sum(monthly_counts == 0),
  "\n"
)

# Checking that the summaries account for all retained records
stopifnot(
  sum(year_counts$records) == nrow(sligo_clean),
  sum(period_counts$records) == nrow(sligo_clean),
  sum(quarter_counts) == nrow(sligo_clean),
  sum(monthly_counts) == nrow(sligo_clean)
)

# Saving the coverage summaries for the report
write.csv(
  year_counts,
  file.path(output_dir, "04_year_counts.csv"),
  row.names = FALSE
)

write.csv(
  period_counts,
  file.path(output_dir, "04_period_counts.csv"),
  row.names = FALSE
)

write.csv(
  as.data.frame(quarter_counts),
  file.path(output_dir, "04_year_quarter_counts.csv"),
  row.names = FALSE
)

write.csv(
  as.data.frame(monthly_counts),
  file.path(output_dir, "04_year_month_counts.csv"),
  row.names = FALSE
)

# Displaying the final counts for this section
cat(
  "\nSection 5 completed.",
  "\nMain-analysis records:", nrow(sligo_clean),
  "\nSensitivity-check records:", nrow(sligo_all_records),
  "\n"
)




# SECTION 6: Reviewing missing fields and extreme prices

# Checking that both working datasets and the output folder exist
stopifnot(
  exists("sligo_clean"),
  exists("sligo_all_records"),
  exists("output_dir")
)

# Recording the datasets before the review
clean_before_review <- sligo_clean
all_before_review <- sligo_all_records


# Reviewing missing fields 

# Counting missing values after the duplicate policy
missing_review <- data.frame(
  variable = names(sligo_clean),
  missing_records = colSums(is.na(sligo_clean)),
  missing_percent = round(
    100 * colMeans(is.na(sligo_clean)),
    2
  ),
  row.names = NULL
)

print(missing_review, row.names = FALSE)

# Checking the fields required for the five research questions
required_fields <- c(
  "sale_date",
  "price_eur",
  "not_full_market_price",
  "vat_exclusive",
  "property_status",
  "year",
  "month",
  "quarter",
  "period"
)

stopifnot(
  all(required_fields %in% names(sligo_clean)),
  all(complete.cases(sligo_clean[required_fields]))
)

# Recording the missing-field decisions
missing_decisions <- data.frame(
  variable = c(
    "eircode",
    "property_size_description"
  ),
  decision = c(
    "Retaining records with missing Eircodes; location-level analysis is not planned.",
    "Retaining records with missing size descriptions; property size is not used in the five questions."
  )
)

print(missing_decisions, row.names = FALSE)


# Reviewing extreme recorded prices 

# Checking that all recorded prices are usable numeric values
stopifnot(
  is.numeric(sligo_clean$price_eur),
  !anyNA(sligo_clean$price_eur),
  all(is.finite(sligo_clean$price_eur)),
  all(sligo_clean$price_eur > 0)
)

# Calculating the quartiles and interquartile range
price_quartiles <- quantile(
  sligo_clean$price_eur,
  probs = c(0.25, 0.75),
  names = FALSE
)

price_iqr <- IQR(sligo_clean$price_eur)

lower_fence <- price_quartiles[1] - 1.5 * price_iqr
upper_fence <- price_quartiles[2] + 1.5 * price_iqr

# Flagging prices outside the review thresholds
low_price_flag <- sligo_clean$price_eur < lower_fence
high_price_flag <- sligo_clean$price_eur > upper_fence

extreme_price_flag <- low_price_flag | high_price_flag

# Summarising the thresholds and flagged record counts
price_review_summary <- data.frame(
  measure = c(
    "First quartile (EUR)",
    "Third quartile (EUR)",
    "Interquartile range (EUR)",
    "Lower review threshold (EUR)",
    "Upper review threshold (EUR)",
    "Records below lower threshold",
    "Records above upper threshold",
    "Total records flagged"
  ),
  value = c(
    price_quartiles[1],
    price_quartiles[2],
    price_iqr,
    lower_fence,
    upper_fence,
    sum(low_price_flag),
    sum(high_price_flag),
    sum(extreme_price_flag)
  )
)

print(price_review_summary, row.names = FALSE)

# Inspecting the ten lowest and highest recorded prices
# Omitting addresses from the printed review
review_fields <- c(
  "sale_date",
  "price_eur",
  "property_status",
  "not_full_market_price",
  "vat_exclusive"
)

lowest_prices <- sligo_clean[
  head(order(sligo_clean$price_eur), 10),
  review_fields
]

highest_prices <- sligo_clean[
  head(order(sligo_clean$price_eur, decreasing = TRUE), 10),
  review_fields
]

cat("\nTen lowest recorded prices:\n")
print(lowest_prices, row.names = FALSE)

cat("\nTen highest recorded prices:\n")
print(highest_prices, row.names = FALSE)


# Reviewing property status and VAT flags 

status_vat_counts <- with(
  sligo_clean,
  table(
    property_status = property_status,
    vat_exclusive = vat_exclusive
  )
)

print(status_vat_counts)

# Counting combinations for review without treating them as errors
new_vat_no <- with(
  sligo_clean,
  property_status == "New" & vat_exclusive == "No"
)

second_hand_vat_yes <- with(
  sligo_clean,
  property_status == "Second-hand" & vat_exclusive == "Yes"
)

vat_review_summary <- data.frame(
  combination = c(
    "New property with VAT Exclusive = No",
    "Second-hand property with VAT Exclusive = Yes"
  ),
  records = c(
    sum(new_vat_no),
    sum(second_hand_vat_yes)
  )
)

print(vat_review_summary, row.names = FALSE)


# Saving the review evidence 

write.csv(
  missing_review,
  file.path(output_dir, "05_missing_values_after_duplicates.csv"),
  row.names = FALSE
)

write.csv(
  missing_decisions,
  file.path(output_dir, "05_missing_field_decisions.csv"),
  row.names = FALSE
)

write.csv(
  price_review_summary,
  file.path(output_dir, "05_price_review_summary.csv"),
  row.names = FALSE
)

write.csv(
  as.data.frame(status_vat_counts),
  file.path(output_dir, "05_property_status_vat_counts.csv"),
  row.names = FALSE
)

write.csv(
  vat_review_summary,
  file.path(output_dir, "05_vat_review_summary.csv"),
  row.names = FALSE
)

# Recording the review decisions without changing the datasets
review_decisions <- c(
  "Records with missing Eircodes or size descriptions were retained.",
  "All fields required for the five research questions were checked for missing values.",
  "Prices outside the quartile-based thresholds were flagged for review.",
  "Flagged prices were not deleted, capped or replaced.",
  "Property-status and VAT combinations were counted without assuming they were errors.",
  "Price-analysis eligibility rules will be applied separately in Section 7."
)

writeLines(
  review_decisions,
  file.path(output_dir, "05_data_quality_decisions.txt")
)

# Checking that this review has not changed either dataset
stopifnot(
  identical(sligo_clean, clean_before_review),
  identical(sligo_all_records, all_before_review)
)

# Removing the temporary comparison copies
rm(clean_before_review, all_before_review)

cat(
  "\nSection 6 completed.",
  "\nMain-analysis records:", nrow(sligo_clean),
  "\nSensitivity-check records:", nrow(sligo_all_records),
  "\nNo records or prices changed during this review.\n"
)



# SECTION 7: Defining the analytical datasets 

# Checking that both working datasets and the output folder exist
stopifnot(
  exists("sligo_clean"),
  exists("sligo_all_records"),
  exists("output_dir")
)

# Checking the fields used to select the analytical records
selection_fields <- c(
  "not_full_market_price",
  "property_status",
  "vat_exclusive",
  "year"
)

stopifnot(
  all(complete.cases(sligo_clean[selection_fields])),
  all(complete.cases(sligo_all_records[selection_fields]))
)

# Keeping all main-analysis records for the categorical questions
# RQ4: Period and property status
# RQ5: Period and not-full-market-price status
categorical_data <- sligo_clean

# Selecting the records for the price questions
# RQ1: Prices across the three periods
# RQ3: Sale date and recorded price
price_data <- subset(
  sligo_clean,
  not_full_market_price == "No" &
    property_status == "Second-hand" &
    vat_exclusive == "No"
)

price_data <- droplevels(price_data)

# Selecting the 2025 records for the quarter comparison
# RQ2: Prices across quarters within 2025
quarter_data <- subset(
  price_data,
  year == 2025
)

quarter_data <- droplevels(quarter_data)


# Preparing the corresponding sensitivity-check subsets

# Applying the same price rules with the exact copies retained
price_data_all_records <- subset(
  sligo_all_records,
  not_full_market_price == "No" &
    property_status == "Second-hand" &
    vat_exclusive == "No"
)

price_data_all_records <- droplevels(
  price_data_all_records
)

# Selecting the corresponding 2025 records
quarter_data_all_records <- subset(
  price_data_all_records,
  year == 2025
)

quarter_data_all_records <- droplevels(
  quarter_data_all_records
)


# Checking the analytical datasets

# Checking that the selected datasets contain records
stopifnot(
  nrow(price_data) > 0,
  nrow(quarter_data) > 0,
  nrow(price_data_all_records) > 0,
  nrow(quarter_data_all_records) > 0
)

# Checking that the price rules were applied correctly
stopifnot(
  all(price_data$not_full_market_price == "No"),
  all(price_data$property_status == "Second-hand"),
  all(price_data$vat_exclusive == "No"),
  all(quarter_data$year == 2025),
  nrow(quarter_data) <= nrow(price_data),
  identical(categorical_data, sligo_clean)
)

# Checking that every comparison group contains records
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
  )
)

# Checking completeness of the variables used in the price tests
stopifnot(
  all(
    complete.cases(
      price_data[c("sale_date", "price_eur", "period")]
    )
  ),
  all(
    complete.cases(
      quarter_data[c("price_eur", "quarter")]
    )
  )
)


# Recording the subset sizes and selection rules 

subset_summary <- data.frame(
  dataset = c(
    "categorical_data",
    "price_data",
    "quarter_data",
    "sligo_all_records",
    "price_data_all_records",
    "quarter_data_all_records"
  ),
  purpose = c(
    "RQ4 and RQ5",
    "RQ1 and RQ3",
    "RQ2",
    "Categorical sensitivity checks",
    "RQ1 and RQ3 sensitivity checks",
    "RQ2 sensitivity check"
  ),
  records = c(
    nrow(categorical_data),
    nrow(price_data),
    nrow(quarter_data),
    nrow(sligo_all_records),
    nrow(price_data_all_records),
    nrow(quarter_data_all_records)
  )
)

print(subset_summary, row.names = FALSE)

# Recording the number outside the price-analysis eligibility rules
price_selection_summary <- data.frame(
  stage = c(
    "Main-analysis records",
    "Eligible price-analysis records",
    "Records outside the price-analysis rules",
    "Eligible price records within 2025"
  ),
  records = c(
    nrow(sligo_clean),
    nrow(price_data),
    nrow(sligo_clean) - nrow(price_data),
    nrow(quarter_data)
  )
)

print(price_selection_summary, row.names = FALSE)

# Recording the inclusion rules for each research question
question_rules <- data.frame(
  question = c("RQ1", "RQ2", "RQ3", "RQ4", "RQ5"),
  dataset = c(
    "price_data",
    "quarter_data",
    "price_data",
    "categorical_data",
    "categorical_data"
  ),
  inclusion_rule = c(
    "Full-market, second-hand, VAT Exclusive No; 2019-2025.",
    "Same price eligibility rules; restricted to 2025.",
    "Full-market, second-hand, VAT Exclusive No; 2019-2025.",
    "All main-analysis records after the exact-copy policy.",
    "All main-analysis records after the exact-copy policy."
  ),
  records = c(
    nrow(price_data),
    nrow(quarter_data),
    nrow(price_data),
    nrow(categorical_data),
    nrow(categorical_data)
  )
)

print(question_rules, row.names = FALSE)

# Checking the group sizes for the planned comparisons
cat("\nPrice-analysis records by period:\n")
print(table(price_data$period))

cat("\nEligible 2025 price records by quarter:\n")
print(table(quarter_data$quarter))


# Saving the subset documentation 

write.csv(
  subset_summary,
  file.path(output_dir, "06_analytical_subset_sizes.csv"),
  row.names = FALSE
)

write.csv(
  price_selection_summary,
  file.path(output_dir, "06_price_selection_summary.csv"),
  row.names = FALSE
)

write.csv(
  question_rules,
  file.path(output_dir, "06_research_question_datasets.csv"),
  row.names = FALSE
)

cat(
  "\nSection 7 completed.",
  "\nCategorical-analysis records:", nrow(categorical_data),
  "\nPrice-analysis records:", nrow(price_data),
  "\nEligible 2025 price records:", nrow(quarter_data),
  "\n"
)








# SECTION 8: Descriptive statistics and preparation charts 

# Checking that the analytical datasets are available
stopifnot(
  exists("categorical_data"),
  exists("price_data"),
  exists("quarter_data"),
  exists("project_dir"),
  exists("output_dir")
)

# Creating a folder for the charts
figures_dir <- file.path(project_dir, "figures")
dir.create(figures_dir, showWarnings = FALSE)

# Setting a consistent coral orange for the charts
chart_colour <- "#FF7F50"

# Defining the price summaries used throughout this section
summarise_prices <- function(x) {
  
  stopifnot(
    is.numeric(x),
    length(x) > 0,
    !anyNA(x),
    all(is.finite(x))
  )
  
  data.frame(
    records = length(x),
    mean_eur = mean(x),
    median_eur = median(x),
    sd_eur = sd(x),
    minimum_eur = min(x),
    q1_eur = unname(quantile(x, 0.25)),
    q3_eur = unname(quantile(x, 0.75)),
    iqr_eur = IQR(x),
    maximum_eur = max(x)
  )
}

# Defining grouped summaries for periods, years and quarters
summarise_groups <- function(data, group_variable) {
  
  grouped_prices <- split(
    data$price_eur,
    data[[group_variable]],
    drop = TRUE
  )
  
  grouped_summary <- do.call(
    rbind,
    lapply(names(grouped_prices), function(group_name) {
      data.frame(
        group = group_name,
        summarise_prices(grouped_prices[[group_name]])
      )
    })
  )
  
  rownames(grouped_summary) <- NULL
  names(grouped_summary)[1] <- group_variable
  
  return(grouped_summary)
}

# Summarising the full eligible price subset
overall_price_summary <- summarise_prices(
  price_data$price_eur
)

cat("\nOverall eligible price summary:\n")
print(overall_price_summary, row.names = FALSE, digits = 8)

# Summarising eligible prices across the three periods
period_price_summary <- summarise_groups(
  price_data,
  "period"
)

cat("\nEligible price summary by period:\n")
print(period_price_summary, row.names = FALSE, digits = 8)

# Summarising eligible prices by year
year_price_summary <- summarise_groups(
  price_data,
  "year"
)

cat("\nEligible price summary by year:\n")
print(year_price_summary, row.names = FALSE, digits = 8)

# Summarising eligible prices across quarters within 2025
quarter_price_summary <- summarise_groups(
  quarter_data,
  "quarter"
)

cat("\nEligible price summary by quarter within 2025:\n")
print(quarter_price_summary, row.names = FALSE, digits = 8)

# Checking that the grouped counts match their source datasets
stopifnot(
  overall_price_summary$records == nrow(price_data),
  sum(period_price_summary$records) == nrow(price_data),
  sum(year_price_summary$records) == nrow(price_data),
  sum(quarter_price_summary$records) == nrow(quarter_data)
)

# Saving the summaries without rounding the underlying values
write.csv(
  overall_price_summary,
  file.path(output_dir, "07_overall_price_summary.csv"),
  row.names = FALSE
)

write.csv(
  period_price_summary,
  file.path(output_dir, "07_period_price_summary.csv"),
  row.names = FALSE
)

write.csv(
  year_price_summary,
  file.path(output_dir, "07_year_price_summary.csv"),
  row.names = FALSE
)

write.csv(
  quarter_price_summary,
  file.path(output_dir, "07_2025_quarter_price_summary.csv"),
  row.names = FALSE
)

# Summarising property status and market-price flags

# Counting property status within each period
status_counts <- with(
  categorical_data,
  table(
    period = period,
    property_status = property_status
  )
)

# Calculating percentages within each period
status_percentages <- 100 * prop.table(
  status_counts,
  margin = 1
)

cat("\nProperty status counts by period:\n")
print(status_counts)

cat("\nProperty status percentages within each period:\n")
print(round(status_percentages, 2))

# Counting market-price flags within each period
market_flag_counts <- with(
  categorical_data,
  table(
    period = period,
    not_full_market_price = not_full_market_price
  )
)

# Calculating percentages within each period
market_flag_percentages <- 100 * prop.table(
  market_flag_counts,
  margin = 1
)

cat("\nNot-full-market-price counts by period:\n")
print(market_flag_counts)

cat("\nNot-full-market-price percentages within each period:\n")
print(round(market_flag_percentages, 2))

# Checking the counts and within-period percentage totals
stopifnot(
  sum(status_counts) == nrow(categorical_data),
  sum(market_flag_counts) == nrow(categorical_data),
  all(abs(rowSums(status_percentages) - 100) < 0.000001),
  all(abs(rowSums(market_flag_percentages) - 100) < 0.000001)
)

# Combining the property-status counts and percentages
status_summary <- as.data.frame(status_counts)
names(status_summary)[3] <- "records"

status_summary$percent_within_period <- as.vector(
  status_percentages
)

# Combining the market-flag counts and percentages
market_flag_summary <- as.data.frame(market_flag_counts)
names(market_flag_summary)[3] <- "records"

market_flag_summary$percent_within_period <- as.vector(
  market_flag_percentages
)

# Saving the categorical summaries
write.csv(
  status_summary,
  file.path(output_dir, "07_property_status_summary.csv"),
  row.names = FALSE
)

write.csv(
  market_flag_summary,
  file.path(output_dir, "07_market_flag_summary.csv"),
  row.names = FALSE
)

# Creating the preparation charts 

# Calculating missingness for the nine original fields
original_fields <- c(
  "sale_date",
  "address",
  "county",
  "eircode",
  "price_eur",
  "not_full_market_price",
  "vat_exclusive",
  "property_status",
  "property_size_description"
)

missing_percent <- 100 * colMeans(
  is.na(categorical_data[original_fields])
)

missing_labels <- c(
  "Sale date",
  "Address",
  "County",
  "Eircode",
  "Price",
  "Non-full-market flag",
  "VAT flag",
  "Property status",
  "Property size"
)

# Saving the missingness chart
png(
  file.path(figures_dir, "01_missing_fields.png"),
  width = 2000,
  height = 1300,
  res = 180
)

par(
  mar = c(5, 10, 4, 2),
  las = 1
)

bar_positions <- barplot(
  missing_percent,
  names.arg = missing_labels,
  horiz = TRUE,
  xlim = c(0, 115),
  col = chart_colour,
  border = NA,
  xlab = "Records with missing values (%)",
  main = "Missing fields in the main dataset"
)

text(
  x = missing_percent + 2,
  y = bar_positions,
  labels = paste0(round(missing_percent, 2), "%"),
  pos = 4,
  cex = 0.8
)

mtext(
  paste("After exact-copy policy; n =", nrow(categorical_data)),
  side = 3,
  line = 0.3,
  cex = 0.8
)

dev.off()


# Counting all main-analysis records by year
annual_record_counts <- data.frame(
  year = 2019:2025,
  records = as.integer(
    table(
      factor(categorical_data$year, levels = 2019:2025)
    )
  )
)

# Saving the annual record-count chart
png(
  file.path(figures_dir, "02_annual_record_counts.png"),
  width = 2000,
  height = 1300,
  res = 180
)

par(
  mar = c(5, 5, 4, 2),
  las = 1
)

bar_positions <- barplot(
  annual_record_counts$records,
  names.arg = annual_record_counts$year,
  ylim = c(0, max(annual_record_counts$records) * 1.15),
  col = chart_colour,
  border = NA,
  xlab = "Sale year",
  ylab = "Recorded transactions",
  main = "Annual record counts, 2019-2025"
)

text(
  x = bar_positions,
  y = annual_record_counts$records,
  labels = annual_record_counts$records,
  pos = 3,
  cex = 0.8
)

mtext(
  "All main-analysis records after the exact-copy policy",
  side = 3,
  line = 0.3,
  cex = 0.8
)

dev.off()


# Saving the annual median-price chart
png(
  file.path(figures_dir, "03_annual_median_prices.png"),
  width = 2000,
  height = 1300,
  res = 180
)

par(
  mar = c(5, 5, 4, 2),
  las = 1
)

plot(
  as.integer(year_price_summary$year),
  year_price_summary$median_eur / 1000,
  type = "o",
  pch = 16,
  lwd = 2,
  col = chart_colour,
  xaxt = "n",
  xlab = "Sale year",
  ylab = "Median recorded price (EUR thousands)",
  main = "Annual median recorded prices, 2019-2025"
)

axis(
  side = 1,
  at = 2019:2025
)

mtext(
  "Full-market, second-hand records; VAT Exclusive = No",
  side = 3,
  line = 0.3,
  cex = 0.8
)

dev.off()


# Checking that all three chart files were created
preparation_charts <- file.path(
  figures_dir,
  c(
    "01_missing_fields.png",
    "02_annual_record_counts.png",
    "03_annual_median_prices.png"
  )
)

stopifnot(
  all(file.exists(preparation_charts)),
  all(file.info(preparation_charts)$size > 0)
)

cat(
  "\nSection 8 completed.",
  "\nPrice-analysis records summarised:", nrow(price_data),
  "\n2025 quarter records summarised:", nrow(quarter_data),
  "\nCategorical records summarised:", nrow(categorical_data),
  "\nPreparation charts saved:", length(preparation_charts),
  "\n"
)

print(preparation_charts)






# SECTION 9: Validating and exporting the prepared data 

# Checking that all analytical datasets are available
stopifnot(
  exists("sligo_clean"),
  exists("price_data"),
  exists("quarter_data"),
  exists("sligo_all_records"),
  exists("price_data_all_records"),
  exists("quarter_data_all_records"),
  exists("project_dir"),
  exists("output_dir"),
  exists("figures_dir")
)

# Creating a data folder for the exported datasets
data_dir <- file.path(project_dir, "data")
dir.create(data_dir, showWarnings = FALSE)

# Defining the fields required for the five research questions
analysis_fields <- c(
  "sale_date",
  "price_eur",
  "not_full_market_price",
  "vat_exclusive",
  "property_status",
  "year",
  "month",
  "quarter",
  "period"
)


# Checking the main-analysis dataset 

# Checking the required variables and types in the main dataset
stopifnot(
  nrow(sligo_clean) == 5957,
  inherits(sligo_clean$sale_date, "Date"),
  is.numeric(sligo_clean$price_eur),
  is.factor(sligo_clean$county),
  is.factor(sligo_clean$not_full_market_price),
  is.factor(sligo_clean$vat_exclusive),
  is.factor(sligo_clean$property_status),
  is.integer(sligo_clean$year),
  is.integer(sligo_clean$month),
  is.factor(sligo_clean$quarter),
  is.factor(sligo_clean$period),
  all(is.finite(sligo_clean$price_eur)),
  all(sligo_clean$price_eur > 0),
  all(sligo_clean$year %in% 2019:2025),
  all(sligo_clean$month %in% 1:12),
  all(levels(sligo_clean$quarter) == c("Q1", "Q2", "Q3", "Q4")),
  all(
    levels(sligo_clean$period) ==
      c("2019", "2020-2021", "2022-2025")
  )
)

# Checking the fields required for the five research questions
# Eircodes and property-size descriptions are retained with missing values
stopifnot(
  all(analysis_fields %in% names(sligo_clean)),
  all(complete.cases(sligo_clean[analysis_fields]))
)


# Checking the price-analysis dataset 

# Checking the price-analysis dataset and its required fields
stopifnot(
  nrow(price_data) == 5070,
  inherits(price_data$sale_date, "Date"),
  is.numeric(price_data$price_eur),
  is.factor(price_data$period),
  all(is.finite(price_data$price_eur)),
  all(price_data$price_eur > 0),
  all(price_data$not_full_market_price == "No"),
  all(price_data$property_status == "Second-hand"),
  all(price_data$vat_exclusive == "No"),
  all(price_data$year %in% 2019:2025)
)

# Checking the fields required for the price questions
# Eircodes and property-size descriptions are retained with missing values
stopifnot(
  all(analysis_fields %in% names(price_data)),
  all(complete.cases(price_data[analysis_fields]))
)

# Checking that every price-analysis period contains records
stopifnot(
  all(
    table(
      factor(
        price_data$period,
        levels = c("2019", "2020-2021", "2022-2025")
      )
    ) > 0
  )
)


# Checking the 2025 quarter dataset 

# Checking the 2025 quarter dataset and its required fields
stopifnot(
  nrow(quarter_data) == 675,
  is.numeric(quarter_data$price_eur),
  is.factor(quarter_data$quarter),
  all(is.finite(quarter_data$price_eur)),
  all(quarter_data$price_eur > 0),
  all(quarter_data$year == 2025),
  all(quarter_data$not_full_market_price == "No"),
  all(quarter_data$property_status == "Second-hand"),
  all(quarter_data$vat_exclusive == "No")
)

# Checking the fields required for the quarter question
# Eircodes and property-size descriptions are retained with missing values
stopifnot(
  all(analysis_fields %in% names(quarter_data)),
  all(complete.cases(quarter_data[analysis_fields]))
)

# Checking that every quarter contains records
stopifnot(
  all(
    table(
      factor(
        quarter_data$quarter,
        levels = c("Q1", "Q2", "Q3", "Q4")
      )
    ) > 0
  )
)


# Checking the sensitivity-check datasets 

# Checking the datasets containing all formatted records
stopifnot(
  nrow(sligo_all_records) == 5964,
  nrow(price_data_all_records) == 5074,
  nrow(quarter_data_all_records) == 676,
  inherits(sligo_all_records$sale_date, "Date"),
  is.numeric(sligo_all_records$price_eur),
  all(
    is.finite(sligo_all_records$price_eur),
    is.finite(price_data_all_records$price_eur),
    is.finite(quarter_data_all_records$price_eur)
  ),
  all(
    sligo_all_records$price_eur > 0,
    price_data_all_records$price_eur > 0,
    quarter_data_all_records$price_eur > 0
  )
)

# Checking the fields required for the sensitivity-check analysis
# Eircodes and property-size descriptions are retained with missing values
stopifnot(
  all(
    complete.cases(
      sligo_all_records[analysis_fields]
    )
  ),
  all(
    complete.cases(
      price_data_all_records[analysis_fields]
    )
  ),
  all(
    complete.cases(
      quarter_data_all_records[analysis_fields]
    )
  )
)

# Checking that the sensitivity-check datasets are larger
stopifnot(
  nrow(sligo_all_records) > nrow(sligo_clean),
  nrow(price_data_all_records) > nrow(price_data),
  nrow(quarter_data_all_records) > nrow(quarter_data)
)


# Exporting the analytical datasets 

# Exporting the main categorical-analysis dataset
saveRDS(
  sligo_clean,
  file.path(data_dir, "sligo_clean.rds")
)

# Exporting the eligible price-analysis dataset
saveRDS(
  price_data,
  file.path(data_dir, "price_data.rds")
)

# Exporting the 2025 quarter dataset
saveRDS(
  quarter_data,
  file.path(data_dir, "quarter_data.rds")
)

# Exporting the sensitivity-check datasets
saveRDS(
  sligo_all_records,
  file.path(data_dir, "sligo_all_records.rds")
)

saveRDS(
  price_data_all_records,
  file.path(data_dir, "price_data_all_records.rds")
)

saveRDS(
  quarter_data_all_records,
  file.path(data_dir, "quarter_data_all_records.rds")
)


# Exporting readable CSV copies 

# Selecting the analytical fields for the CSV exports
# Excluding addresses and Eircodes from the analytical exports
write.csv(
  sligo_clean[analysis_fields],
  file.path(data_dir, "sligo_clean.csv"),
  row.names = FALSE
)

write.csv(
  price_data[analysis_fields],
  file.path(data_dir, "price_data.csv"),
  row.names = FALSE
)

write.csv(
  quarter_data[analysis_fields],
  file.path(data_dir, "quarter_data.csv"),
  row.names = FALSE
)

write.csv(
  sligo_all_records[analysis_fields],
  file.path(data_dir, "sligo_all_records.csv"),
  row.names = FALSE
)

write.csv(
  price_data_all_records[analysis_fields],
  file.path(data_dir, "price_data_all_records.csv"),
  row.names = FALSE
)

write.csv(
  quarter_data_all_records[analysis_fields],
  file.path(data_dir, "quarter_data_all_records.csv"),
  row.names = FALSE
)


# Checking the exported RDS files 

# Reading each RDS file back into a temporary object
check_clean <- readRDS(
  file.path(data_dir, "sligo_clean.rds")
)

check_prices <- readRDS(
  file.path(data_dir, "price_data.rds")
)

check_quarters <- readRDS(
  file.path(data_dir, "quarter_data.rds")
)

check_all_records <- readRDS(
  file.path(data_dir, "sligo_all_records.rds")
)

check_prices_all_records <- readRDS(
  file.path(data_dir, "price_data_all_records.rds")
)

check_quarters_all_records <- readRDS(
  file.path(data_dir, "quarter_data_all_records.rds")
)

# Checking that the restored objects match the working datasets
stopifnot(
  identical(check_clean, sligo_clean),
  identical(check_prices, price_data),
  identical(check_quarters, quarter_data),
  identical(check_all_records, sligo_all_records),
  identical(check_prices_all_records, price_data_all_records),
  identical(check_quarters_all_records, quarter_data_all_records)
)

# Removing the temporary restored objects
rm(
  check_clean,
  check_prices,
  check_quarters,
  check_all_records,
  check_prices_all_records,
  check_quarters_all_records
)


# Recording the final preparation audit 

# Creating the final preparation audit
final_preparation_audit <- data.frame(
  stage = c(
    "Original master records",
    "Additional exact copies set aside",
    "Main-analysis records after duplicate policy",
    "Categorical-analysis records",
    "Eligible price-analysis records",
    "Eligible 2025 quarter records",
    "All formatted records for sensitivity checks",
    "Eligible price records with copies retained",
    "Eligible 2025 quarter records with copies retained",
    "Missing Eircodes retained",
    "Missing property-size descriptions retained",
    "Flagged prices retained",
    "New records with VAT Exclusive No retained",
    "Preparation charts saved"
  ),
  records = c(
    nrow(raw),
    nrow(raw) - nrow(sligo_clean),
    nrow(sligo_clean),
    nrow(categorical_data),
    nrow(price_data),
    nrow(quarter_data),
    nrow(sligo_all_records),
    nrow(price_data_all_records),
    nrow(quarter_data_all_records),
    sum(is.na(sligo_clean$eircode)),
    sum(is.na(sligo_clean$property_size_description)),
    sum(sligo_clean$price_eur > 495500),
    sum(
      sligo_clean$property_status == "New" &
        sligo_clean$vat_exclusive == "No"
    ),
    length(list.files(figures_dir, pattern = "\\.png$"))
  )
)

print(final_preparation_audit, row.names = FALSE)

# Saving the final preparation audit
write.csv(
  final_preparation_audit,
  file.path(output_dir, "08_final_preparation_audit.csv"),
  row.names = FALSE
)


# Recording the final preparation decisions 

final_preparation_notes <- c(
  "The original master dataset contained 5,964 records and nine variables.",
  "Dates were converted to Date format and prices were converted to numeric values.",
  "County, property status, VAT flag and market-price flag were converted to factors.",
  "Seven additional exact copies were set aside for the main analysis.",
  "All 5,964 formatted records were retained for a later sensitivity check.",
  "Year, month, quarter and comparison-period variables were created.",
  "Missing Eircodes and property-size descriptions were retained.",
  "All fields required for the five research questions were complete.",
  "201 prices above the quartile-based review threshold were retained.",
  "Fourteen new-property records with VAT Exclusive No were retained.",
  "The price-analysis subset applied full-market, second-hand and VAT Exclusive No rules.",
  "The 2025 quarter subset applied the same rules and was restricted to 2025.",
  "The categorical dataset retained all 5,957 main-analysis records.",
  "RDS files preserve the prepared variable types for the testing script.",
  "CSV exports omit addresses and Eircodes but are not a guarantee of full anonymisation.",
  "The original master CSV was not changed during preparation."
)

# Saving the final preparation decisions
writeLines(
  final_preparation_notes,
  file.path(output_dir, "08_final_preparation_notes.txt")
)


# Recording the R session information 

# Saving the R version, operating system and package information
writeLines(
  capture.output(sessionInfo()),
  file.path(output_dir, "08_preparation_session_info.txt")
)


# Checking the final outputs

# Listing the exported RDS files
exported_rds_files <- list.files(
  data_dir,
  pattern = "\\.rds$",
  full.names = TRUE
)

# Listing the exported CSV files
exported_csv_files <- list.files(
  data_dir,
  pattern = "\\.csv$",
  full.names = TRUE
)

# Checking that all expected exports exist
stopifnot(
  length(exported_rds_files) == 6,
  length(exported_csv_files) == 6,
  all(file.info(exported_rds_files)$size > 0),
  all(file.info(exported_csv_files)$size > 0)
)

# Displaying the completed preparation outputs
cat("\nPreparation completed successfully.\n")
cat("Main-analysis records:", nrow(sligo_clean), "\n")
cat("Eligible price-analysis records:", nrow(price_data), "\n")
cat("Eligible 2025 quarter records:", nrow(quarter_data), "\n")
cat("Sensitivity-check records:", nrow(sligo_all_records), "\n")
cat("RDS files exported:", length(exported_rds_files), "\n")
cat("CSV files exported:", length(exported_csv_files), "\n")
cat("Preparation charts saved:", length(list.files(figures_dir, pattern = "\\.png$")), "\n")

print(exported_rds_files)
print(exported_csv_files)