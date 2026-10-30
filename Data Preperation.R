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
