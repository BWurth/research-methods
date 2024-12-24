################################################################################
#   R Code for Lab 1 Exercise A (MGT4018/MGT4090)
#                       
#   Dr Bernd Wurth (bernd.wurth@glasgow.ac.uk)
#
#   Last modified: 22 December 2024
################################################################################

#===============================================================================
# Step A1 (Only needed for for Option 2)
#===============================================================================

#-------------------------------------------------------------------------------
# Option 2: Set Working Directory Manually
#-------------------------------------------------------------------------------

# Set working directory
setwd("path/to/your/folder") # CHANGE THIS!

# You can check whether the working directory is set to the folder that you want 
#  to by using the following code:
getwd()


#===============================================================================
# Step A2: Creating or Loading the Dataset
#===============================================================================

#-------------------------------------------------------------------------------
# Option 1: Create your Dataset
#-------------------------------------------------------------------------------

# Create the Dataset -----------------------------------------------------------

# Create vectors for our data
id <- 1:30

# Create age bands with labels
age_bands <- factor(
  c(1,3,2,4,2,5,5,2,3,1,4,1,3,2,4,2,5,5,2,3,1,4,2,4,2,5,5,2,3,1),
  levels = 1:5,
  labels = c("<21", "21-30", "31-40", "41-50", ">50")
)

# Create gender categories with labels
gender <- factor(
  c(0,1,1,0,0,0,1,1,1,0,9,0,2,1,0,0,1,1,0,0,1,0,0,0,1,1,1,0,9,9),
  levels = c(0,1,2,9),
  labels = c("Male", "Female", "Other", "Prefer not to say")
)

# Combine into a data frame
survey_data <- data.frame(
  ID = id,
  AgeBand = age_bands,
  Gender = gender
)

# Check the Dataset ------------------------------------------------------------

# View the first few rows using head()
head(survey_data)

# Examine the structure
str(survey_data)

# Get basic summary statistics
summary(survey_data)

# Save the Dataset -------------------------------------------------------------

# Save as R data file
saveRDS(survey_data, "survey_data.rds")

# Save as .csv
write_csv(survey_data, "survey_data.csv")

# By default, the data is exported with row names. Now to export data without 
#  row names we simply have to pass row.names=FALSE as an argument in the 
#  write.csv() function.
write_csv(survey_data, "survey_data.csv", row.names=FALSE)

#-------------------------------------------------------------------------------
# Option 2: 
#-------------------------------------------------------------------------------

# Load the Dataset -------------------------------------------------------------

# Load the CSV file stored in the "data" folder
survey_data <- read.csv("data/lab1-exercise-a.csv")

# Check the Dataset ------------------------------------------------------------

# View the first few rows using head()
head(survey_data)

# Examine the structure
str(survey_data)

# Get basic summary statistics
summary(survey_data)


#===============================================================================
# Step A3: Modifying your Dataset
#===============================================================================

# Create a copy of your dataset
survey_data_copy <- survey_data

#-------------------------------------------------------------------------------
# Editing Specific Values
#-------------------------------------------------------------------------------

# Edit the Gender of the participant with ID 15
survey_data_copy[survey_data_copy$ID == 15, "Gender"] <- "Female"
print(survey_data_copy[survey_data_copy$ID == 15, ])

#-------------------------------------------------------------------------------
# Adding a Column
#-------------------------------------------------------------------------------

# Add a new column indicating if the participant is above 30 based on AgeBand
survey_data_copy$Above30 <- survey_data_copy$AgeBand %in% c("31-40", "41-50", ">50")
print(head(survey_data_copy))

#-------------------------------------------------------------------------------
# Adding a Row
#-------------------------------------------------------------------------------

# Add a new participant to the data frame
new_row <- data.frame(ID = 31, AgeBand = "<21", Gender = "Other", Above30 = FALSE)
survey_data_copy <- rbind(survey_data_copy, new_row)
print(tail(survey_data_copy))

#-------------------------------------------------------------------------------
# Removing a Column
#-------------------------------------------------------------------------------

# Remove the "Above30" column
survey_data_copy$Above30 <- NULL
print(head(survey_data_copy))

#-------------------------------------------------------------------------------
# Removing a Row
#-------------------------------------------------------------------------------

# Remove the row where ID is 10
survey_data_copy <- survey_data_copy[survey_data_copy$ID != 10, ]
print(head(survey_data_copy))

#-------------------------------------------------------------------------------
# Renaming Columns
#-------------------------------------------------------------------------------

# Rename columns
names(survey_data_copy) <- c("ParticipantID", "AgeGroup", "GenderIdentity")
print(head(survey_data_copy))

#-------------------------------------------------------------------------------
# Combining Data Frames
#-------------------------------------------------------------------------------

# Create another data frame with additional information
additional_data <- data.frame(
  ParticipantID = c(1, 2, 3, 4, 5),
  CompletedSurvey = c(TRUE, TRUE, FALSE, TRUE, FALSE)
)

# Merge data frames by the "ParticipantID" column
survey_data_copy <- merge(survey_data_copy, additional_data, by = "ParticipantID", all.x = TRUE)
print(head(survey_data_copy))


#===============================================================================
# Step A4: Frequency Tables (Summary Statistics)
#===============================================================================

#-------------------------------------------------------------------------------
# Creating a Basic Frequency Table
#-------------------------------------------------------------------------------

# Frequency table for AgeBand
age_band_freq <- table(survey_data$AgeBand)

# Frequency table for Gender
gender_freq <- table(survey_data$Gender)

# Display the results
print("Frequency table for AgeBand:")
print(age_band_freq)
print("Frequency table for Gender:")
print(gender_freq)

#-------------------------------------------------------------------------------
# Adding Proportions
#-------------------------------------------------------------------------------

# Proportions for AgeBand
age_band_prop <- prop.table(age_band_freq)
print(age_band_prop)

# Proportions for AgeBand as percentages
age_band_percent <- round(prop.table(age_band_freq) * 100, 2)
print(age_band_percent)

# Combine frequencies and percentages
age_summary <- cbind(
  Frequency = age_band_freq,
  Percentage = age_band_percent
)

# Display the results
print("AgeBand Distribution:")
print(age_summary)

#-------------------------------------------------------------------------------
# Summary Statistics for Frequency Tables
#-------------------------------------------------------------------------------

# Find the most frequent AgeBand
most_frequent_age_band <- names(age_band_freq[which.max(age_band_freq)])
print(paste("Most frequent age band:", most_frequent_age_band))

# Identify rare categories (frequency <= 2)
rare_categories <- names(age_band_freq[age_band_freq <= 2])
print(paste("Rare categories:", paste(rare_categories, collapse = ", ")))

#-------------------------------------------------------------------------------
# Handling Missing or Unexpected Values
#-------------------------------------------------------------------------------

# Check for missing or unusual values in Gender
gender_freq <- table(survey_data$Gender, useNA = "ifany")
print(gender_freq)


#===============================================================================
# Step A5: Cross-Tabulation
#===============================================================================

#-------------------------------------------------------------------------------
# Creating a Cross Tabulation
#-------------------------------------------------------------------------------

# Create a cross-tabulation of AgeBand and Gender
age_gender_table <- table(survey_data$AgeBand, survey_data$Gender)
print(age_gender_table)

#-------------------------------------------------------------------------------
# Adding Proportions to Cross Tabulations
#-------------------------------------------------------------------------------

# Proportions for the Entire Table ---------------------------------------------

# Proportions for the entire table
age_gender_prop <- prop.table(age_gender_table)
print(round(age_gender_prop * 100, 2))

# Row-Wise Proportions ---------------------------------------------------------

# Row-wise proportions
row_prop <- prop.table(age_gender_table, margin = 1)
print(round(row_prop * 100, 2))

# Column-Wise Proportions ------------------------------------------------------

# Column-wise proportions
col_prop <- prop.table(age_gender_table, margin = 2)
print(round(col_prop * 100, 2))

#-------------------------------------------------------------------------------
# Marginal Totals
#-------------------------------------------------------------------------------

# Add row and column totals to the table
age_gender_with_totals <- addmargins(age_gender_table)
print(age_gender_with_totals)

#-------------------------------------------------------------------------------
# Customizing Cross Tabulation Output
#-------------------------------------------------------------------------------

# Customize row and column names
dimnames(age_gender_table) <- list(
  AgeGroup = levels(survey_data$AgeBand),
  GenderCategory = levels(survey_data$Gender)
)
print(age_gender_table)

#-------------------------------------------------------------------------------
# Detecting Relationships in Cross Tabulations
#-------------------------------------------------------------------------------

# Perform a Chi-Square Test of Independence
chi_test <- chisq.test(age_gender_table)
print(chi_test)


#===============================================================================
# Step A6: Saving Frequency Tables and Cross-Tabulations
#===============================================================================

#-------------------------------------------------------------------------------
# Saving Frequency Tables
#-------------------------------------------------------------------------------

# Saving as a CSV File ---------------------------------------------------------

# Create a frequency table for AgeBand
age_band_freq <- table(survey_data$AgeBand)

# Convert the frequency table to a data frame
age_band_df <- as.data.frame(age_band_freq)

# Save the data frame to a CSV file
write.csv(age_band_df, "output/tables/age_band_frequency.csv", row.names = FALSE)

# Exporting to Plain Text ------------------------------------------------------

# Export the frequency table to a text file
capture.output(age_band_freq, file = "output/tables/age_band_frequency.txt")

#-------------------------------------------------------------------------------
# Saving Cross-Tabulations
#-------------------------------------------------------------------------------

# Saving as a CSV File ---------------------------------------------------------

# Create a cross-tabulation of AgeBand and Gender
age_gender_table <- table(survey_data$AgeBand, survey_data$Gender)

# Convert the cross-tabulation to a data frame
age_gender_df <- as.data.frame(age_gender_table)

# Save the cross-tabulation to a CSV file
write.csv(age_gender_df, "output/tables/age_gender_cross_tabulation.csv", row.names = FALSE)

# Exporting to Plain Text ------------------------------------------------------

# Export the cross-tabulation to a text file
capture.output(age_gender_table, file = "output/tables/age_gender_cross_tabulation.txt")

# Adding Totals Before Export --------------------------------------------------

# Add row and column totals
age_gender_with_totals <- addmargins(age_gender_table)

# Save the table with totals as plain text
capture.output(age_gender_with_totals, file = "output/tables/age_gender_with_totals.txt")

#-------------------------------------------------------------------------------
# Saving Proportions
#-------------------------------------------------------------------------------

# Frequency Counts -------------------------------------------------------------

# Calculate proportions for AgeBand
age_band_prop <- prop.table(age_band_freq)

# Convert to a data frame and save as CSV
age_band_prop_df <- as.data.frame(age_band_prop)
write.csv(age_band_prop_df, "output/tables/age_band_proportions.csv", row.names = FALSE)

# Cross-Tabulations ------------------------------------------------------------
  
# Calculate proportions for the cross-tabulation
age_gender_prop <- prop.table(age_gender_table)

# Convert to a data frame and save as CSV
age_gender_prop_df <- as.data.frame(age_gender_prop)
write.csv(age_gender_prop_df, "output/tables/age_gender_proportions.csv", row.names = FALSE)


#===============================================================================
# Alternatives Using R Packages
#===============================================================================

#-------------------------------------------------------------------------------
# Beautiful, customizable tables with export options with `gt`
#-------------------------------------------------------------------------------

# Install package
install.packages("gt")

# Load janitor package
library(gt)

# Create a cross-tabulation
age_gender_table <- table(survey_data$AgeBand, survey_data$Gender)

# Convert the table to a data frame
age_gender_df <- as.data.frame(age_gender_table)

# Create a gt table
gt_table <- gt(data = age_gender_df) %>%
  tab_header(
    title = "Cross-Tabulation of Age Band and Gender",
    subtitle = "Frequency Distribution"
  ) %>%
  cols_label(
    Var1 = "Age Band",
    Var2 = "Gender",
    Freq = "Count"
  ) %>%
  fmt_number(
    columns = vars(Freq),
    decimals = 0
  ) %>%
  tab_source_note(
    source_note = "Data Source: Survey Data"
  )

# Save the table as an HTML file
gtsave(gt_table, filename = "output/tables/age_gender_cross_tabulation.html")

#-------------------------------------------------------------------------------
# Cleaning and Tabulating Data with `janitor`
#-------------------------------------------------------------------------------

# Creating and Saving Frequency Tables -----------------------------------------

# Install package
install.packages("janitor")

# Load janitor package
library(janitor)

# Frequency table for AgeBand
age_band_freq <- survey_data %>%
  tabyl(AgeBand)

# Save as CSV
write.csv(age_band_freq, "output/tables/age_band_frequency.csv", row.names = FALSE)

# Creating and Saving Cross-Tabulation -----------------------------------------

# Make sure that the package is installed and loaded

# Cross-tabulation of AgeBand and Gender
age_gender_table <- survey_data %>%
  tabyl(AgeBand, Gender)

# Save as CSV
write.csv(age_gender_table, "output/tables/age_gender_cross_tabulation.csv", row.names = FALSE)

#-------------------------------------------------------------------------------
# Publication-Ready Tables with `gtsummary`
#-------------------------------------------------------------------------------

# Install package
install.packages("gtsummary")

# Load package
library(gtsummary)

# Create a cross-tabulation table
age_gender_table <- survey_data %>%
  tbl_cross(
    row = AgeBand,
    col = Gender
  )

# Save as a CSV or Word document
as_gt(age_gender_table) %>%
  gtsave("output/tables/age_gender_cross_tabulation.html")

#-------------------------------------------------------------------------------
# Customizable Table Formatting with `flextable`
#-------------------------------------------------------------------------------

# Install package
install.packages("flextable")

# Load package
library(flextable)

# Convert a cross-tabulation to a flextable
age_gender_table <- table(survey_data$AgeBand, survey_data$Gender) %>%
  as.data.frame()

ft <- flextable(age_gender_table)

# Save as Word document
save_as_docx(ft, path = "output/tables/age_gender_cross_tabulation.docx")
save_as_pptx(ft, path = "output/tables/age_gender_cross_tabulation.pptx")
save_as_image(ft, path = "output/tables/age_gender_cross_tabulation.png")
