################################################################################
#   R Code for Lab 1 Exercises B and C (MGT4018/MGT4090)
#                       
#   Dr Bernd Wurth (bernd.wurth@glasgow.ac.uk)
#
#   Last modified: 22 December 2024
################################################################################

#===============================================================================
# Step B1 (Not needed, we work in the same Project or WD)
#===============================================================================


#===============================================================================
# Step B2: Creating or Loading the Dataset
#===============================================================================

#-------------------------------------------------------------------------------
# Load the Dataset
#-------------------------------------------------------------------------------

# Load the CSV file stored in the "data" folder
survey_data_full <- read.csv("data/lab1-survey.csv")

#-------------------------------------------------------------------------------
# Check the Dataset
#-------------------------------------------------------------------------------

# View the first few rows using head()
head(survey_data_full)

# Examine the structure
str(survey_data_full)

# Get basic summary statistics
summary(survey_data_full)

#-------------------------------------------------------------------------------
# Assign Labels to Demographic Variables
#-------------------------------------------------------------------------------

# Assign labels for "sex"
survey_data_full$sex <- factor(
  survey_data_full$sex,
  levels = c(1, 2),
  labels = c("Male", "Female")
)

# Assign labels for "marital"
survey_data_full$marital <- factor(
  survey_data_full$marital,
  levels = c(1, 2, 3, 4, 5, 6, 7, 8),
  labels = c(
    "Single", "Steady relationship", "Living with partner",
    "Married first time", "Remarried", "Separated",
    "Divorced", "Widowed"
  )
)

# Assign labels for "child"
survey_data_full$child <- factor(
  survey_data_full$child,
  levels = c(1, 2),
  labels = c("Yes", "No")
)

# Assign labels for "educ"
survey_data_full$educ <- factor(
  survey_data_full$educ,
  levels = c(1, 2, 3, 4, 5, 6),
  labels = c(
    "Primary", "Some secondary", "Completed high school",
    "Some additional training", "Completed undergraduate",
    "Postgraduate completed"
  )
)

# Assign labels for "source"
survey_data_full$source <- factor(
  survey_data_full$source,
  levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
  labels = c(
    "Work", "Spouse or partner", "Relationships", "Children",
    "Family", "Health/illness", "Life in general",
    "Money/finances", "Lack of time, too much to do"
  )
)

# Assign labels for "smoke"
survey_data_full$smoke <- factor(
  survey_data_full$smoke,
  levels = c(1, 2),
  labels = c("Yes", "No")
)

# Verify changes by printing a summary
summary(survey_data_full)


#===============================================================================
# Step B3: Exploring your Data
#===============================================================================

#-------------------------------------------------------------------------------
# Gender (`sex`)
#-------------------------------------------------------------------------------

# Frequency table for Gender
gender_freq <- table(survey_data_full$sex)
print(gender_freq)

# Proportions and percentages
gender_prop <- prop.table(gender_freq)
gender_percent <- round(gender_prop * 100, 2)
print(gender_percent)

#-------------------------------------------------------------------------------
# Marital Status (`marital`)
#-------------------------------------------------------------------------------

# Frequency table for Marital Status
marital_freq <- table(survey_data_full$marital)
print(marital_freq)

# Proportions and percentages
marital_prop <- prop.table(marital_freq)
marital_percent <- round(marital_prop * 100, 2)
print(marital_percent)

#-------------------------------------------------------------------------------
# Parental Status (`child`)
#-------------------------------------------------------------------------------

# Frequency table for Parental Status
child_freq <- table(survey_data_full$child)
print(child_freq)

# Proportions and percentages
child_prop <- prop.table(child_freq)
child_percent <- round(child_prop * 100, 2)
print(child_percent)

#-------------------------------------------------------------------------------
# Education Level (`educ`)
#-------------------------------------------------------------------------------

# Frequency table for Education Level
educ_freq <- table(survey_data_full$educ)
print(educ_freq)

# Proportions and percentages
educ_prop <- prop.table(educ_freq)
educ_percent <- round(educ_prop * 100, 2)
print(educ_percent)

#-------------------------------------------------------------------------------
# Primary Source of Stress (`source`)
#-------------------------------------------------------------------------------

# Frequency table for Primary Source of Stress
source_freq <- table(survey_data_full$source)
print(source_freq)

# Proportions and percentages
source_prop <- prop.table(source_freq)
source_percent <- round(source_prop * 100, 2)
print(source_percent)

#-------------------------------------------------------------------------------
# Smoking Status (`smoke`)
#-------------------------------------------------------------------------------

# Frequency table for Smoking Status
smoke_freq <- table(survey_data_full$smoke)
print(smoke_freq)

# Proportions and percentages
smoke_prop <- prop.table(smoke_freq)
smoke_percent <- round(smoke_prop * 100, 2)
print(smoke_percent)

#-------------------------------------------------------------------------------
# Summarising Frequencies
#-------------------------------------------------------------------------------

# Combine frequency tables into a list for easy viewing
frequency_summary <- list(
  Gender = list(Count = gender_freq, Percent = gender_percent),
  MaritalStatus = list(Count = marital_freq, Percent = marital_percent),
  ParentalStatus = list(Count = child_freq, Percent = child_percent),
  EducationLevel = list(Count = educ_freq, Percent = educ_percent),
  StressSource = list(Count = source_freq, Percent = source_percent),
  SmokingStatus = list(Count = smoke_freq, Percent = smoke_percent)
)

# Print the summary
print(frequency_summary)


#===============================================================================
# Step B4: Exploring Participants' Age
#===============================================================================

# Creating an AgeBand variable
survey_data_full$AgeBand <- cut(
  survey_data_full$age, 
  breaks = c(-Inf, 20, 30, 40, 50, 60, Inf), 
  labels = c("<21", "21-30", "31-40", "41-50", "51-60", ">60"), 
  right = TRUE
)

# Verify the new AgeBand variable

# Frequency table for AgeBand
AgeBand_freq <- table(survey_data_full$AgeBand)
print(AgeBand_freq)

# Percentages for AgeBand
AgeBand_prop <- prop.table(AgeBand_freq)
AgeBand_percent <- round(AgeBand_prop * 100, 2)
print(AgeBand_percent)


#===============================================================================
# Step B5: Cross-Tabulations
#===============================================================================

#-------------------------------------------------------------------------------
# Relationship between `marital` and `child`
#-------------------------------------------------------------------------------

# Create a cross-tabulation of Marital Status by Parental Status
marital_child_table <- table(survey_data_full$marital, survey_data_full$child)

# Print the cross-tabulation
print(marital_child_table)

#-------------------------------------------------------------------------------
# Adding Proportions to Cross Tabulations
#-------------------------------------------------------------------------------

# Proportions for the Entire Table ---------------------------------------------

# Proportions for the entire table
marital_child_prop <- prop.table(marital_child_table)
print(round(marital_child_prop * 100, 2))

# Row-Wise Proportions ---------------------------------------------------------

# Row-wise proportions (within marital status groups)
marital_child_row_prop <- prop.table(marital_child_table, margin = 1)
print(round(marital_child_row_prop * 100, 2))

# Column-Wise Proportions ------------------------------------------------------

# Column-wise proportions (within parental status groups)
marital_child_col_prop <- prop.table(marital_child_table, margin = 2)
print(round(marital_child_col_prop * 100, 2))

#-------------------------------------------------------------------------------
# Marginal Totals
#-------------------------------------------------------------------------------

# Add marginal totals to the table
marital_child_with_totals <- addmargins(marital_child_table)
print(marital_child_with_totals)


#===============================================================================
# Step C1: Setup
#===============================================================================

#-------------------------------------------------------------------------------
# Install and Load the `ggplot2` Package
#-------------------------------------------------------------------------------

# Install ggplot2
install.packages("ggplot2")

# Load ggplot2
library(ggplot2)

#-------------------------------------------------------------------------------
# Exploring Help and Resources (optional)
#-------------------------------------------------------------------------------

# Access the ggplot2 documentation
help(package = "ggplot2")


#===============================================================================
# Step C2: Histograms
#===============================================================================

#-------------------------------------------------------------------------------
# Creating a Histogram for `tpstress` 
#-------------------------------------------------------------------------------


# Create a histogram for tpstress
histogram_tpstress <- ggplot(data = survey_data_full, aes(x = tpstress)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Perceived Stress Levels",
    x = "Perceived Stress Score (tpstress)",
    y = "Frequency"
  ) +
  theme_minimal()

# Show plot
histogram_tpstress

# Save plot
ggsave("output/figures/histogram_tpstress.pdf", width = 8, height = 5)

#-------------------------------------------------------------------------------
# Compare Groups Using Histograms 
#-------------------------------------------------------------------------------

# Side-by-Side -----------------------------------------------------------------

# Panelled histograms for tpstress by sex
histogram_tpstress_gender <- ggplot(data = survey_data_full, aes(x = tpstress)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  facet_wrap(~ sex, ncol = 2) +  # Creates separate panels for each group
  labs(
    title = "Comparison of Perceived Stress Levels by Gender",
    x = "Perceived Stress Score (tpstress)",
    y = "Frequency"
  ) +
  theme_minimal()

# Show plot
histogram_tpstress_gender

# Save plot
ggsave("output/figures/histogram_tpstress_gender.pdf", width = 8, height = 5)

# Stacked ----------------------------------------------------------------------

# Stacked histograms for tpstress by sex
histogram_tpstress_gender_stack <- ggplot(data = survey_data_full, aes(x = tpstress)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  facet_wrap(~ sex, ncol = 1) +  # Stacks panels vertically
  labs(
    title = "Comparison of Perceived Stress Levels by Gender (Stacked)",
    x = "Perceived Stress Score (tpstress)",
    y = "Frequency"
  ) +
  theme_minimal()

# Show plot
histogram_tpstress_gender_stack

# Save plot
ggsave("output/figures/histogram_tpstress_gender_stack.pdf", width = 8, height = 5)

#===============================================================================
# Step C3: Bar Charts
#===============================================================================

#-------------------------------------------------------------------------------
# Creating a Simple Bar Chart 
#-------------------------------------------------------------------------------

# Simple bar chart for Gender
bar_gender <- ggplot(data = survey_data_full, aes(x = sex)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(
    title = "Gender Distribution",
    x = "Gender",
    y = "Count"
  ) +
  theme_minimal()

# Show plot
bar_gender

# Save plot
ggsave("output/figures/bar_gender.pdf", width = 8, height = 5)

#-------------------------------------------------------------------------------
# Creating a Clustered Bar Chart 
#-------------------------------------------------------------------------------

# Base R -----------------------------------------------------------------------

# Calculate group means using aggregate()
group_means <- aggregate(
  tpstress ~ agegp3 + sex,
  data = survey_data_full,
  FUN = mean,
  na.rm = TRUE
)

# Display the group means
print(group_means)

# Calculate group standard deviations
group_sd <- aggregate(
  tpstress ~ agegp3 + sex,
  data = survey_data_full,
  FUN = sd,
  na.rm = TRUE
)

# Add a column for group sizes
group_counts <- aggregate(
  tpstress ~ agegp3 + sex,
  data = survey_data_full,
  FUN = length
)

# Merge the results into a single data frame
group_stats <- merge(group_means, group_sd, by = c("agegp3", "sex"))
group_stats <- merge(group_stats, group_counts, by = c("agegp3", "sex"))

# Rename columns for clarity
colnames(group_stats) <- c("agegp3", "sex", "mean_tpstress", "sd_tpstress", "n")

# Calculate standard errors
group_stats$se_tpstress <- group_stats$sd_tpstress / sqrt(group_stats$n)

# Display the final data frame
print(group_stats)

# Clustered bar chart using the prepared data
bar_gender_age <- ggplot(data = group_stats, aes(x = agegp3, y = mean_tpstress, fill = sex)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black") +
  geom_errorbar(
    aes(ymin = mean_tpstress - se_tpstress, ymax = mean_tpstress + se_tpstress),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  labs(
    title = "Mean Perceived Stress by Age Group and Gender",
    x = "Age Group",
    y = "Mean Perceived Stress Score (tpstress)",
    fill = "Gender"
  ) +
  theme_minimal()

# Show plot
bar_gender_age

# Save plot
ggsave("output/figures/bar_gender_age.pdf", width = 8, height = 5)

# Using `dplyr` Package --------------------------------------------------------

# Install dplyr
install.packages("dplyr")

# Load library
library(dplyr)

# Summarize the data
survey_summary <- survey_data_full %>%
  group_by(agegp3, sex) %>%
  summarise(
    mean_tpstress = mean(tpstress, na.rm = TRUE),
    se_tpstress = sd(tpstress, na.rm = TRUE) / sqrt(n())
  )

# Display the summarized data
print(survey_summary)

# Create a clustered bar chart with ggplot2
bar_gender_age2 <- ggplot(data = survey_summary, aes(x = agegp3, y = mean_tpstress, fill = sex)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black") +
  geom_errorbar(aes(ymin = mean_tpstress - se_tpstress, ymax = mean_tpstress + se_tpstress),
                position = position_dodge(width = 0.8), width = 0.2) +
  labs(
    title = "Mean Perceived Stress by Age Group and Gender",
    x = "Age Group",
    y = "Mean Perceived Stress Score (tpstress)",
    fill = "Gender"
  ) +
  theme_minimal()

# Show plot
bar_gender_age2

# Save plot
ggsave("output/figures/bar_gender_age2.pdf", width = 8, height = 5)


#===============================================================================
# Step C4: Line Graphs
#===============================================================================

#-------------------------------------------------------------------------------
# Creating a Simple Line Graph
#-------------------------------------------------------------------------------

# Calculate the mean tpstress for each age group
simple_line_data <- aggregate(
  tpstress ~ agegp5,
  data = survey_data_full,
  FUN = mean,
  na.rm = TRUE
)

# Create the line graph
line_stress_age <- ggplot(data = simple_line_data, aes(x = agegp5, y = tpstress, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "Mean Perceived Stress by Age Group",
    x = "Age Group",
    y = "Mean Perceived Stress Score"
  ) +
  theme_minimal()

# Show plot
line_stress_age

# Save plot
ggsave("output/figures/line_stress_age.pdf", width = 8, height = 5)

#-------------------------------------------------------------------------------
# Creating a Multi-Line Graph 
#-------------------------------------------------------------------------------

# Calculate the mean tpstress for each combination of age group and gender
multi_line_data <- aggregate(
  tpstress ~ agegp5 + sex,
  data = survey_data_full,
  FUN = mean,
  na.rm = TRUE
)

# Create the multi-line graph
line_stress_age_gender <- ggplot(data = multi_line_data, aes(x = agegp5, y = tpstress, color = sex, group = sex)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Mean Perceived Stress by Age Group and Gender",
    x = "Age Group",
    y = "Mean Perceived Stress Score",
    color = "Gender"
  ) +
  theme_minimal()

# Show plot
line_stress_age_gender

# Save plot
ggsave("output/figures/line_stress_age_gender.pdf", width = 8, height = 5)

#===============================================================================
# Step C4: Scatterplots
#===============================================================================

#-------------------------------------------------------------------------------
# Simple Scatterplot 
#-------------------------------------------------------------------------------

# Simple scatterplot
scatter_stress_control <- ggplot(data = survey_data_full, aes(x = tpcoiss, y = tpstress)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "Scatterplot of Perceived Stress vs Coping Strategies",
    x = "Coping Strategies (tpcoiss)",
    y = "Perceived Stress (tpstress)"
  ) +
  theme_minimal()

# Show plot
scatter_stress_control

# Save plot
ggsave("output/figures/scatter_stress_control.pdf", width = 8, height = 5)

#-------------------------------------------------------------------------------
# Scatterplot with Categorical Grouping 
#-------------------------------------------------------------------------------

# Scatterplot with grouping by sex
scatter_stress_control_gender <- ggplot(data = survey_data_full, aes(x = tpcoiss, y = tpstress, color = sex)) +
  geom_point(size = 2) +
  labs(
    title = "Scatterplot of Perceived Stress vs Coping Strategies by Gender",
    x = "Coping Strategies (tpcoiss)",
    y = "Perceived Stress (tpstress)",
    color = "Gender"
  ) +
  theme_minimal()

# Show plot
scatter_stress_control_gender

# Save plot
ggsave("output/figures/scatter_stress_control_gender.pdf", width = 8, height = 5)

#-------------------------------------------------------------------------------
# Scatterplot with Point Labels 
#-------------------------------------------------------------------------------

# Scatterplot with point labels
scatter_stress_control_gender_id <- ggplot(data = survey_data_full, aes(x = tpcoiss, y = tpstress, color = sex)) +
  geom_point(size = 2) +
  geom_text(aes(label = id), hjust = 0, vjust = -0.5, size = 3) +
  labs(
    title = "Scatterplot of Perceived Stress vs Coping Strategies with Point Labels",
    x = "Coping Strategies (tpcoiss)",
    y = "Perceived Stress (tpstress)",
    color = "Gender"
  ) +
  theme_minimal()


# Show plot
scatter_stress_control_gender_id

# Save plot
ggsave("output/figures/scatter_stress_control_gender_id.pdf", width = 8, height = 5)