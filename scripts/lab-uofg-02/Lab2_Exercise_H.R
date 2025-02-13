################################################################################
#   R Code for Lab 2 Exercise H (MGT4018/MGT4090)
#                       
#   Dr Bernd Wurth (bernd.wurth@glasgow.ac.uk)
#
#   Last modified: 10 February 2025
################################################################################

#===============================================================================
# Step H1: Setup
#===============================================================================


#===============================================================================
# Step H2: Load the Dataset
#===============================================================================

#-------------------------------------------------------------------------------
# Load the Dataset
#-------------------------------------------------------------------------------

# Load the CSV file stored in the "data" folder
staff_survey_data <- read.csv("data/lab2-staff-survey.csv")

#-------------------------------------------------------------------------------
# Check the Dataset
#-------------------------------------------------------------------------------

# View the first few rows using head()
head(staff_survey_data)

# Examine the structure
str(staff_survey_data)

# Get basic summary statistics
summary(staff_survey_data)

#-------------------------------------------------------------------------------
# Assign Labels for Categorical Variables
#-------------------------------------------------------------------------------

# First, let's create vectors for our common level labels since they're reused often
extent_levels <- c("not at all", "to a slight extent", "to a moderate extent", 
                   "to a great extent", "to a very great extent")

importance_levels <- c("not important", "slightly important", "moderately important",
                       "very important", "extremely important")

# Create age groups labels
staff_survey_data$age <- factor(staff_survey_data$age,
                                levels = 1:5,
                                labels = c("under 20", "21 to 30", "31 to 40",
                                           "41 to 50", "over 50"))

# Employment status
staff_survey_data$employstatus <- factor(staff_survey_data$employstatus,
                                         levels = 1:2,
                                         labels = c("permanent", "casual"))

# Now let's handle the repeated patterns for Q1a through Q10a
# We'll use a loop to avoid repetitive code
for(i in 1:10) {
  # Convert "extent" questions (Q1a through Q10a)
  staff_survey_data[[paste0("Q", i, "a")]] <- factor(
    staff_survey_data[[paste0("Q", i, "a")]],
    levels = 1:5,
    labels = extent_levels
  )
  
  # Convert "importance" questions (Q1imp through Q10imp)
  staff_survey_data[[paste0("Q", i, "imp")]] <- factor(
    staff_survey_data[[paste0("Q", i, "imp")]],
    levels = 1:5,
    labels = importance_levels
  )
}

# Finally, convert the recommend variable
staff_survey_data$recommend <- factor(staff_survey_data$recommend,
                                      levels = c(0, 1),
                                      labels = c("no", "yes"))

# Let's add a verification step to check our work
# This will print the levels of each variable to confirm they're correctly labeled
verify_factors <- function(data) {
  for(col in names(data)) {
    if(is.factor(data[[col]])) {
      cat("\nLevels for", col, ":\n")
      print(levels(data[[col]]))
    }
  }
}

# Run the verification
verify_factors(staff_survey_data)

# Verify changes by printing a summary
summary(staff_survey_data)


#===============================================================================
# Step H3: Frequency Tables
#===============================================================================

#-------------------------------------------------------------------------------
# Base R Solution
#-------------------------------------------------------------------------------

# Frequency Table and Distribution for `city` ----------------------------------

# Create frequency table for city
city_table <- table(staff_survey_data$city)
city_props <- prop.table(city_table) * 100

# Combine counts and percentages
city_summary <- cbind(
  Frequency = as.vector(city_table),
  Percentage = as.vector(city_props)
)
rownames(city_summary) <- names(city_table)

# Display the results with formatting
print("Distribution of Staff by City:")
print(round(city_summary, 1))

# Frequency Table and Distribution for `employment status` ---------------------

# Create frequency table for employment status
status_table <- table(staff_survey_data$employstatus)
status_props <- prop.table(status_table) * 100

status_summary <- cbind(
  Frequency = as.vector(status_table),
  Percentage = as.vector(status_props)
)
rownames(status_summary) <- names(status_table)

print("\nDistribution of Employment Status:")
print(round(status_summary, 1))

# Frequency Table and Distribution for `service` -------------------------------

# First, let's create reasonable bins for years of service
service_breaks <- seq(0, max(staff_survey_data$service, na.rm = TRUE) + 5, by = 5)
service_cats <- cut(staff_survey_data$service, 
                    breaks = service_breaks,
                    include.lowest = TRUE)

service_table <- table(service_cats)
service_props <- prop.table(service_table) * 100

service_summary <- cbind(
  Frequency = as.vector(service_table),
  Percentage = as.vector(service_props)
)
rownames(service_summary) <- names(service_table)

print("\nDistribution of Years of Service:")
print(round(service_summary, 1))

#-------------------------------------------------------------------------------
# Solution with R Packages
#-------------------------------------------------------------------------------

# Install and Load Packages ----------------------------------------------------

# Install packages if needed
install.packages(c("tidyverse", "knitr", "kableExtra"))

# Load required packages
library(tidyverse)  # For data manipulation and visualization
library(knitr)      # For nice tables
library(kableExtra) # For enhanced table formatting

# Create a Function for Nicely Formatted Frequency Tables ----------------------

create_freq_table <- function(data, variable) {
  data %>%
    count({{variable}}) %>%
    mutate(Percentage = n/sum(n) * 100) %>%
    kable(digits = 1,
          col.names = c("Category", "Count", "Percentage"),
          caption = paste("Distribution of", deparse(substitute(variable)))) %>%
    kable_styling(bootstrap_options = c("striped", "hover"))
}

# Create Frequency Tables for Key Variables ---=--------------------------------

# Create tables for each demographic variable
create_freq_table(staff_survey_data, city)
create_freq_table(staff_survey_data, service)
create_freq_table(staff_survey_data, employstatus)


#===============================================================================
# Step Step H4: Hisogram
#===============================================================================

#-------------------------------------------------------------------------------
# Base R Solution
#-------------------------------------------------------------------------------

# Create histogram
hist(staff_survey_data$service,
     main = "Distribution of Years of Service",
     xlab = "Years of Service",
     ylab = "Frequency",
     breaks = 20,
     col = "lightblue",
     border = "white")

# Add a box plot for outlier detection
boxplot(staff_survey_data$service,
        horizontal = TRUE,
        main = "Box Plot of Years of Service",
        xlab = "Years of Service",
        col = "lightblue")

# Reset plotting parameters
par(mfrow = c(1, 1))

# Calculate summary statistics
service_summary_stats <- summary(staff_survey_data$service)
service_sd <- sd(staff_survey_data$service, na.rm = TRUE)

# Print summary statistics
print("\nSummary Statistics for Years of Service:")
print(service_summary_stats)
print(paste("Standard Deviation:", round(service_sd, 2)))

# Identify potential outliers using the 1.5 * IQR rule
Q1 <- quantile(staff_survey_data$service, 0.25, na.rm = TRUE)
Q3 <- quantile(staff_survey_data$service, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
outlier_threshold_upper <- Q3 + 1.5 * IQR
outlier_threshold_lower <- Q1 - 1.5 * IQR

outliers <- staff_survey_data$service[staff_survey_data$service > outlier_threshold_upper |
                                        staff_survey_data$service < outlier_threshold_lower]

if(length(outliers) > 0) {
  print("\nPotential outliers identified:")
  print(sort(outliers))
}

#-------------------------------------------------------------------------------
# Solution with R Packages
#-------------------------------------------------------------------------------

# Install and Load Packages ----------------------------------------------------

# Install packages if needed
install.packages(c("tidyverse", "knitr", "kableExtra"))

# Load required packages
library(tidyverse)  # For data manipulation and visualization
library(knitr)      # For nice tables
library(kableExtra) # For enhanced table formatting

# Create Histogram with Density Curve ------------------------------------------

ggplot(staff_survey_data, aes(x = service)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 30,
                 fill = "lightblue",
                 color = "black") +
  geom_density(color = "red") +
  theme_minimal() +
  labs(x = "Years of Service",
       y = "Density",
       title = "Distribution of Years of Service",
       subtitle = "With density curve overlay") +
  # Add boxplot for outlier visualization
  geom_boxplot(aes(y = -0.02), width = 0.1)

# Summary Statistics and Outliers for `service` --------------------------------

# Calculate summary statistics for service
service_summary <- staff_survey_data %>%
  summarize(
    Mean = mean(service, na.rm = TRUE),
    Median = median(service, na.rm = TRUE),
    SD = sd(service, na.rm = TRUE),
    Q1 = quantile(service, 0.25, na.rm = TRUE),
    Q3 = quantile(service, 0.75, na.rm = TRUE),
    IQR = IQR(service, na.rm = TRUE),
    Min = min(service, na.rm = TRUE),
    Max = max(service, na.rm = TRUE)
  )

# Print summary statistics
kable(service_summary, digits = 1,
      caption = "Summary Statistics for Years of Service") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Identify potential outliers
outliers <- staff_survey_data %>%
  filter(service > (service_summary$Q3 + 1.5 * service_summary$IQR) |
           service < (service_summary$Q1 - 1.5 * service_summary$IQR))


#===============================================================================
# Step H5: Cross-Tabulation
#===============================================================================

#-------------------------------------------------------------------------------
# Base R Solution
#-------------------------------------------------------------------------------

# Create cross-tabulation
age_employ_table <- table(staff_survey_data$agerecode, 
                          staff_survey_data$employstatus)

# Calculate row percentages
age_employ_props <- prop.table(age_employ_table, margin = 1) * 100

# Print the results
print("Counts by Age Group and Employment Status:")
print(age_employ_table)
print("\nPercentages within Age Groups:")
print(round(age_employ_props, 1))

# Create a visual representation using base R
# Set up the plotting area
barplot(t(age_employ_props),
        beside = TRUE,
        col = c("lightblue", "lightgreen"),
        main = "Employment Status by Age Group",
        xlab = "Age Group",
        ylab = "Percentage",
        legend.text = colnames(age_employ_props))

# Perform chi-square test of independence
chi_sq_test <- chisq.test(age_employ_table)
print("\nChi-square test of independence:")
print(chi_sq_test)

#-------------------------------------------------------------------------------
# Solution with R Packages
#-------------------------------------------------------------------------------

# Install and Load Packages ----------------------------------------------------

# Install packages if needed
install.packages(c("tidyverse", "knitr", "kableExtra"))

# Load required packages
library(tidyverse)  # For data manipulation and visualization
library(knitr)      # For nice tables
library(kableExtra) # For enhanced table formatting

# Cross-Tabulation and Visualization -------------------------------------------

# Create cross-tabulation
age_employ_table <- table(staff_survey_data$agerecode, 
                          staff_survey_data$employstatus)

# Convert to percentages and format nicely
age_employ_props <- prop.table(age_employ_table, margin = 1) * 100

# Combine counts and percentages in a nice table
age_employ_combined <- cbind(
  as.data.frame.matrix(age_employ_table),
  as.data.frame.matrix(age_employ_props)
) %>%
  setNames(c("Permanent (n)", "Casual (n)", 
             "Permanent (%)", "Casual (%)"))

kable(age_employ_combined, 
      digits = 1,
      caption = "Age Groups by Employment Status") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Create a visualization
ggplot(staff_survey_data, 
       aes(x = agerecode, fill = employstatus)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(x = "Age Group",
       y = "Proportion",
       fill = "Employment Status",
       title = "Employment Status Distribution by Age Group") +
  coord_flip()

#===============================================================================
# Step H6: Total Staff Satisfaction
#===============================================================================

#-------------------------------------------------------------------------------
# Base R Solution
#-------------------------------------------------------------------------------

# How is Total Staff Satisfaction (`totsatis`) calculated? ---------------------

# Identify satisfaction-related variables (Q*a)
satisfaction_vars <- grep("Q.*a$", names(staff_survey_data), value = TRUE)

# Calculate correlations with total satisfaction
correlations <- sapply(staff_survey_data[satisfaction_vars], 
                       function(x) cor(x, staff_survey_data$totsatis,
                                       use = "complete.obs"))

# Print correlations
print("Correlations with Total Satisfaction:")
print(round(correlations, 3))

# Create scatterplot matrix of selected variables
pairs(staff_survey_data[c(satisfaction_vars[1:5], "totsatis")],
      main = "Relationships between Satisfaction Measures")

# Differences in TSS (`totsatis`) between Permanent and Casual Staff -----------

# Calculate descriptive statistics by group
tapply(staff_survey_data$totsatis, 
       staff_survey_data$employstatus,
       function(x) c(n = length(x),
                     mean = mean(x, na.rm = TRUE),
                     sd = sd(x, na.rm = TRUE)))

# Create box plot for visual comparison
boxplot(totsatis ~ employstatus,
        data = staff_survey_data,
        main = "Total Satisfaction by Employment Status",
        xlab = "Employment Status",
        ylab = "Total Satisfaction Score",
        col = "lightblue")

# Add individual points for better visualization
stripchart(totsatis ~ employstatus,
           data = staff_survey_data,
           vertical = TRUE,
           method = "jitter",
           add = TRUE,
           pch = 20,
           col = "darkgray")

# Perform t-test
satisfaction_ttest <- t.test(totsatis ~ employstatus, 
                             data = staff_survey_data)

# Print t-test results
print("\nt-test Results:")
print(satisfaction_ttest)

# Calculate effect size (Cohen's d)
group1 <- staff_survey_data$totsatis[staff_survey_data$employstatus == "permanent"]
group2 <- staff_survey_data$totsatis[staff_survey_data$employstatus == "casual"]

cohens_d <- function(x, y) {
  nx <- length(x)
  ny <- length(y)
  pooled_sd <- sqrt(((nx-1)*var(x) + (ny-1)*var(y))/(nx+ny-2))
  abs(mean(x) - mean(y))/pooled_sd
}

effect_size <- cohens_d(group1, group2)
print(paste("\nEffect size (Cohen's d):", round(effect_size, 3)))

#-------------------------------------------------------------------------------
# Solution with R Packages
#-------------------------------------------------------------------------------

# Install and Load Packages ----------------------------------------------------

# Install packages if needed
install.packages(c("tidyverse", "knitr", "kableExtra"))

# Load required packages
library(tidyverse)  # For data manipulation and visualization
library(knitr)      # For nice tables
library(kableExtra) # For enhanced table formatting

# How is Total Staff Satisfaction (`totsatis`) calculated? ---------------------

# Examine the structure of satisfaction-related variables
satisfaction_vars <- names(staff_survey_data)[grep("Q.*a$", names(staff_survey_data))]

# Look at correlations between these variables and totsatis
satisfaction_correlations <- staff_survey_data %>%
  select(all_of(c(satisfaction_vars, "totsatis"))) %>%
  cor(use = "complete.obs")

# Print correlations with total satisfaction
kable(satisfaction_correlations["totsatis", ],
      digits = 3,
      caption = "Correlations with Total Satisfaction") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Differences in TSS (`totsatis`) between Permanent and Casual Staff -----------

# First, let's look at descriptive statistics
satisfaction_by_status <- staff_survey_data %>%
  group_by(employstatus) %>%
  summarise(
    n = n(),
    mean = mean(totsatis, na.rm = TRUE),
    sd = sd(totsatis, na.rm = TRUE),
    se = sd/sqrt(n)
  )

# Print descriptive statistics
kable(satisfaction_by_status,
      digits = 2,
      caption = "Satisfaction Scores by Employment Status") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Create visualization
ggplot(staff_survey_data, aes(x = employstatus, y = totsatis)) +
  geom_boxplot(fill = "lightblue") +
  geom_jitter(width = 0.2, alpha = 0.2) +
  theme_minimal() +
  labs(x = "Employment Status",
       y = "Total Satisfaction Score",
       title = "Distribution of Satisfaction Scores by Employment Status")

# Perform t-test
satisfaction_ttest <- t.test(totsatis ~ employstatus, 
                             data = staff_survey_data)

# Calculate effect size (Cohen's d)
cohens_d <- function(x, y) {
  nx <- length(x)
  ny <- length(y)
  pooled_sd <- sqrt(((nx-1)*var(x) + (ny-1)*var(y))/(nx+ny-2))
  abs(mean(x) - mean(y))/pooled_sd
}

effect_size <- with(staff_survey_data,
                    cohens_d(totsatis[employstatus == "permanent"],
                             totsatis[employstatus == "casual"]))

# Print t-test results
print(satisfaction_ttest)
cat("\nEffect size (Cohen's d):", round(effect_size, 3))
