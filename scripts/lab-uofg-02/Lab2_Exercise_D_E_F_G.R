################################################################################
#   R Code for Lab 2 Exercises D, E, F and G (MGT4018/MGT4090)
#                       
#   Dr Bernd Wurth (bernd.wurth@glasgow.ac.uk)
#
#   Last modified: 10 February 2025
################################################################################

#===============================================================================
# Step D1: Setup
#===============================================================================


#===============================================================================
# Step D2: Load the Dataset
#===============================================================================

#-------------------------------------------------------------------------------
# Load the Dataset
#-------------------------------------------------------------------------------

# Load the CSV file stored in the "data" folder
survey_data_full <- read.csv("data/lab2-survey.csv")

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
# Step D3: Correlation
#===============================================================================

#-------------------------------------------------------------------------------
# Visual Inspection: Scatterplots
#-------------------------------------------------------------------------------

# Install ggplot2 (NOTE: not needed if you completed the first lab)
install.packages("ggplot2")

# Load ggplot2
library(ggplot2)


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

#-------------------------------------------------------------------------------
# Single Correlation
#-------------------------------------------------------------------------------

# Handle Missing Values in `cor()` With `use` ----------------------------------

# Calculate correlation between tpcoiss and tpstress
cor(survey_data_full$tpcoiss, survey_data_full$tpstress, use = "complete.obs")

# Specify the method (default is Pearson)
cor(survey_data_full$tpcoiss, survey_data_full$tpstress, use = "complete.obs", 
    method = "pearson")

# Alternative: Remove or Replace Missing Values Beforehand ---------------------

# Filter the dataset to remove missing values before calculation
clean_data <- na.omit(survey_data_full[, c("tpcoiss", "tpstress")])
cor(clean_data$tpcoiss, clean_data$tpstress)

# Or you can replace missing values (e.g., using the mean).
modified_data <- survey_data_full
modified_data$tpcoiss[is.na(modified_data$tpcoiss)] <- mean(modified_data$tpcoiss, na.rm = TRUE)
modified_data$tpstress[is.na(modified_data$tpstress)] <- mean(modified_data$tpstress, na.rm = TRUE)

cor(modified_data$tpcoiss, modified_data$tpstress)

#-------------------------------------------------------------------------------
# Correlation Matrix
#-------------------------------------------------------------------------------

# Create Data Frame ------------------------------------------------------------

# Create a smaller data frame with only continuous variables
survey_data_small <- survey_data_full[, c("tpcoiss", "tpstress", "toptim", 
                                          "tposaff", "tnegaff", "tlifesat", 
                                          "tslfest", "tmarlow")]

# Check the structure of the new data frame
str(survey_data_small)

# Or see the first few rows
head(survey_data_small)

# Correlation Matrix -----------------------------------------------------------

# Calculate correlation matrix
correlation_matrix <- cor(survey_data_small, use = "pairwise.complete.obs")

# Round to 3 decimal places for clarity
round(correlation_matrix, 3)

#-------------------------------------------------------------------------------
# Statistical Testing with `cor.test()`
#-------------------------------------------------------------------------------

# Perform correlation test
correlation_test <- cor.test(survey_data_full$tpcoiss, 
                             survey_data_full$tpstress, 
                             use = "complete.obs")

# View complete results
print(correlation_test)


#===============================================================================
# Step D4: Linear Regression
#===============================================================================

#-------------------------------------------------------------------------------
# Creating Our Linear Regression Model
#-------------------------------------------------------------------------------

# Visualizing the Relationship Using `ggplot2` ---------------------------------

# Create Scatterplot
ggplot(survey_data_full, aes(x = tpcoiss, y = tpstress)) +
  geom_point(color = "darkblue", alpha = 0.7) +  # Scatterplot points
  geom_smooth(method = "lm", color = "red", se = FALSE, linewidth = 1.5) +  # Regression line
  labs(
    title = "Relationship between Stress and Control",
    x = "Sense of Control (tpcoiss)",
    y = "Perceived Stress (tpstress)"
  ) +
  theme_minimal()

# Create our Regression Model --------------------------------------------------

# Create the linear regression model
stress_model <- lm(tpstress ~ tpcoiss, data = survey_data_full)

# View the complete summary
summary(stress_model)

#-------------------------------------------------------------------------------
# Understanding the Results
#-------------------------------------------------------------------------------

# The Correlation Coefficient (R) ----------------------------------------------

# Calculate r from our model
r <- sign(coef(stress_model)[2]) * sqrt(summary(stress_model)$r.squared)
cat("Correlation coefficient (r):", round(r, 3))

# The ANOVA Table --------------------------------------------------------------

# Display the ANOVA table
anova(stress_model)

# The Regression Equation ------------------------------------------------------

# Display coefficients
coef(stress_model)

# Extract coefficients
intercept <- coef(stress_model)[1]
slope <- coef(stress_model)[2]

# Let's fill in the actual values:
cat("Regression equation:\n")
cat("Stress =", round(intercept, 3), "+", round(slope, 3), "× Control")

# R-squared (R²) ---------------------------------------------------------------

# Extract R-squared
r_squared <- summary(stress_model)$r.squared
cat("R-squared:", round(r_squared, 3))

#-------------------------------------------------------------------------------
# Making Predictions
#-------------------------------------------------------------------------------

# Create some example control values
new_control <- data.frame(tpcoiss = c(20, 30, 40))

# Make predictions
predictions <- predict(stress_model, newdata = new_control)

# Display predictions
cbind(Control = new_control, Predicted_Stress = round(predictions, 2))

#-------------------------------------------------------------------------------
# Checking Model Assumptions
#-------------------------------------------------------------------------------

# Create diagnostic plots
par(mfrow = c(2, 2))
plot(stress_model)

#-------------------------------------------------------------------------------
# Linear Regression Analysis with R Packages
#-------------------------------------------------------------------------------

# Install and Load Packages ----------------------------------------------------

# Install packages if needed
install.packages(c("tidyverse", "broom", "performance", "see", "ggpubr", 
                   "sjPlot", "sjmisc", "sjlabelled"))

# Load required packages
library(tidyverse)    # For data manipulation and visualization
library(broom)        # For tidying statistical objects
library(performance)  # For model performance metrics
library(see)          # For model visualization
library(ggpubr)       # For publication-ready plots
library(sjPlot)       # For model visualization and tables
library(sjmisc)       # For model visualization and tables
library(sjlabelled)   # For model visualization and tables

# Data Preparation -------------------------------------------------------------

# Create a focused dataset for analysis
analysis_data <- survey_data_full %>%
  select(tpstress, tpcoiss) %>%
  drop_na()  # Remove any missing values

# Quick summary of our variables
summary(analysis_data)

# Visual Exploration with `ggplot2` --------------------------------------------

# Create an enhanced scatter plot
ggplot(analysis_data, aes(x = tpcoiss, y = tpstress)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Relationship between Stress and Control",
    subtitle = "With linear regression line and 95% confidence interval",
    x = "Sense of Control (tpcoiss)",
    y = "Perceived Stress Level (tpstress)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Creating the Regression Model ------------------------------------------------

# Create the model
model <- lm(tpstress ~ tpcoiss, data = analysis_data)

# Get a tidy summary using broom
tidy_model <- tidy(model, conf.int = TRUE)
glance_model <- glance(model)

# Display tidy results
tidy_model

# Model Diagnostics with `performance` -----------------------------------------

# Check model assumptions
check_model(model)

# Model performance metrics
model_performance(model)

# Creating Publication-Ready Tables with `sjPlot`, `sjmisc`, `sjlabelled` ------

# Create regression table
tab_model(model,
          title = "Linear Regression Results",
          dv.labels = "Perceived Stress",
          pred.labels = c("(Intercept)", "Sense of Control"))

# Export tables in html or Word format
tab_model(model, file = "output/tables/regression_table.html")
tab_model(model, file = "output/tables/regression_table.doc")

# Visualizing Effects with `ggeffects` -----------------------------------------

# Plot predicted values
plot_model(model, type = "pred") +
  labs(
    title = "Predicted Stress Levels by Control",
    x = "Sense of Control",
    y = "Predicted Stress Level"
  )

# Enhanced Regression Diagnostics ----------------------------------------------

# Get augmented data (includes residuals, etc.)
model_data <- augment(model)

# Create diagnostic plots
p1 <- ggplot(model_data, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted",
       x = "Fitted values",
       y = "Residuals") +
  theme_minimal()

p2 <- ggplot(model_data, aes(sample = .std.resid)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Normal Q-Q Plot") +
  theme_minimal()

# Arrange plots side by side
diagnostic_plots <- ggarrange(p1, p2, ncol = 2)

# Show combined plot
diagnostic_plots

# Save combined plot as a pdf (change file ending for other formats)
ggsave("output/figures/diagnostic_plots.pdf", plot = diagnostic_plots, width = 8, height = 5)

# Making Predictions -----------------------------------------------------------

# Create new control values
new_control <- tibble(tpcoiss = c(20, 30, 40))

# Make predictions and bind results
predictions <- new_control %>%
  mutate(Predicted_Stress = predict(stress_model, newdata = .) %>% round(2))

# Display predictions
print(predictions)

# Interactive Model Summary ----------------------------------------------------

# Create model summary
summary_stats <- tibble(
  Statistic = c("R-squared", "Adjusted R-squared", "F-statistic", "p-value"),
  Value = c(
    glance_model$r.squared,
    glance_model$adj.r.squared,
    glance_model$statistic,
    glance_model$p.value
  )
) %>%
  mutate(Value = round(Value, 3))

# Display as a formatted table
knitr::kable(summary_stats, 
             caption = "Model Summary Statistics")


#===============================================================================
# Step E1: Understanding the Independent Samples t-test
#===============================================================================

# First, let's see what our data looks like
head(survey_data_full[c("sex", "tslfest", "tpcoiss")])

# Create boxplot with jittered points of self-esteem by gender
ggplot(survey_data_full, aes(x = factor(sex), y = tslfest)) +
  geom_boxplot(aes(fill = factor(sex)), outlier.shape = NA) +  # Boxplot without default outliers
  geom_jitter(width = 0.2, alpha = 0.5, color = "darkgray") +  # Individual points (jittered)
  scale_fill_manual(values = c("lightblue", "lightpink")) +
  labs(
    title = "Distribution of Self-esteem by Gender",
    x = "Gender",
    y = "Total Self-esteem Score"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend since colors only represent gender

# Create violin plot with a small boxplot inside
ggplot(survey_data_full, aes(x = factor(sex), y = tslfest, fill = factor(sex))) +
  geom_violin(trim = FALSE, alpha = 0.5) +  # Violin plot with full density curve
  geom_boxplot(width = 0.2, fill = "white", outlier.shape = NA) +  # Add a small boxplot inside
  scale_fill_manual(values = c("lightblue", "lightpink")) + 
  labs(
    title = "Distribution of Self-esteem by Gender",
    x = "Gender",
    y = "Total Self-esteem Score"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend since colors only represent gender

#===============================================================================
# Step E2: Performing the T-Test (Self-esteem by Gender)
#===============================================================================

#-------------------------------------------------------------------------------
# Base R Solution
#-------------------------------------------------------------------------------

# First, let's check basic descriptive statistics
tapply(survey_data_full$tslfest, 
       survey_data_full$sex, 
       function(x) c(mean = mean(x, na.rm = TRUE), 
                     sd = sd(x, na.rm = TRUE),
                     n = sum(!is.na(x))))

# Perform Levene's test for equality of variances
var_test <- var.test(tslfest ~ sex, data = survey_data_full)
print("Levene's test results:")
print(var_test)

# Perform t-test based on Levene's test result
t_test_result <- t.test(tslfest ~ sex, 
                        data = survey_data_full,
                        var.equal = var_test$p.value > 0.05)  # true if p > 0.05
print("\nt-test results:")
print(t_test_result)

# Effect Size ------------------------------------------------------------------

# Calculate Cohen's d
group1 <- survey_data_full$tslfest[survey_data_full$sex == "Male"]
group2 <- survey_data_full$tslfest[survey_data_full$sex == "Female"]

calc_cohens_d <- function(x, y) {
  # Remove NA values
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  
  nx <- length(x)
  ny <- length(y)
  
  # Ensure non-empty groups
  if (nx < 2 || ny < 2) {
    return(NA)  # Return NA if a group has fewer than 2 values
  }
  
  pooled_sd <- sqrt(((nx-1)*var(x) + (ny-1)*var(y))/(nx+ny-2))
  abs(mean(x) - mean(y))/pooled_sd
  
  return(abs(mean(x) - mean(y)) / pooled_sd)
}

effect_size <- calc_cohens_d(group1, group2)
cat("Cohen's d effect size:", round(effect_size, 3))

#-------------------------------------------------------------------------------
# Solution with R Packages
#-------------------------------------------------------------------------------

# Install and Load Packages ----------------------------------------------------

# Install packages if needed
install.packages(c("tidyverse", "car", "rstatix", "ggpubr", "effectsize"))

# Load required packages
library(tidyverse)  # For data manipulation and visualization
library(car)        # For Levene's test and additional diagnostics
library(rstatix)    # For statistical analysis
library(ggpubr)    # For publication-ready plots
library(effectsize) # For effect size calculations

# Data Preparation -------------------------------------------------------------

# Create a small dataframe with relevant variables only
analysis_data <- survey_data_full %>% select(sex, tslfest, tpcoiss)

# Display the structure of our prepared data
str(analysis_data)

# Visual Exploration -----------------------------------------------------------

# Create boxplot with jittered points of self-esteem by gender
ggplot(analysis_data, aes(x = factor(sex), y = tslfest)) +
  geom_boxplot(aes(fill = factor(sex)), outlier.shape = NA) +  # Boxplot without default outliers
  geom_jitter(width = 0.2, alpha = 0.5, color = "darkgray") +  # Individual points (jittered)
  scale_fill_manual(values = c("lightblue", "lightpink")) +
  labs(
    title = "Distribution of Self-esteem by Gender",
    x = "Gender",
    y = "Total Self-esteem Score"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend since colors only represent gender

# Alternative: Create violin plot with a small boxplot inside
ggplot(analysis_data, aes(x = factor(sex), y = tslfest, fill = factor(sex))) +
  geom_violin(trim = FALSE, alpha = 0.5) +  # Violin plot with full density curve
  geom_boxplot(width = 0.2, fill = "white", outlier.shape = NA) +  # Add a small boxplot inside
  scale_fill_manual(values = c("lightblue", "lightpink")) + 
  labs(
    title = "Distribution of Self-esteem by Gender",
    x = "Gender",
    y = "Total Self-esteem Score"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend since colors only represent gender

# Add descriptive statistics
desc_stats <- analysis_data %>%
  group_by(sex) %>%
  get_summary_stats(tslfest, type = "common")

print(desc_stats)

# Checking Assumptions ---------------------------------------------------------

# 1. We can check normality both visually and statistically:

# Create Q-Q plots by group
ggqqplot(analysis_data, "tslfest", facet.by = "sex")

# Shapiro-Wilk test for each group
analysis_data %>%
  group_by(sex) %>%
  shapiro_test(tslfest)

# 2. Homogeneity of Variances

# Levene's test using car package
leveneTest(tslfest ~ sex, data = analysis_data)

# Conducting the T-Test --------------------------------------------------------

# Perform t-test
t_test_results <- analysis_data %>%
  t_test(tslfest ~ sex) %>%
  add_significance()

# Display results
t_test_results

# Effect Size Calculation ------------------------------------------------------

# Calculate Cohen's d (using `rstatix` package)
cohens_result <- cohens_d(tslfest ~ sex, data = analysis_data)
print(cohens_result)

# Interpret Cohen's d effect size with the `effectsize` package
interpretation <- interpret_cohens_d(cohens_result$Cohens_d)

# Print interpretation
print(interpretation)

# Complete Analysis for Control Levels -----------------------------------------

# Visualization
# Create boxplot with jittered points of perceived control by gender
ggplot(analysis_data, aes(x = factor(sex), y = tpcoiss)) +
  geom_boxplot(aes(fill = factor(sex)), outlier.shape = NA) +  # Boxplot without default outliers
  geom_jitter(width = 0.2, alpha = 0.5, color = "darkgray") +  # Individual points (jittered)
  scale_fill_manual(values = c("lightblue", "lightpink")) +
  labs(
    title = "Distribution of Perceived Control by Gender",
    x = "Gender",
    y = "Perceived Control"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend since colors only represent gender

# Check assumptions
# 1. Normality
analysis_data %>%
  group_by(sex) %>%
  shapiro_test(tpcoiss)

# 2. Homogeneity of variance
leveneTest(tpcoiss ~ sex, data = analysis_data)

# Perform t-test
control_t_test <- analysis_data %>%
  t_test(tpcoiss ~ sex) %>%
  add_significance()

# Calculate effect size
control_effect <- cohens_d(tpcoiss ~ sex, data = analysis_data)

# Display comprehensive results
list(
  "T-test Results" = control_t_test,
  "Effect Size" = control_effect
)

# Creating a Complete Report ---------------------------------------------------

# Function to run complete analysis
analyze_group_difference <- function(data, dv, group_var) {
  # Descriptive statistics
  desc <- data %>%
    group_by(!!sym(group_var)) %>%
    get_summary_stats(!!sym(dv), type = "common")
  
  # Assumption checks
  normality <- data %>%
    group_by(!!sym(group_var)) %>%
    shapiro_test(!!sym(dv))
  
  # Levene's test
  homogeneity <- leveneTest(as.formula(paste(dv, "~", group_var)), data = data)
  
  # T-test
  t_test <- data %>%
    t_test(as.formula(paste(dv, "~", group_var))) %>%
    add_significance()
  
  # Effect size
  effect <- cohens_d(as.formula(paste(dv, "~", group_var)), data = data)
  
  list(
    descriptives = desc,
    normality_test = normality,
    variance_test = homogeneity,
    t_test = t_test,
    effect_size = effect
  )
}

# Example usage
self_esteem_analysis <- analyze_group_difference(
  analysis_data, 
  "tslfest", 
  "sex"
)

# Display results
print(self_esteem_analysis)

# Writing Up Results -----------------------------------------------------------

# Create formatted output
report_results <- function(analysis) {
  with(analysis, {
    cat("Results:\n")
    cat("Descriptive Statistics:\n")
    print(descriptives)
    cat("\nAssumption Tests:\n")
    cat("Normality (Shapiro-Wilk):\n")
    print(normality_test)
    cat("\nHomogeneity of Variance (Levene's Test):\n")
    print(variance_test)
    cat("\nT-test Results:\n")
    print(t_test)
    cat("\nEffect Size:\n")
    print(effect_size)
  })
}

report_results(self_esteem_analysis)

#===============================================================================
# Step E3: Performing the T-Test (Control Levels by Gender)
#===============================================================================

#-------------------------------------------------------------------------------
# Base R Solution
#-------------------------------------------------------------------------------

# Visualize Control Levels by Gender -------------------------------------------

# Create boxplot with jittered points of perceived control by gender
ggplot(analysis_data, aes(x = factor(sex), y = tpcoiss)) +
  geom_boxplot(aes(fill = factor(sex)), outlier.shape = NA) +  # Boxplot without default outliers
  geom_jitter(width = 0.2, alpha = 0.5, color = "darkgray") +  # Individual points (jittered)
  scale_fill_manual(values = c("lightblue", "lightpink")) +
  labs(
    title = "Distribution of Perceived Control by Gender",
    x = "Gender",
    y = "Perceived Control"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend since colors only represent gender

# Perform the Analysis ---------------------------------------------------------

# Check basic descriptive statistics
tapply(survey_data_full$tslfest, 
       survey_data_full$sex, 
       function(x) c(mean = mean(x, na.rm = TRUE), 
                     sd = sd(x, na.rm = TRUE),
                     n = sum(!is.na(x))))

# Perform Levene's test for equality of variances
var_test <- var.test(tslfest ~ sex, data = survey_data_full)
print("Levene's test results:")
print(var_test)

# Perform t-test based on Levene's test result
t_test_result <- t.test(tslfest ~ sex, 
                        data = survey_data_full,
                        var.equal = var_test$p.value > 0.05)  # true if p > 0.05
print("\nt-test results:")
print(t_test_result)

# Effect Size ------------------------------------------------------------------

# Calculate Cohen's d
group1 <- survey_data_full$tslfest[survey_data_full$sex == "Male"]
group2 <- survey_data_full$tslfest[survey_data_full$sex == "Female"]

# We defined our function `calc_cohens_d()` earlier.

effect_size <- calc_cohens_d(group1, group2)
cat("Cohen's d effect size:", round(effect_size, 3))

#-------------------------------------------------------------------------------
# Solution with R Packages
#-------------------------------------------------------------------------------

# Install and Load Packages ----------------------------------------------------

# NOTE: We are using the same packages as in Step E2, so if you completed that 
#       step (including the alternative with R packages), you can skip this.

#Install packages if needed
install.packages(c("tidyverse", "car", "rstatix", "ggpubr", "effectsize"))

# Load required packages
library(tidyverse)  # For data manipulation and visualization
library(car)        # For Levene's test and additional diagnostics
library(rstatix)    # For statistical analysis
library(ggpubr)     # For publication-ready plots
library(effectsize) # For effect size calculations

# Complete Analysis for Control Levels -----------------------------------------

# Create boxplot with jittered points of perceived control by gender
ggplot(analysis_data, aes(x = factor(sex), y = tpcoiss)) +
  geom_boxplot(aes(fill = factor(sex)), outlier.shape = NA) +  # Boxplot without default outliers
  geom_jitter(width = 0.2, alpha = 0.5, color = "darkgray") +  # Individual points (jittered)
  scale_fill_manual(values = c("lightblue", "lightpink")) +
  labs(
    title = "Distribution of Perceived Control by Gender",
    x = "Gender",
    y = "Perceived Control"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend since colors only represent gender

# Check assumptions
# 1. Normality
analysis_data %>%
  group_by(sex) %>%
  shapiro_test(tpcoiss)

# 2. Homogeneity of variance
leveneTest(tpcoiss ~ sex, data = analysis_data)

# Perform t-test
control_t_test <- analysis_data %>%
  t_test(tpcoiss ~ sex) %>%
  add_significance()

# Calculate effect size
control_effect <- cohens_d(tpcoiss ~ sex, data = analysis_data)

# Display comprehensive results
list(
  "T-test Results" = control_t_test,
  "Effect Size" = control_effect
)

#===============================================================================
# Step F1: Understanding One-way ANOVA
#===============================================================================

# First, let's prepare our age groups properly
survey_data_full$agegp3 <- factor(survey_data_full$agegp3, 
                                  levels = c(1, 2, 3),
                                  labels = c("18-29", "30-44", "45+"))

# Create boxplot with jittered points of optimism levels by age group
ggplot(survey_data_full, aes(x = factor(agegp3), y = toptim)) +
  geom_boxplot(aes(fill = factor(agegp3)), outlier.shape = NA) +  # Boxplot without default outliers
  #  geom_boxplot(aes(fill = factor(sex)), outlier.shape = NA) +  # Boxplot without default outliers
  geom_jitter(width = 0.2, alpha = 0.5, color = "darkgray") +  # Individual points (jittered)
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightpink")) +
  labs(
    title = "Optimism Levels by Age Group",
    x = "Age Group",
    y = "Total Optimism Score"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend since colors only represent gender


#===============================================================================
# Step F2: Performing the One-Way ANOVA (Optimism Across Age Groups)
#===============================================================================

#-------------------------------------------------------------------------------
# Base R Solution
#-------------------------------------------------------------------------------

# Examine the Data -------------------------------------------------------------

# Create comprehensive descriptive statistics
get_descriptives <- function(dv, group) {
    tapply(dv, group, function(x) {
        c(n = sum(!is.na(x)),
          mean = mean(x, na.rm = TRUE),
          sd = sd(x, na.rm = TRUE),
          se = sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))))
    })
}

# Get descriptives for optimism by age group
descriptives <- get_descriptives(survey_data_full$toptim, 
                               survey_data_full$agegp3)
print("Descriptive Statistics:")
print(descriptives)

# Check Assumptions ------------------------------------------------------------

# Normality test (Shapiro-Wilk test)
shapiro.test(survey_data_full$toptim)

# Interpreation of Shapiro-Wilk test:
# - If p > 0.05 → Data is approximately normal → Levene’s Test
# - If p < 0.05 → Data is not normal → Brown-Forsythe Test

# Levene's test for homogeneity of variance
levene_test <- function(y, group) {
  group <- factor(group)
  means <- tapply(y, group, mean, na.rm = TRUE)
  abs_dev <- abs(y - means[group])
  anova(lm(abs_dev ~ group))[1, "Pr(>F)"]
}

# Perform Levene's test
levene_p <- levene_test(survey_data_full$toptim, survey_data_full$agegp3)
cat("Levene's test p-value:", levene_p, "\n")

# Brown-Forsythe test (more robust to non-normality)
bf_test <- function(y, group) {
  group <- factor(group)
  meds <- tapply(y, group, median, na.rm = TRUE)
  abs_dev <- abs(y - meds[group])
  anova(lm(abs_dev ~ group))[1, "Pr(>F)"]
}

bf_p <- bf_test(survey_data_full$toptim, survey_data_full$agegp3)
cat("Brown-Forsythe test p-value:", bf_p, "\n")

# Perform ANOVA ----------------------------------------------------------------

# Standard one-way ANOVA
anova_result <- aov(toptim ~ agegp3, data = survey_data_full)
print("Standard ANOVA results:")
print(summary(anova_result))

# Alternative: Welch ANOVA (robust to unequal variances) -----------------------

# Welch's ANOVA (robust to unequal variances)
welch_result <- oneway.test(toptim ~ agegp3, 
                            data = survey_data_full, 
                            var.equal = FALSE)
print("\nWelch's ANOVA results:")
print(welch_result)

# Tukey's HSD Test for Pairwise Comparisons ------------------------------------

# If we find significant differences, we need to know which groups differ from 
# each other, for which we can use Tukey's HSD test for pairwise comparisons.
  
tukey_result <- TukeyHSD(anova_result)
print("Tukey's HSD test results:")
print(tukey_result)

# Visualize the Tukey results
plot(tukey_result)

# Calculate Eta-Squared for Practical Significance -----------------------------

aov_summary <- summary(anova_result)
eta_squared <- aov_summary[[1]]$"Sum Sq"[1] / sum(aov_summary[[1]]$"Sum Sq")
cat("Eta-squared:", round(eta_squared, 3))

#-------------------------------------------------------------------------------
# Solution with R Packages
#-------------------------------------------------------------------------------

# Install and Load Packages ----------------------------------------------------

# Install packages if needed
install.packages(c("tidyverse", "car", "rstatix", "ggpubr", "effectsize", "emmeans"))

# Load required packages
library(tidyverse)  # For data manipulation and visualization
library(car)        # For Levene's test and additional diagnostics
library(rstatix)    # For statistical analysis
library(ggpubr)    # For publication-ready plots
library(effectsize) # For effect size calculations
library(emmeans)    # For estimated marginal means and post-hoc tests

# Data Preparation -------------------------------------------------------------

# Convert age groups to a factor with meaningful labels
analysis_data <- survey_data_full %>%
  select(agegp3, educ, toptim, tpstress)

# Display the structure of our prepared data
glimpse(analysis_data)

# Visual Exploration -----------------------------------------------------------

# Create violin plot with boxplot and individual points
ggviolin(analysis_data, 
         x = "age_group", 
         y = "toptim",
         fill = "age_group",
         add = "boxplot",
         add.params = list(fill = "white")) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  theme_minimal() +
  labs(title = "Optimism Levels by Age Group",
       y = "Total Optimism Score",
       x = "Age Group") +
  theme(legend.position = "none")

# Add descriptive statistics
desc_stats <- analysis_data %>%
  group_by(age_group) %>%
  get_summary_stats(toptim, type = "common")
print(desc_stats)

# Checking Assumptions ---------------------------------------------------------

# 1. Normality Check

# Create Q-Q plots by group
ggqqplot(analysis_data, "toptim", facet.by = "age_group")

# Shapiro-Wilk test for each group
analysis_data %>%
  group_by(age_group) %>%
  shapiro_test(toptim)

# 2. Homogeneity of Variances

# Levene's test using car package
leveneTest(toptim ~ age_group, data = analysis_data)

# Brown-Forsythe test using car package
leveneTest(toptim ~ age_group, data = analysis_data, center = median)

# Conducting the ANOVA ---------------------------------------------------------

# Perform one-way ANOVA
anova_results <- analysis_data %>%
  anova_test(toptim ~ age_group) %>%
  add_significance()

# Display results
anova_results

# Welch's ANOVA (robust to heterogeneity of variance)
welch_results <- analysis_data %>%
  welch_anova_test(toptim ~ age_group)

print("Welch's ANOVA results:")
welch_results

# Post-hoc Analysis ------------------------------------------------------------

# Tukey's HSD using emmeans
emmeans_result <- emmeans(aov(toptim ~ age_group, data = analysis_data), 
                          "age_group")
pairs(emmeans_result)

# Games-Howell post-hoc test (robust to heterogeneity of variance)
games_howell_result <- analysis_data %>%
  games_howell_test(toptim ~ age_group)
print("Games-Howell test results:")
games_howell_result

# Effect Size Calculation ------------------------------------------------------------

# Calculate eta squared
eta_squared <- effectsize::eta_squared(aov(toptim ~ age_group, data = analysis_data))
print(eta_squared)

# Interpret effect size
interpret_eta_squared(eta_squared$Eta2)

# Effect Size Calculation ------------------------------------------------------------



#===============================================================================
# Step D4: Linear Regression
#===============================================================================

#-------------------------------------------------------------------------------
# Creating Our Linear Regression Model
#-------------------------------------------------------------------------------




#===============================================================================
# Step G1: Chi-Square Test for Goodness of Fit (Smoking Rates)
#===============================================================================

#-------------------------------------------------------------------------------
# Base R Solution
#-------------------------------------------------------------------------------

# In this example, we check whether our sample's smoking rate matches the 
# expected population rate of 20%.

# First, let's look at our data
smoker_table <- table(survey_data_full$smoker)
print("Observed frequencies:")
print(smoker_table)

# Calculate percentages
smoker_props <- prop.table(smoker_table) * 100
print("\nObserved percentages:")
print(round(smoker_props, 1))

# Perform Chi-Square test
# Expected proportions (20% smokers, 80% non-smokers)
expected_props <- c(0.2, 0.8)

chisq_result <- chisq.test(smoker_table, p = expected_props)

# Display detailed results
print("\nChi-Square Test Results:")
print(chisq_result)

# Calculate and display expected frequencies
n_total <- sum(smoker_table)
expected_freq <- n_total * expected_props

print("\nComparison of observed vs expected frequencies:")
comparison_df <- data.frame(
  Category = c("Smokers", "Non-smokers"),
  Observed = as.numeric(smoker_table),
  Expected = expected_freq,
  Difference = as.numeric(smoker_table) - expected_freq
)
print(comparison_df)

#-------------------------------------------------------------------------------
# Solution with R Packages
#-------------------------------------------------------------------------------

# Install and Load Packages ----------------------------------------------------

# Install packages if needed
install.packages(c("tidyverse", "rstatix", "ggstatsplot", "ggsignif", 
                   "corrplot", "effectsize", "DescTools"))

# Load required packages
library(tidyverse)   # For data manipulation and visualization
library(rstatix)     # For statistical analysis
library(ggstatsplot) # For statistical visualization
library(ggsignif)    # For significance annotations
library(corrplot)    # For visualization of relationships
library(effectsize)  # For effect size calculations
library(DescTools)   # For additional statistical tools

# Data Preparation -------------------------------------------------------------

# Prepare data for analysis
analysis_data %
mutate(
  sex = factor(sex, levels = c(1, 2), labels = c("Male", "Female")),
  smoker = factor(smoker, levels = c(1, 2), labels = c("Smoker", "Non-smoker"))
)

# Display the structure of our prepared data
glimpse(analysis_data)

# Enhanced Visualization of Observed vs Expected -------------------------------

# Create enhanced bar plot with statistical results
ggbarstats(
  data = analysis_data,
  x = smoker,
  title = "Observed vs Expected Smoking Rates",
  xlab = "Smoking Status",
  ylab = "Count",
  results.subtitle = TRUE,
  subtitle = "Testing against expected 20% smoking rate",
  type = "parametric",
  paired = FALSE,
  null.value = 0.2
)

# Comprehensive Statistical Analysis -------------------------------------------

# Calculate observed frequencies
obs_freq %
count(smoker) %>%
  mutate(
    prop = n / sum(n),
    expected_prop = c(0.2, 0.8),
    expected_n = sum(n) * expected_prop,
    contrib = (n - expected_n)^2 / expected_n
  )

# Display comprehensive results
obs_freq %>%
  knitr::kable(digits = 3)

# Perform test with effect size
chisq_result %
add_significance()

# Calculate effect size
effect <- cramers_v(table(analysis_data$smoker))

# Display results
list(
  "Chi-square test results" = chisq_result,
  "Effect size (Cramer's V)" = effect
)


#===============================================================================
# Step G3: Chi-Square Test for Independence (Smoking and Gender)
#===============================================================================

#-------------------------------------------------------------------------------
# Base R Solution
#-------------------------------------------------------------------------------

# Cross-Tabulation and Examining Data ------------------------------------------

# Create cross-tabulation
cross_tab <- table(survey_data_full$sex, survey_data_full$smoker)

# Add row and column names for clarity
dimnames(cross_tab) <- list(
  Gender = c("Male", "Female"),
  Smoking = c("Smoker", "Non-smoker")
)

# Display the cross-tabulation
print("Cross-tabulation of Gender and Smoking:")
print(cross_tab)

# Calculate and display percentages by gender
prop_table <- prop.table(cross_tab, margin = 1) * 100
print("\nPercentages within each gender:")
print(round(prop_table, 1))

# Check Assumptions ------------------------------------------------------------

# Calculate expected frequencies
expected <- chisq.test(cross_tab)$expected
print("\nExpected frequencies:")
print(round(expected, 2))

# Check if any expected frequencies are less than 5
min_expected <- min(expected)
print("\nMinimum expected frequency:", min_expected)
if(min_expected < 5) {
  print("Warning: Some expected frequencies are less than 5!")
}

# Perform Chi-Square Test of Independence --------------------------------------

# Perform Chi-Square test with continuity correction (for 2x2 tables)
chi_result <- chisq.test(cross_tab, correct = TRUE)
print("\nChi-Square Test Results:")
print(chi_result)

# Calculate effect size (Cramer's V)
n <- sum(cross_tab)
cramer_v <- sqrt(chi_result$statistic / (n * (min(dim(cross_tab)) - 1)))
print("\nCramer's V:", round(cramer_v, 3))

# Standardized Residuals -------------------------------------------------------

# Calculate adjusted standardized residuals
stdres <- chisq.test(cross_tab)$stdres
dimnames(stdres) <- dimnames(cross_tab)
print("\nAdjusted standardized residuals:")
print(round(stdres, 2))

# Helpful Visualizations -------------------------------------------------------

# Create barplot of smoking rates by gender
barplot(prop_table,
        beside = TRUE,
        main = "Smoking Status by Gender",
        xlab = "Smoking Status",
        ylab = "Percentage",
        col = c("lightblue", "pink"),
        legend.text = TRUE)

#-------------------------------------------------------------------------------
# Solution with R Packages
#-------------------------------------------------------------------------------

# Install and Load Packages ----------------------------------------------------

# Install packages if needed
install.packages(c("tidyverse", "rstatix", "ggstatsplot", "ggsignif", 
                   "corrplot", "effectsize", "DescTools"))

# Load required packages
library(tidyverse)   # For data manipulation and visualization
library(rstatix)     # For statistical analysis
library(ggstatsplot) # For statistical visualization
library(ggsignif)    # For significance annotations
library(corrplot)    # For visualization of relationships
library(effectsize)  # For effect size calculations
library(DescTools)   # For additional statistical tools

# Data Preparation -------------------------------------------------------------

# Prepare data for analysis
analysis_data %
mutate(
  sex = factor(sex, levels = c(1, 2), labels = c("Male", "Female")),
  smoker = factor(smoker, levels = c(1, 2), labels = c("Smoker", "Non-smoker"))
)

# Display the structure of our prepared data
glimpse(analysis_data)

# Enhanced Cross-Tabulation Visualization --------------------------------------

# Create an enhanced visualization of the relationship
ggbarstats(
  data = analysis_data,
  x = sex,
  y = smoker,
  title = "Relationship between Gender and Smoking Status",
  xlab = "Gender",
  ylab = "Proportion",
  label = "percentage",
  results.subtitle = TRUE
)

# Comprehensive Analysis -------------------------------------------------------

# Perform analysis with rstatix
independence_test %
tabyl(sex, smoker) %>%
  chisq_test() %>%
  add_significance()

# Calculate effect size
independence_effect %
tabyl(sex, smoker) %>%
  cramer_v()

# Display results
list(
  "Chi-square test results" = independence_test,
  "Effect size (Cramer's V)" = independence_effect
)

# Post-hoc Analysis ------------------------------------------------------------

# Calculate and visualize standardized residuals
residuals_analysis %
tabyl(sex, smoker) %>%
  chisq_test() %>%
  subset_residuals()

# Create heatmap of residuals
ggplot(residuals_analysis, aes(sex, smoker, fill = residual)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white") +
  geom_text(aes(label = round(residual, 2))) +
  theme_minimal() +
  labs(title = "Standardized Residuals",
       fill = "Residual")

# Effect Size Visualization ----------------------------------------------------

# Calculate and visualize effect sizes
effect_size <- effectsize::cramers_v(
  table(analysis_data$sex, analysis_data$smoker)
)

# Create effect size plot
plot(effect_size) +
  theme_minimal() +
  labs(title = "Effect Size (Cramer's V) with Confidence Interval")

# Creating a Complete Report ---------------------------------------------------

analyze_categorical %
count(!!sym(var1)) %>%
  mutate(proportion = n/sum(n)),

visualization = ggbarstats(
  data = data,
  x = !!sym(var1)
),

test = data %>%
  pull(!!sym(var1)) %>%
  table() %>%
  chisq_test() %>%
  add_significance(),

effect = data %>%
  pull(!!sym(var1)) %>%
  table() %>%
  cramers_v()
)
} else {
  # Independence test
  result %
  tabyl(!!sym(var1), !!sym(var2)),
  
  visualization = ggbarstats(
    data = data,
    x = !!sym(var1),
    y = !!sym(var2)
  ),
  
  test = data %>%
    tabyl(!!sym(var1), !!sym(var2)) %>%
    chisq_test() %>%
    add_significance(),
  
  effect = data %>%
    tabyl(!!sym(var1), !!sym(var2)) %>%
    cramer_v()
  )
}

return(result)
}

# Example usage
smoking_analysis <- analyze_categorical(
  data = analysis_data,
  var1 = "sex",
  var2 = "smoker"
)

# Display results
smoking_analysis


