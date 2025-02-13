################################################################################
#   R Code for Lab 2 Exercise I (MGT4018/MGT4090)
#                       
#   Dr Bernd Wurth (bernd.wurth@glasgow.ac.uk)
#
#   Last modified: 10 February 2025
################################################################################

#===============================================================================
# Step I1: Setup
#===============================================================================


#===============================================================================
# Step I2: Load the Dataset
#===============================================================================

#-------------------------------------------------------------------------------
# Load the Dataset
#-------------------------------------------------------------------------------

# Load the CSV file stored in the "data" folder
sleep_survey_data <- read.csv("data/lab2-sleep-survey.csv")

#-------------------------------------------------------------------------------
# Check the Dataset
#-------------------------------------------------------------------------------

# View the first few rows using head()
head(sleep_survey_data)

# Examine the structure
str(sleep_survey_data)

# Get basic summary statistics
summary(sleep_survey_data)

#-------------------------------------------------------------------------------
# Assign Labels for Categorical Variables
#-------------------------------------------------------------------------------

# Let's start by creating our factor labels in a clear, organized way.
# When working with binary variables (0/1), it's especially important to be 
# consistent with our labeling approach.

# First, let's handle the binary (0/1) variables
# We'll create these first since they share the same structure
sleep_survey_data$sex <- factor(sleep_survey_data$sex,
                                levels = c(0, 1),
                                labels = c("female", "male"))

sleep_survey_data$probsleep <- factor(sleep_survey_data$probsleep,
                                      levels = c(0, 1),
                                      labels = c("no", "yes"))

sleep_survey_data$fallsleep <- factor(sleep_survey_data$fallsleep,
                                      levels = c(0, 1),
                                      labels = c("no", "yes"))

sleep_survey_data$staysleep <- factor(sleep_survey_data$staysleep,
                                      levels = c(0, 1),
                                      labels = c("no", "yes"))

# Now let's handle the categorical variables with multiple levels
# For these, we'll use more descriptive labels
sleep_survey_data$marital <- factor(sleep_survey_data$marital,
                                    levels = 1:4,
                                    labels = c("single", 
                                               "married/defacto",
                                               "divorced",
                                               "widowed"))

sleep_survey_data$edlevel <- factor(sleep_survey_data$edlevel,
                                    levels = 1:5,
                                    labels = c("primary school",
                                               "secondary school",
                                               "trade training",
                                               "undergraduate degree",
                                               "postgraduate degree"))

# Let's add a verification step to make sure our conversions worked correctly
# This function will print the levels of each factor variable we created
verify_factors <- function(data) {
  cat("Checking factor levels for all converted variables:\n\n")
  
  # List of variables we converted
  factor_vars <- c("sex", "probsleep", "fallsleep", "staysleep", 
                   "marital", "edlevel")
  
  for(var in factor_vars) {
    cat(paste0("Levels for ", var, ":\n"))
    print(levels(data[[var]]))
    cat("\n")
  }
}

# Run the verification
verify_factors(sleep_survey_data)


#===============================================================================
# Step I3: Logistic Regression (Base R)
#===============================================================================

#-------------------------------------------------------------------------------
# Initial Data Preparation
#-------------------------------------------------------------------------------

# First, let's check our sample size and missing data
initial_sample <- nrow(sleep_survey_data)
complete_cases <- sum(complete.cases(sleep_survey_data[
  c("probsleep", "sex", "age", "fallsleep", "staysleep", "hrswknight")
]))

# Print sample information
cat("Total cases:", initial_sample, "\n")
cat("Complete cases:", complete_cases, "\n")
cat("Missing cases:", initial_sample - complete_cases, "\n")

# Check coding of categorical variables
cat("\nCoding of categorical variables:\n")
cat("\nSleep Problems (probsleep):\n")
print(table(sleep_survey_data$probsleep))

cat("\nSex:\n")
print(table(sleep_survey_data$sex))

cat("\nTrouble Falling Asleep (fallsleep):\n")
print(table(sleep_survey_data$fallsleep))

cat("\nTrouble Staying Asleep (staysleep):\n")
print(table(sleep_survey_data$staysleep))

#-------------------------------------------------------------------------------
# Building the Logistic Regression Model
#-------------------------------------------------------------------------------

# Create the logistic regression model
sleep_model <- glm(probsleep ~ sex + age + fallsleep + staysleep + hrswknight,
                   family = binomial(link = "logit"),
                   data = sleep_survey_data)

# Display the summary of our model
summary_results <- summary(sleep_model)
print(summary_results)

# Calculate odds ratios and confidence intervals
odds_ratios <- exp(coef(sleep_model))
conf_int <- exp(confint(sleep_model))

# Combine results in a readable format
results_table <- cbind(
  Estimate = round(coef(sleep_model), 3),
  "Odds Ratio" = round(odds_ratios, 3),
  "CI Lower" = round(conf_int[,1], 3),
  "CI Upper" = round(conf_int[,2], 3),
  "p-value" = round(summary_results$coefficients[,4], 3)
)

# Print results
cat("\nDetailed Results:\n")
print(results_table)

# Calculate model fit statistics
null_deviance <- sleep_model$null.deviance
model_deviance <- sleep_model$deviance
pseudo_r2 <- 1 - (model_deviance / null_deviance)

cat("\nModel Fit Statistics:\n")
cat("Null deviance:", round(null_deviance, 2), "\n")
cat("Model deviance:", round(model_deviance, 2), "\n")
cat("McFadden's Pseudo R²:", round(pseudo_r2, 3), "\n")


#===============================================================================
# Step I4: Interpreting the Results (Base R)
#===============================================================================

#-------------------------------------------------------------------------------
# Individual Predictors
#-------------------------------------------------------------------------------

# Create a function to interpret odds ratios
interpret_or <- function(or, p_value) {
  direction <- if(or > 1) "increase" else "decrease"
  magnitude <- abs(1 - or) * 100
  significance <- if(p_value < 0.05) "significant" else "not significant"
  
  sprintf("%.1f%% %s in odds (%s)", magnitude, direction, significance)
}

# Print interpretations for each predictor
cat("Interpretation of Effects:\n\n")
for(i in 2:nrow(results_table)) {
  var_name <- rownames(results_table)[i]
  or <- results_table[i, "Odds Ratio"]
  p_val <- results_table[i, "p-value"]
  
  cat(var_name, ":", interpret_or(or, p_val), "\n")
}

#-------------------------------------------------------------------------------
# Model Accuracy
#-------------------------------------------------------------------------------

# Calculate predicted probabilities
predicted_probs <- predict(sleep_model, type = "response")
predicted_class <- ifelse(predicted_probs > 0.5, 1, 0)
actual_class <- as.numeric(sleep_survey_data$probsleep) - 1

# Create confusion matrix
conf_matrix <- table(Predicted = predicted_class, Actual = actual_class)
print("\nConfusion Matrix:")
print(conf_matrix)

# Calculate accuracy metrics
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
sensitivity <- conf_matrix[2,2] / sum(conf_matrix[,2])
specificity <- conf_matrix[1,1] / sum(conf_matrix[,1])

cat("\nModel Performance Metrics:\n")
cat("Accuracy:", round(accuracy, 3), "\n")
cat("Sensitivity:", round(sensitivity, 3), "\n")
cat("Specificity:", round(specificity, 3), "\n")


#===============================================================================
# Step I3: Logistic Regression (R Packages)
#===============================================================================

#-------------------------------------------------------------------------------
# Install and Load Packages
#-------------------------------------------------------------------------------

# Install packages if needed
install.packages(c("tidyverse", "broom", "performance", "sjPlot", "marginaleffects"))

# Load required packages
library(tidyverse)    # For data manipulation and visualization
library(broom)        # For tidying model output
library(performance)  # For model diagnostics and performance metrics
library(sjPlot)      # For plotting odds ratios and model summaries
library(marginaleffects)  # For calculating and plotting marginal effects

#-------------------------------------------------------------------------------
# Understanding our Data
#-------------------------------------------------------------------------------

# Create our analysis dataset
sleep_data <- sleep_survey_data %>%
  select(probsleep, sex, age, fallsleep, staysleep, hrswknight) %>%
  # Convert to factors with meaningful labels if not already done
  mutate(across(c(probsleep, sex, fallsleep, staysleep), 
                ~factor(., levels = c(0, 1), 
                        labels = c("No", "Yes"))))

# Examine our data structure
glimpse(sleep_data)

# Check missing data patterns
missing_pattern <- sleep_data %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), 
               names_to = "Variable", 
               values_to = "Missing_Count")

ggplot(missing_pattern, 
       aes(x = reorder(Variable, Missing_Count), y = Missing_Count)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Missing Data Pattern",
       x = "Variable",
       y = "Number of Missing Values")

#-------------------------------------------------------------------------------
# Building the Logistic Regression Model
#-------------------------------------------------------------------------------

# Fit the model
sleep_model <- glm(probsleep ~ sex + age + fallsleep + staysleep + hrswknight,
                   family = binomial(link = "logit"),
                   data = sleep_data)

# Get tidy model summary
model_summary <- tidy(sleep_model, 
                      conf.int = TRUE, 
                      exponentiate = TRUE)

# Create a nice summary table
model_summary %>%
  mutate(across(where(is.numeric), ~round(., 3))) %>%
  knitr::kable(col.names = c("Predictor", "Odds Ratio", "Std. Error", 
                             "z value", "p-value", "CI Lower", "CI Upper"),
               caption = "Logistic Regression Results") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))

#-------------------------------------------------------------------------------
# Model Diagnostics and Performance
#-------------------------------------------------------------------------------

# Check model performance
model_performance <- check_model(sleep_model)

# Get various performance metrics
performance_metrics <- model_performance(sleep_model)

# Plot ROC curve
pred_data <- sleep_data %>%
  mutate(predicted = predict(sleep_model, type = "response"))

ggplot(pred_data, aes(d = as.numeric(probsleep) - 1, m = predicted)) +
  geom_roc(n.cuts = 0) +
  style_roc() +
  theme_minimal() +
  labs(title = "ROC Curve for Sleep Problems Model")

#-------------------------------------------------------------------------------
# Understanding Variable Effects
#-------------------------------------------------------------------------------

# Calculate average marginal effects
marg_effects <- avg_slopes(sleep_model)

# Plot marginal effects
plot_slopes(sleep_model, 
            variables = "hrswknight",
            condition = "staysleep") +
  theme_minimal() +
  labs(title = "Effect of Sleep Hours by Staying Asleep Status",
       x = "Hours of Sleep per Weeknight",
       y = "Predicted Probability of Sleep Problems")

# Create effects plot for categorical variables
plot_model(sleep_model, 
           type = "pred", 
           terms = c("staysleep", "fallsleep")) +
  theme_minimal() +
  labs(title = "Interaction between Falling and Staying Asleep",
       y = "Predicted Probability of Sleep Problems")

#-------------------------------------------------------------------------------
# Visualizing Model Results
#-------------------------------------------------------------------------------

# Plot odds ratios
plot_model(sleep_model, 
           show.values = TRUE, 
           title = "Odds Ratios for Sleep Problems") +
  theme_minimal()

# Create predicted probabilities plot
plot_model(sleep_model, 
           type = "pred", 
           terms = "hrswknight",
           title = "Effect of Sleep Hours on Problem Probability") +
  theme_minimal()

#-------------------------------------------------------------------------------
# Creating a Complete Report
#-------------------------------------------------------------------------------

# Function to create a complete model summary
create_model_report <- function(model) {
  # Model summary
  summary_stats <- glance(model)
  
  # Coefficients and odds ratios
  coef_table <- tidy(model, conf.int = TRUE, exponentiate = TRUE)
  
  # Predictions
  pred_data <- augment(model, type.predict = "response")
  
  # Return as list
  list(
    model_stats = summary_stats,
    coefficients = coef_table,
    predictions = pred_data
  )
}

# Generate report
model_report <- create_model_report(sleep_model)

# Print key findings
cat("Model Performance Metrics:\n")
print(model_report$model_stats)

cat("\nKey Predictors:\n")
print(model_report$coefficients)

#-------------------------------------------------------------------------------
# Practical Implementation
#-------------------------------------------------------------------------------

# Create a prediction function for new cases
predict_sleep_problems <- function(new_data) {
  predictions <- predict(sleep_model, 
                         newdata = new_data, 
                         type = "response")
  
  tibble(
    probability = predictions,
    predicted_outcome = if_else(predictions > 0.5, "Yes", "No")
  )
}

# Example usage
example_cases <- tibble(
  sex = factor(c("Male", "Female"), levels = c("Male", "Female")),
  age = c(30, 45),
  fallsleep = factor(c("Yes", "No"), levels = c("No", "Yes")),
  staysleep = factor(c("No", "Yes"), levels = c("No", "Yes")),
  hrswknight = c(7, 6)
)

predictions <- predict_sleep_problems(example_cases)
print(predictions)
