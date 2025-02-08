################################################################################
#   R Code for Lab 2 Exercises D, E, F and G (MGT4018/MGT4090)
#                       
#   Dr Bernd Wurth (bernd.wurth@glasgow.ac.uk)
#
#   Last modified: 22 December 2024
################################################################################

#===============================================================================
# Step D1: Setup
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

# Calculate correlation between tpcoiss and tpstress
cor(survey_data_full$tpcoiss, survey_data_full$tpstress)

# Specify the method (default is Pearson)
cor(survey_data_full$tpcoiss, survey_data_full$tpstress, method = "pearson")

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
correlation_matrix <- cor(survey_data_small)

# Round to 3 decimal places for clarity
round(correlation_matrix, 3)

#-------------------------------------------------------------------------------
# Statistical Testing with `cor.test()`
#-------------------------------------------------------------------------------

# Perform correlation test
correlation_test <- cor.test(x, y1)

# View complete results
print(correlation_test)
