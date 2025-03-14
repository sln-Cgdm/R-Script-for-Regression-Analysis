# Load required libraries
library(haven)     # For reading .dta files
library(dplyr)     # For data wrangling
library(ggplot2)   # For visualization if needed
library(car)       # For regression diagnostics
library(mice)      # Imputation for missing data
library(lmtest)    # Regression tests
library(stargazer) # Regression tables
library(forcats)

# Load the dataset
worker_data <- read_dta("/Users/selincigdemozturk/Desktop/OECD data/worker_shared.dta")

################################## AI Decision-Making Impact INDEX (Q30) ##################################

# Select relevant variables for AI impact on decision-making (Q30)
q30_index <- c("impactdecisionsuser_1",  # AI helps me make faster decisions.
               "impactdecisionsuser_2",  # AI helps me make better decisions.
               "impactdecisionsuser_3")  # I like that AI assists me with decision-making.

# Convert selected columns to numeric (handling factor or character values)
worker_data <- worker_data %>%
  mutate(across(all_of(q30_index), as.numeric))

# Compute AI Decision-Making Impact Index (AIDI) as the mean of valid responses
worker_data <- worker_data %>%
  rowwise() %>%
  mutate(AIDI = mean(c(impactdecisionsuser_1, 
                       impactdecisionsuser_2, 
                       impactdecisionsuser_3), na.rm = TRUE)) %>%
  ungroup()

# Print summary of AIDI
summary(worker_data$AIDI)

# Optional: Histogram of AIDI
ggplot(worker_data, aes(x = AIDI)) +
  geom_histogram(binwidth = 0.5, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of AI Decision-Making Impact Index (AIDI)",
       x = "AIDI Score",
       y = "Frequency") +
  theme_minimal()

################################## Fairness, Autonomy, Trust Perception INDEX ################################

# Compute Fairness Perception Index (Q36)
worker_data <- worker_data %>%
  rowwise() %>%
  mutate(Fairness_Index = mean(c(impactmanagementuser), na.rm = TRUE)) %>%
  ungroup()

# Compute Autonomy Index (Q31)
worker_data <- worker_data %>%
  rowwise() %>%
  mutate(Autonomy_Index = mean(c(impactautonomyuser_1, impactautonomyuser_2), na.rm = TRUE)) %>%
  ungroup()

# Compute Trust in AI Index (Q75)
worker_data <- worker_data %>%
  rowwise() %>%
  mutate(Trust_AI_Index = mean(c(trustcompany_1, trustcompany_2, trustcompany_3, 
                                 trustcompany_4, trustcompany_5), na.rm = TRUE)) %>%
  ungroup()

############################################################################################################
colnames(worker_data)
unique(worker_data$impactmanagementuser)
unique(worker_data$impactautonomyuser_1) 
unique(worker_data$impactautonomyuser_2)

#Age
cat("\nSector (sector):\n")
print(unique(worker_data$employeeagecat2))
print(attr(worker_data$employeeagecat2, "labels"))

# Sector
cat("\nSector (sector):\n")
print(unique(worker_data$sector))
print(attr(worker_data$sector, "labels"))

# Employee Sex
cat("\nEmployee Sex (employeesex):\n")
print(unique(worker_data$employeesex))
print(attr(worker_data$employeesex, "labels"))

# Education
cat("\nEducation (education):\n")
print(unique(worker_data$education))
print(attr(worker_data$education, "labels"))


# Business Size
cat("\nCompany Size (businesssizebands):\n")
print(unique(worker_data$businesssizebands))
print(attr(worker_data$businesssizebands, "labels"))

# Employee Role
cat("\nEmployee Role (employeerole):\n")
print(unique(worker_data$employeerole))
print(attr(worker_data$employeerole, "labels"))

# Convert categorical variables to factors
worker_data$employeeagecat2 <- factor(worker_data$employeeagecat2, 
                                      levels = c(1, 2, 3, 4, 5, 6, 9), 
                                      labels = c("Age below 16", 
                                                 "Age between 16 and 24", 
                                                 "Age between 25 and 34", 
                                                 "Age between 35 and 49", 
                                                 "Age between 50 and 64", 
                                                 "Age 65 or above", 
                                                 "Age not specified"))

worker_data$sector <- factor(worker_data$sector, 
                             levels = c(1, 2), 
                             labels = c("Finance and Insurance", "Manufacturing"))

worker_data$employeesex <- factor(worker_data$employeesex, 
                                  levels = c(1, 2, 3), 
                                  labels = c("Male", "Female", "Gender not specified"))

worker_data$education <- factor(worker_data$education, 
                                levels = c(1, 2, 9), 
                                labels = c("Yes", "No", "No Answer"))


worker_data$businesssizebands <- factor(worker_data$businesssizebands, 
                                        levels = c(1, 2, 3, 4, 5, 6, 9), 
                                        labels = c("Up to 19", "20 to 49", "50 to 99", 
                                                   "100 to 249", "250 to 499", "500+", 
                                                   "Don't know"))

worker_data$employeerole <- factor(worker_data$employeerole, 
                                   levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
                                   labels = c("Manager", "Professional", 
                                              "Technician and Associate", 
                                              "Clerical Support", 
                                              "Service and Sales", 
                                              "Craft and Related Trades", 
                                              "Plant and Machine Operator", 
                                              "Elementary Occupation", 
                                              "Other", "No Answer"))
# Convert factor columns to numeric
worker_data_numeric <- worker_data %>% 
  mutate(across(where(is.factor), as.numeric))

# Correct correlation matrix function
cor(worker_data_numeric[, c("impactmanagementuser", "impactautonomyuser_1", 
                            "impactautonomyuser_2", "sector", "employeesex", "employeeagecat2",
                            "education", "businesssizebands", "employeerole")], 
    use = "pairwise.complete.obs")

################################## REGRESSION ANALYSES #############################################
# Regression for AIDI
model_AIDI <- lm(AIDI ~ sector + employeesex + employeeagecat2 + education + businesssizebands + employeerole, 
                 data = worker_data)
summary(model_AIDI)

# Regression for Autonomy
model_autonomy <- lm(Autonomy_Index ~ sector + employeesex + employeeagecat2 + education + businesssizebands + employeerole, 
                     data = worker_data)
summary(model_autonomy)

# Regression for Trust in AI
model_trust <- lm(Trust_AI_Index ~ sector + employeesex + employeeagecat2 + education + businesssizebands + employeerole, 
                  data = worker_data)
summary(model_trust)

# Regression for Fairness
model_fairness <- lm(Fairness_Index ~ sector + employeesex + employeeagecat2 + education + businesssizebands + employeerole, 
                     data = worker_data)
summary(model_fairness)

################################## Check for Multicollinearity ##################################

# Check multicollinearity for AIDI model
vif(model_AIDI)

# Check multicollinearity for Fairness model
vif(model_fairness)

# Check multicollinearity for Autonomy model
vif(model_autonomy)

# Check multicollinearity for Trust in AI model
vif(model_trust)

##################################  Address High Missingness ################################## 

# Impute missing values using Predictive Mean Matching (pmm)
imputed_data <- mice(worker_data, method = "pmm", m = 5, seed = 123)

# Extract complete dataset from first imputation
worker_data <- complete(imputed_data, 1)


##################################  Test Interaction Effects   ##################################

model_interaction <- lm(AIDI ~ impactmanagementuser * education + 
                          impactautonomyuser_1 * sector + 
                          impactautonomyuser_2 * employeesex + 
                          Trust_AI_Index * employeerole, data = worker_data)

summary(model_interaction)

############################################# Linearity Check ######################################

# Scatterplot matrix for visualizing linear relationships
pairs(worker_data[, c("AIDI", "Fairness_Index", "Autonomy_Index", "Trust_AI_Index")], 
      main = "Scatterplot Matrix", 
      col = "blue", pch = 19)

# Individual scatterplots with trend lines
ggplot(worker_data, aes(x = Fairness_Index, y = AIDI)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", col = "red") +
  labs(title = "AIDI vs Fairness Index")

ggplot(worker_data, aes(x = Autonomy_Index, y = AIDI)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", col = "red") +
  labs(title = "AIDI vs Autonomy Index")

ggplot(worker_data, aes(x = Trust_AI_Index, y = AIDI)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", col = "red") +
  labs(title = "AIDI vs Trust AI Index")

############################################# Normality of Residuals ###############################################################

# Histogram of residuals
hist(residuals(model_AIDI), breaks = 30, col = "blue", main = "Histogram of Residuals")

# Q-Q Plot (Normality check)
qqnorm(residuals(model_AIDI))
qqline(residuals(model_AIDI), col = "red")

# Shapiro-Wilk test for normality (only valid for small datasets)
shapiro.test(residuals(model_AIDI))

#############################################  Homoscedasticity   ############################################# 

# Residuals vs Fitted plot
plot(model_AIDI$fitted.values, residuals(model_AIDI),
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 19, col = "blue")
abline(h = 0, col = "red")

# Breusch-Pagan test for heteroscedasticity
bptest(model_AIDI)
############################################# VIF for Model AIDI  #############################################

vif(model_AIDI)

############################################# Outlier Detection  #############################################

# Cook’s Distance
plot(model_AIDI, which = 4, main = "Cook's Distance")

# Identify highly influential points
influential_points <- which(cooks.distance(model_AIDI) > (4/length(residuals(model_AIDI))))
print(influential_points)

############################################ Handling Influential Observations  ##########################################

# Calculate Cook's Distance for each observation
cooksd <- cooks.distance(model_AIDI)

# Set a threshold (4/n rule of thumb)
threshold <- 4 / length(cooksd)

# Identify influential points above the threshold
influential_points <- which(cooksd > threshold)

# Print the most influential points
print(influential_points)

# Plot Cook's Distance to visualize influential points
plot(cooksd, type = "h", main = "Cook's Distance", ylab = "Cook's Distance")
abline(h = threshold, col = "red")

# Remove the influential points from the dataset
worker_data_clean <- worker_data[-c(756, 1976, 3693), ]

################################################################################
# Run the regression again without influential points
model_clean <- lm(AIDI ~ Fairness_Index + Autonomy_Index + Trust_AI_Index +
                    sector + employeesex +  employeeagecat2 + education + businesssizebands + employeerole,
                  data = worker_data_clean)

# Summarize new model
summary(model_clean)

################################################################################

# Compare coefficients side by side
stargazer(model_AIDI, model_clean, type = "text", title = "Comparison of Regression Models",
          column.labels = c("Full Model", "Without Influential Points"),
          omit.stat = c("ser", "f"))

# Compare coefficients
coefficients_full <- coef(model_AIDI)
coefficients_clean <- coef(model_clean)

# Create a dataframe for comparison
comparison_df <- data.frame(Full_Model = coefficients_full, Reduced_Model = coefficients_clean)

# Print the comparison
print(comparison_df)

############################################ Display Regression Models ############################################ 

# Display the regression output with renamed variables
stargazer(model_AIDI, model_autonomy, model_trust, model_fairness,
          type = "text",
          title = "Regression Results",
          dep.var.labels = c("AI-Decision Making Index", "Autonomy Index", "Trust in AI Index", "Fairness Index"),
          covariate.labels = c("Manufacturing", "Female", "Gender not specified", 
                               "Age between 25 and 34", "Age between 35 and 49", 
                               "Age between 50 and 64", "Age 65 or above", 
                               "No university degree", "University degree or above",
                               "Business size between 20 to 49", "Business size between 50 to 99",
                               "Business size between 100 to 249", "Business size between 250 to 499",
                               "Business size 500+", "Business size not mentioned",
                               "Professional employee", "Technician and Associate",
                               "Clerical Support", "Service and Sales", 
                               "Craft and Related Trades", "Plant and Machine Operator",
                               "Elementary Occupation", "Other roles", "Employee role not mentioned"),
          omit.stat = c("f", "ser"),
          notes = "This table presents the regression results with renamed variables.")







