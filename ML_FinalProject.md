Obesity_final_project
================
Sanjana Battula, Kajal Gupta, Emily Kasuga
2024-05-01

## Data Cleaning

``` r
# Load Required Packages
library(tidyverse)
library(readr)
library(expss)
library(openxlsx)
library(dplyr)
library(caret)
library(glmnet)
library(leaps)
library(readxl)
library(pROC)
library(ggplot2)
library(kknn)
library(plotmo)
library(tree)
library(ggplot2)
library(knitr)
library(ROCR) 
# Load dataset NYUHDS
# obesity <- read_csv("ObesityDataSet_raw_and_data_sinthetic.csv")
```

This chunk loads the necessary packages required through the analysis.

**Renaming variables - more self explanatory names!**

``` r
#obesity <- obesity %>%
#  rename(fam_history = family_history_with_overweight)

#obesity <- obesity %>%
#  rename(high_cal_freq = FAVC)

#obesity <- obesity %>%
#  rename(veg_intake = FCVC)

#obesity <- obesity %>%
#  rename(daily_meal = NCP)

#obesity <- obesity %>%
#  rename(snack = CAEC)

#obesity <- obesity %>%
#  rename(smoke = SMOKE)

#obesity <- obesity %>%
#  rename(water_intake = CH2O)

#obesity <- obesity %>%
#  rename(monitor_cal = SCC)

#obesity <- obesity %>%
#  rename(exercise = FAF)

#obesity <- obesity %>%
#  rename(tech_use = TUE)

#obesity <- obesity %>%
#  rename(alc_freq = CALC)

#obesity <- obesity %>%
#  rename(transport = MTRANS)

#obesity <- obesity %>%
#  rename(obese = NObeyesdad)
```

This section renames variables in the dataset to more descriptive names
for better understanding.

**Variable Recode: Character to Numeric**

``` r
# Gender Recode (0 - Male, 1 - Female)
#obesity <- obesity %>%
#  mutate(Gender = if_else(Gender == "Male", 0, 1))

# Family History of Obesity Recode (0 - No, 1 - Yes)
#obesity <- obesity %>%
#  mutate(fam_history = if_else(fam_history == "no", 0, 1))

# FAVC Recode - Do you eat high caloric food frequently? (0 - No, 1 - Yes)
#obesity <- obesity %>%
#  mutate(high_cal_freq = if_else(high_cal_freq == "no", 0, 1))

# CAEC Recode - Do you eat any food between meals?
#obesity <- obesity %>%
#  mutate(snack = case_when(
#    snack == "no" ~ 0,
#    snack == "Sometimes" ~ 1,
#    snack == "Frequently" ~ 2,
#    snack  == "Always" ~ 3))

# SMOKE Recode - Do you smoke?
#obesity <- obesity %>%
#  mutate(smoke = if_else(smoke == "no", 0, 1))

# SCC Recode - Do you monitor the calories you eat daily?
#obesity <- obesity %>%
#  mutate(monitor_cal = if_else(monitor_cal == "no", 0, 1))

# CALC Recode - How often do you drink alcohol?
#obesity <- obesity %>%
#  mutate(alc_freq = case_when(
#    alc_freq == "no" ~ 0,
#    alc_freq == "Sometimes" ~ 1,
#    alc_freq == "Frequently" ~ 2,
#    alc_freq  == "Always" ~ 3))

# MTRANS Recode - Which transportation do you usually use?
#obesity <- obesity %>%
#  mutate(transport = case_when(
#    transport == "Automobile" ~ 0,
#    transport == "Motorbike" ~ 1,
#    transport == "Bike" ~ 2,
#    transport  == "Public_Transportation" ~ 3,
#    transport == "Walking" ~ 4))

# NObeyesdad Recode - Obesity Level
#obesity <- obesity %>%
#  mutate(obese = case_when(
#    obese == "Insufficient_Weight" ~ 0,
#    obese == "Normal_Weight" ~ 1,
#    obese == "Overweight_Level_I" ~ 2,
#    obese  == "Overweight_Level_II" ~ 3,
#    obese  == "Obesity_Type_I" ~ 4,
#    obese  == "Obesity_Type_II" ~ 5,
#    obese  == "Obesity_Type_III" ~ 6))
#write_csv(obesity, "Obesity_Data.csv")
```

Converts certain categorical variables from character to numeric format
for analysis.

**New Dataset with rounded values**

``` r
#obesity <- read_csv("Obesity_Data.csv")
# Round the values to the nearest whole number

#obesity$veg_intake <- round(obesity$veg_intake)

#obesity$daily_meal <- round(obesity$daily_meal)

#obesity$water_intake <- round(obesity$water_intake)

#obesity$exercise <- round(obesity$exercise)

#obesity$tech_use <- round(obesity$tech_use)
##### Recode 1 to 0, 2 to 1, 3 to 2, etc. ####
#obesity$veg_intake <- obesity$veg_intake - 1

#obesity$daily_meal <- obesity$daily_meal - 1

#obesity$water_intake <- obesity$water_intake - 1
# Save this dataset: 
#write_csv(obesity, "Obesity_Data_rounded_val.csv")
#obesity_rounded <- read_csv("Obesity_Data_rounded_val.csv")
```

Now , rounding specific numeric variables in the dataset to the nearest
whole number and saves the modified dataset.

**New Dataset with removed values**

``` r
#Load dataset NYUHDS AGAIN (Must do this step before cleaning and removing values)
#obesity <- read_csv("Obesity_Data.csv")
#obesity_removed_values <- obesity %>%
#  mutate(
 #   veg_intake = ifelse((veg_intake > 1 & veg_intake < 2) | (veg_intake > 2 & veg_intake < 3), NA, veg_intake)
 # )
#obesity_removed_values <- obesity_removed_values %>%
#  mutate(
#    daily_meal = ifelse((daily_meal > 1 & daily_meal < 2) | (daily_meal > 2 & daily_meal < 3) | (daily_meal > 3 & daily_meal < 4), NA, daily_meal)
#  )
#obesity_removed_values <- obesity_removed_values %>%
#  mutate(
#    water_intake = ifelse((water_intake > 1 & water_intake < 2) | (water_intake > 2 & water_intake < 3), NA, water_intake)
#  )
#obesity_removed_values <- obesity_removed_values %>%
 # mutate(
#    exercise = ifelse((exercise > 0 & exercise < 1) | (exercise > 1 & exercise < 2) | (exercise > 2 & exercise < 3), NA, exercise)
#  )
#obesity_removed_values <- obesity_removed_values %>%
#  mutate(
#    tech_use = ifelse((tech_use > 0 & tech_use < 1) | (tech_use > 1 & tech_use < 2) | (tech_use > 2 & tech_use < 3), NA, exercise)
#  )
##### Recode 1 to 0, 2 to 1, 3 to 2, etc. ####
#obesity_removed_values$veg_intake <- obesity_removed_values$veg_intake - 1

#obesity_removed_values$daily_meal <- obesity_removed_values$daily_meal - 1

#obesity_removed_values$water_intake <- obesity_removed_values$water_intake - 1
#### Remove NA values ####
#obesity_removed_values <- na.omit(obesity_removed_values)
# Save this dataset: 
#write_csv(obesity_removed_values, "Obesity_Data_removed_val.csv")
## Make outcome into three categories: ##
#Load data
#obesity_rounded <- read_csv("Obesity_Data_rounded_val.csv")
# Preserve cats 0 and 1 - but combine 2,3 and then cmbine 4-6 
#obesity_round <- obesity_rounded %>%
#  mutate(obese_recode = ifelse(obese %in% c(0,1), obese,
#                               ifelse(obese %in% c(2,3), 2,
#                                      ifelse(obese %in% c(4,5,6), 3, NA))))
# Save data 
#write_csv(obesity_round, "Obesity_Data_rounded_val2.csv")
```

Removes certain values from the dataset based on predefined conditions,
rounds numeric variables, and saves the modified dataset.

**Removed values**

``` r
# Load the Data 
#obesity_removed <- read_csv("Obesity_Data_removed_val.csv")
#obesity_removed <- obesity_removed %>%
#  mutate(obese_recode = ifelse(obese %in% c(0,1), obese,
#                               ifelse(obese %in% c(2,3), 2,
#                                      ifelse(obese %in% c(4,5,6), 3, NA))))
# Save data 
#write_csv(obesity_removed, "Obesity_Data_removed_val2.csv")
# Randomize the data to choose 531 observations in teh rounded data: 
#obesity_rounded <- read_csv("Obesity_Data_rounded_val2.csv")
#set.seed(0) # for reproducibility
#random_sample <- obesity_rounded[sample(nrow(obesity_rounded), 531), ]
#write_csv(random_sample, "Obesity_Data_rounded_val_subset.csv")
```

Further processes the dataset by recoding variables and creating a
binary obesity variable. Randomizes the data and selects a subset for
analysis.

**Making Obesity Binary Variable**

``` r
# Make the obesity outcome binary
#obesity_rounded <- read_csv("Obesity_Data_rounded_val2.csv")
#obesity_random <- read_csv("Obesity_Data_rounded_val_subset.csv")
#obesity_removed <- read_csv("Obesity_Data_removed_val2.csv")

#obesity_rounded <- obesity_rounded %>%
#  mutate(binary_obese = ifelse(obese_recode %in% c(0, 1), 0,
#                               ifelse(obese_recode %in% c(2, 3), 1, #NA)))
#write_csv(obesity_rounded, "Obesity_Data_rounded_val2.csv")
#obesity_random <- obesity_random %>%
#  mutate(binary_obese = ifelse(obese_recode %in% c(0, 1), 0,
#                               ifelse(obese_recode %in% c(2, 3), 1, #NA)))
#write_csv(obesity_random, "Obesity_Data_rounded_val_subset.csv")
#obesity_removed <- obesity_removed %>%
#  mutate(binary_obese = ifelse(obese_recode %in% c(0, 1), 0,
#                               ifelse(obese_recode %in% c(2, 3), 1, NA)))
#write_csv(obesity_removed, "Obesity_Data_removed_val2.csv")
```

Creates a binary variable for obesity based on the recoded obesity
levels in the dataset.

## Features Selections

### ROUNDED FULL DATA

**Splitting The Dataset**

``` r
# Load the dataset
obesity_data_rounded <- read.csv("Obesity_Data_rounded_val2.csv")
obesity_data_rounded <- obesity_data_rounded %>% select(-obese ,-Weight ,-obese_recode)
# Set the seed for reproducibility
set.seed(0)
# Split the dataset into training and test sets (80:20)
train_indices_rounded <- sample(1:nrow(obesity_data_rounded), 0.8 * nrow(obesity_data_rounded))
tr_data_round <- obesity_data_rounded[train_indices_rounded, ]
te_data_round <- obesity_data_rounded[-train_indices_rounded, ]
```

This code chunk loads the dataset - Rounded Full data and removes some
columns (obese, Weight, and obese_recode). It then sets a random seed
for reproducibility and splits the dataset into training (80%) and test
(20%) sets.

**Ridge Regression with 5 fold CV**

``` r
# Perform Ridge Regression with 5-fold CV
set.seed(0)
k_tr_round <- as.matrix(tr_data_round[, -ncol(tr_data_round)])
s_tr_round <- tr_data_round[, ncol(tr_data_round), drop = T]
k_te_round <- as.matrix(te_data_round[, -ncol(te_data_round)])
s_te_round <- te_data_round[, ncol(te_data_round), drop = T]
std_fit_rounded <- preProcess(k_tr_round, method = c("center", "scale"))
k_tr_std_round <- predict(std_fit_rounded, k_tr_round)
k_te_std_round <- predict(std_fit_rounded, k_te_round)

# Train Ridge model with cross-validation
cv_fit_ridge_rd <- cv.glmnet(k_tr_std_round, s_tr_round, alpha = 0, nfolds = 5)
# Get coefficients
ridge_coef_round <- coef(cv_fit_ridge_rd, s = cv_fit_ridge_rd$lambda.min)
# Predict on training data
ridge_tr_pred_rd <- predict(cv_fit_ridge_rd, newx = k_tr_std_round)
# Calculate training error
ridge_train_error_rd <- mean((ridge_tr_pred_rd - s_tr_round)^2)
# Predict on test data
ridge_te_pred_rd <- predict(cv_fit_ridge_rd, newx = k_te_std_round)
# Calculate test error
ridge_test_error_rd <- mean((ridge_te_pred_rd - s_te_round)^2)
# Display results
ridge_results_rd <- data.frame(Method = "Ridge Regression with 5-fold CV",
                            Coefficients = as.numeric(ridge_coef_round),
                            Test_Error = ridge_test_error_rd,
                            Train_Error = ridge_train_error_rd)
# Print the table
print(ridge_results_rd)
```

    ##                             Method Coefficients Test_Error Train_Error
    ## 1  Ridge Regression with 5-fold CV  0.732227488  0.1110041   0.1122895
    ## 2  Ridge Regression with 5-fold CV -0.012068690  0.1110041   0.1122895
    ## 3  Ridge Regression with 5-fold CV  0.123443631  0.1110041   0.1122895
    ## 4  Ridge Regression with 5-fold CV  0.015399467  0.1110041   0.1122895
    ## 5  Ridge Regression with 5-fold CV  0.147679679  0.1110041   0.1122895
    ## 6  Ridge Regression with 5-fold CV  0.029332492  0.1110041   0.1122895
    ## 7  Ridge Regression with 5-fold CV -0.001251852  0.1110041   0.1122895
    ## 8  Ridge Regression with 5-fold CV -0.036849586  0.1110041   0.1122895
    ## 9  Ridge Regression with 5-fold CV -0.135330678  0.1110041   0.1122895
    ## 10 Ridge Regression with 5-fold CV -0.015840627  0.1110041   0.1122895
    ## 11 Ridge Regression with 5-fold CV  0.019469547  0.1110041   0.1122895
    ## 12 Ridge Regression with 5-fold CV  0.004297047  0.1110041   0.1122895
    ## 13 Ridge Regression with 5-fold CV -0.045284374  0.1110041   0.1122895
    ## 14 Ridge Regression with 5-fold CV -0.001189448  0.1110041   0.1122895
    ## 15 Ridge Regression with 5-fold CV  0.038654688  0.1110041   0.1122895
    ## 16 Ridge Regression with 5-fold CV  0.052640707  0.1110041   0.1122895

Here Ridge Regression with 5-fold cross-validation is performed on the
training data. It calculates the coefficients, training error, and test
error for the Ridge Regression model and displays the results in a
table. Training Error:0.112 Testing Error:0.111

**Ridge Solution Path**

``` r
fit_ridge_round <- glmnet(k_tr_std_round, s_tr_round, alpha = 0)
plot_glmnet(fit_ridge_round)
```

![](<figures/unnamed-chunk-10-1.png>)
In this chunk a Ridge Regression model is fitted on the standardized
training data and plots the solution path, which shows the coefficients
for different values of the regularization parameter (lambda).

**Lasso Regression With 5 fold CV**

``` r
# Perform Lasso Regression with 5-fold CV
set.seed(0)
lasso_model_round <- cv.glmnet(k_tr_round, s_tr_round, alpha = 1, nfolds = 5)
lasso_coef_rd <- coef(lasso_model_round, s = lasso_model_round$lambda.min)
lasso_tr_pred_rd <- predict(lasso_model_round, newx = k_tr_round)
lasso_train_error_rd <- mean((lasso_tr_pred_rd - s_tr_round)^2)
lasso_te_pred_rd <- predict(lasso_model_round, newx = k_te_round)
lasso_test_error_rd <- mean((lasso_te_pred_rd - s_te_round)^2)
lasso_results_rd <- data.frame(Method = "Lasso Regression with 5-fold CV",
                           Coefficients = as.numeric(lasso_coef_rd),
                           Test_Error = lasso_test_error_rd,
                           Train_Error = lasso_train_error_rd)
#Print the table
print(lasso_results_rd)
```

    ##                             Method Coefficients Test_Error Train_Error
    ## 1  Lasso Regression with 5-fold CV  -0.08503072  0.1090025   0.1130901
    ## 2  Lasso Regression with 5-fold CV  -0.02235052  0.1090025   0.1130901
    ## 3  Lasso Regression with 5-fold CV   0.01987609  0.1090025   0.1130901
    ## 4  Lasso Regression with 5-fold CV   0.13605831  0.1090025   0.1130901
    ## 5  Lasso Regression with 5-fold CV   0.39016324  0.1090025   0.1130901
    ## 6  Lasso Regression with 5-fold CV   0.08240344  0.1090025   0.1130901
    ## 7  Lasso Regression with 5-fold CV   0.00000000  0.1090025   0.1130901
    ## 8  Lasso Regression with 5-fold CV  -0.04348883  0.1090025   0.1130901
    ## 9  Lasso Regression with 5-fold CV  -0.29048801  0.1090025   0.1130901
    ## 10 Lasso Regression with 5-fold CV  -0.09679782  0.1090025   0.1130901
    ## 11 Lasso Regression with 5-fold CV   0.02462740  0.1090025   0.1130901
    ## 12 Lasso Regression with 5-fold CV   0.01192794  0.1090025   0.1130901
    ## 13 Lasso Regression with 5-fold CV  -0.04823515  0.1090025   0.1130901
    ## 14 Lasso Regression with 5-fold CV   0.00000000  0.1090025   0.1130901
    ## 15 Lasso Regression with 5-fold CV   0.07426518  0.1090025   0.1130901
    ## 16 Lasso Regression with 5-fold CV   0.04343052  0.1090025   0.1130901

This code chunk performs Lasso Regression with 5-fold cross-validation
on the training data. It calculates the coefficients, training error,
and test error for the Lasso Regression model and displays the results
in a table. Training Error:0.113 Testing Error:0.109

**lasso Solution Path**

``` r
fit_lasso_round <- glmnet(k_tr_std_round, s_tr_round)
plot_glmnet(fit_lasso_round)
```

![](<figures/unnamed-chunk-12-1.png>) 
In this chunk a Lasso Regression model is fitted on the standardized
training data and plots the solution path, which shows the coefficients
for different values of the regularization parameter (lambda).
**Features Selected : Family history, Age, Transport, Alcohol
frequencies, High Calorie frequency , Water intake , Smoke, Daily meal,
Exercise, Snack**

### ROUNDED SUBSET DATA

**Splitting The Dataset**

``` r
# Load the dataset
obesity_data_round.subset <- read.csv("Obesity_Data_rounded_val_subset.csv")
obesity_data_round.subset <- obesity_data_round.subset %>% select(-obese , -Weight , -obese_recode)
# Set the seed for reproducibility
set.seed(0)
# Split the dataset into training and test sets (80:20)
tr_indi <- sample(1:nrow(obesity_data_round.subset), 0.8 * nrow(obesity_data_round.subset))
tr_dat <- obesity_data_round.subset[tr_indi, ]
te_dat <- obesity_data_round.subset[-tr_indi, ]
```

This code chunk loads the dataset Rounded Subset Data and removes some
columns (obese, Weight, and obese_recode). It then sets a random seed
for reproducibility and splits the dataset into training (80%) and test
(20%) sets.

**Ridge Reegression with 5 fold CV**

``` r
# Perform Ridge Regression with 5-fold CV
set.seed(0)
e_tr_sub <- as.matrix(tr_dat[, -ncol(tr_dat)])
f_tr_sub <- tr_dat[, ncol(tr_dat), drop = T]
e_te_sub <- as.matrix(te_dat[, -ncol(te_dat)])
f_te_sub <- te_dat[, ncol(te_dat), drop = T]
std_fit_sub <- preProcess(e_tr_sub, method = c("center", "scale"))
e_tr_std_sub <- predict(std_fit_sub, e_tr_sub)
e_te_std_sub <- predict(std_fit_sub, e_te_sub)

# Train Ridge model with cross-validation
cv_fit_ridge_sub <- cv.glmnet(e_tr_std_sub, f_tr_sub, alpha = 0, nfolds = 5)
# Get coefficients
ridge_coef_sub <- coef(cv_fit_ridge_sub, s = cv_fit_ridge_sub$lambda.min)
# Predict on training data
ridge_tr_pred_sub <- predict(cv_fit_ridge_sub, newx = e_tr_std_sub)
# Calculate training error
ridge_train_error_sub <- mean((ridge_tr_pred_sub - f_tr_sub)^2)
# Predict on test data
ridge_te_pred_sub <- predict(cv_fit_ridge_sub, newx = e_te_std_sub)
# Calculate test error
ridge_test_error_sub <- mean((ridge_te_pred_sub - f_te_sub)^2)
# Display results
ridge_results_sub <- data.frame(Method = "Ridge Regression with 5-fold CV - Rounded Subset",
                            Coefficients = as.numeric(ridge_coef_sub),
                            Test_Error = ridge_test_error_sub,
                            Train_Error = ridge_train_error_sub)
# Print the table
print(ridge_results_sub)
```

    ##                                              Method Coefficients Test_Error
    ## 1  Ridge Regression with 5-fold CV - Rounded Subset   0.74764151  0.1573882
    ## 2  Ridge Regression with 5-fold CV - Rounded Subset  -0.01148122  0.1573882
    ## 3  Ridge Regression with 5-fold CV - Rounded Subset   0.09020807  0.1573882
    ## 4  Ridge Regression with 5-fold CV - Rounded Subset   0.01068697  0.1573882
    ## 5  Ridge Regression with 5-fold CV - Rounded Subset   0.13739070  0.1573882
    ## 6  Ridge Regression with 5-fold CV - Rounded Subset   0.03027447  0.1573882
    ## 7  Ridge Regression with 5-fold CV - Rounded Subset  -0.01048402  0.1573882
    ## 8  Ridge Regression with 5-fold CV - Rounded Subset  -0.02085176  0.1573882
    ## 9  Ridge Regression with 5-fold CV - Rounded Subset  -0.12185878  0.1573882
    ## 10 Ridge Regression with 5-fold CV - Rounded Subset  -0.02238769  0.1573882
    ## 11 Ridge Regression with 5-fold CV - Rounded Subset   0.02106155  0.1573882
    ## 12 Ridge Regression with 5-fold CV - Rounded Subset  -0.01521174  0.1573882
    ## 13 Ridge Regression with 5-fold CV - Rounded Subset  -0.03884543  0.1573882
    ## 14 Ridge Regression with 5-fold CV - Rounded Subset   0.01244390  0.1573882
    ## 15 Ridge Regression with 5-fold CV - Rounded Subset   0.02441781  0.1573882
    ## 16 Ridge Regression with 5-fold CV - Rounded Subset   0.02407693  0.1573882
    ##    Train_Error
    ## 1    0.1076455
    ## 2    0.1076455
    ## 3    0.1076455
    ## 4    0.1076455
    ## 5    0.1076455
    ## 6    0.1076455
    ## 7    0.1076455
    ## 8    0.1076455
    ## 9    0.1076455
    ## 10   0.1076455
    ## 11   0.1076455
    ## 12   0.1076455
    ## 13   0.1076455
    ## 14   0.1076455
    ## 15   0.1076455
    ## 16   0.1076455

Here Ridge Regression with 5-fold cross-validation is performed on the
training data. It calculates the coefficients, training error, and test
error for the Ridge Regression model and displays the results in a
table. Training Error:0.108 Testing Error:0.158

**Ridge Solution Path**

``` r
fit_ridge.sub <- glmnet(e_tr_std_sub, f_tr_sub, alpha = 0)
plot_glmnet(fit_ridge.sub)
```

![](<figures/unnamed-chunk-15-1.png>)
In this chunk a Ridge Regression model is fitted on the standardized
training data and plots the solution path, which shows the coefficients
for different values of the regularization parameter (lambda).

**Lasso Regression With 5 fold CV**

``` r
# Perform Lasso Regression with 10-fold CV
set.seed(0)
lasso_model_sub <- cv.glmnet(e_tr_sub, f_tr_sub, alpha = 1, nfolds = 5)
lasso_coef_sub <- coef(lasso_model_sub, s = lasso_model_sub$lambda.min)
lasso_tr_pred_sub <- predict(lasso_model_sub, newx = e_tr_sub)
lasso_train_error_sub <- mean((lasso_tr_pred_sub - f_tr_sub)^2)
lasso_te_pred_sub <- predict(lasso_model_sub, newx = e_te_sub)
lasso_test_error_sub <- mean((lasso_te_pred_sub - f_te_sub)^2)
lasso_results_sub <- data.frame(Method = "Lasso Regression with 5-fold CV - Rounded Subset",
                           Coefficients = as.numeric(lasso_coef_sub),
                           Test_Error = lasso_test_error_sub,
                           Train_Error = lasso_train_error_sub)
#Print the table
print(lasso_results_sub)
```

    ##                                              Method Coefficients Test_Error
    ## 1  Lasso Regression with 5-fold CV - Rounded Subset  0.367325345  0.1715808
    ## 2  Lasso Regression with 5-fold CV - Rounded Subset -0.014370263  0.1715808
    ## 3  Lasso Regression with 5-fold CV - Rounded Subset  0.013954993  0.1715808
    ## 4  Lasso Regression with 5-fold CV - Rounded Subset  0.000000000  0.1715808
    ## 5  Lasso Regression with 5-fold CV - Rounded Subset  0.416761092  0.1715808
    ## 6  Lasso Regression with 5-fold CV - Rounded Subset  0.076226058  0.1715808
    ## 7  Lasso Regression with 5-fold CV - Rounded Subset  0.000000000  0.1715808
    ## 8  Lasso Regression with 5-fold CV - Rounded Subset -0.012095686  0.1715808
    ## 9  Lasso Regression with 5-fold CV - Rounded Subset -0.328648196  0.1715808
    ## 10 Lasso Regression with 5-fold CV - Rounded Subset -0.110685658  0.1715808
    ## 11 Lasso Regression with 5-fold CV - Rounded Subset  0.012120016  0.1715808
    ## 12 Lasso Regression with 5-fold CV - Rounded Subset -0.036442146  0.1715808
    ## 13 Lasso Regression with 5-fold CV - Rounded Subset -0.038410861  0.1715808
    ## 14 Lasso Regression with 5-fold CV - Rounded Subset  0.003838759  0.1715808
    ## 15 Lasso Regression with 5-fold CV - Rounded Subset  0.037953170  0.1715808
    ## 16 Lasso Regression with 5-fold CV - Rounded Subset  0.012489841  0.1715808
    ##    Train_Error
    ## 1    0.1109308
    ## 2    0.1109308
    ## 3    0.1109308
    ## 4    0.1109308
    ## 5    0.1109308
    ## 6    0.1109308
    ## 7    0.1109308
    ## 8    0.1109308
    ## 9    0.1109308
    ## 10   0.1109308
    ## 11   0.1109308
    ## 12   0.1109308
    ## 13   0.1109308
    ## 14   0.1109308
    ## 15   0.1109308
    ## 16   0.1109308

This code chunk performs Lasso Regression with 5-fold cross-validation
on the training data. It calculates the coefficients, training error,
and test error for the Lasso Regression model and displays the results
in a table. Training Error:0.111 Testing Error:0.172

**lasso Solution Path**

``` r
fit_lasso.sub <- glmnet(e_tr_std_sub, f_tr_sub)
plot_glmnet(fit_lasso.sub)
```

![](<figures/unnamed-chunk-17-1.png>)
In this chunk a Lasso Regression model is fitted on the standardized
training data and plots the solution path, which shows the coefficients
for different values of the regularization parameter (lambda).
**Features Selected : Age, Family history , Tech_use , Alcohol frequency
, Transport , Snack , Smoke , Exercise, High calorie frequency , Daily
meal**

### REMOVED DATASET

**Splitting The Dataset**

``` r
# Load the dataset
obesity_data_removed <- read.csv("Obesity_Data_removed_val2.csv")
obesity_data_removed <- obesity_data_removed %>% select(-obese ,-Weight ,-obese_recode)
# Set the seed for reproducibility
set.seed(0)
# Split the dataset into training and test sets (80:20)
train_indices <- sample(1:nrow(obesity_data_removed), 0.8 * nrow(obesity_data_removed))
tr_data <- obesity_data_removed[train_indices, ]
te_data <- obesity_data_removed[-train_indices, ]
```

This code chunk loads the dataset Removed Data and removes some columns
(obese, Weight, and obese_recode). It then sets a random seed for
reproducibility and splits the dataset into training (80%) and test
(20%) sets.

**Ridge Reegression with 5 fold CV**

``` r
# Perform Ridge Regression with 5-fold CV
set.seed(0)
x_tr_rem <- as.matrix(tr_data[, -ncol(tr_data)])
y_tr_rem <- tr_data[, ncol(tr_data), drop = TRUE]
x_te_rem <- as.matrix(te_data[, -ncol(te_data)])
y_te_rem <- te_data[, ncol(te_data), drop = TRUE]
std_fit_rem <- preProcess(x_tr_rem, method = c("center", "scale"))
x_tr_std_rem <- predict(std_fit_rem, x_tr_rem)
x_te_std_rem <- predict(std_fit_rem, x_te_rem)
# Train Ridge model with cross-validation
cv_fit_ridge_rem <- cv.glmnet(x_tr_std_rem, y_tr_rem, alpha = 0, nfolds = 5)
# Get coefficients
ridge_coef_rem <- coef(cv_fit_ridge_rem, s = cv_fit_ridge_rem$lambda.min)
# Predict on training data
ridge_tr_pred_rem <- predict(cv_fit_ridge_rem, newx = x_tr_std_rem)
# Calculate training error
ridge_train_error_rem <- mean((ridge_tr_pred_rem - y_tr_rem)^2)
# Predict on test data
ridge_te_pred_rem <- predict(cv_fit_ridge_rem, newx = x_te_std_rem)
# Calculate test error
ridge_test_error_rem <- mean((ridge_te_pred_rem - y_te_rem)^2)
# Display results
ridge_results_rem <- data.frame(Method = "Ridge Regression with 5-fold CV - Removed Data",
                            Coefficients = as.numeric(ridge_coef_rem),
                            Test_Error = ridge_test_error_rem,
                            Train_Error = ridge_train_error_rem)
# Print the table
print(ridge_results_rem)
```

    ##                                            Method Coefficients Test_Error
    ## 1  Ridge Regression with 5-fold CV - Removed Data  0.393867925  0.1906612
    ## 2  Ridge Regression with 5-fold CV - Removed Data -0.038665428  0.1906612
    ## 3  Ridge Regression with 5-fold CV - Removed Data  0.110132525  0.1906612
    ## 4  Ridge Regression with 5-fold CV - Removed Data  0.018432510  0.1906612
    ## 5  Ridge Regression with 5-fold CV - Removed Data  0.054445451  0.1906612
    ## 6  Ridge Regression with 5-fold CV - Removed Data  0.009145526  0.1906612
    ## 7  Ridge Regression with 5-fold CV - Removed Data -0.048998911  0.1906612
    ## 8  Ridge Regression with 5-fold CV - Removed Data -0.055372480  0.1906612
    ## 9  Ridge Regression with 5-fold CV - Removed Data -0.048760999  0.1906612
    ## 10 Ridge Regression with 5-fold CV - Removed Data  0.016848182  0.1906612
    ## 11 Ridge Regression with 5-fold CV - Removed Data  0.055291259  0.1906612
    ## 12 Ridge Regression with 5-fold CV - Removed Data  0.009403867  0.1906612
    ## 13 Ridge Regression with 5-fold CV - Removed Data -0.025655204  0.1906612
    ## 14 Ridge Regression with 5-fold CV - Removed Data -0.025414484  0.1906612
    ## 15 Ridge Regression with 5-fold CV - Removed Data  0.018137402  0.1906612
    ## 16 Ridge Regression with 5-fold CV - Removed Data -0.030493345  0.1906612
    ##    Train_Error
    ## 1     0.196608
    ## 2     0.196608
    ## 3     0.196608
    ## 4     0.196608
    ## 5     0.196608
    ## 6     0.196608
    ## 7     0.196608
    ## 8     0.196608
    ## 9     0.196608
    ## 10    0.196608
    ## 11    0.196608
    ## 12    0.196608
    ## 13    0.196608
    ## 14    0.196608
    ## 15    0.196608
    ## 16    0.196608

Here Ridge Regression with 5-fold cross-validation is performed on the
training data. It calculates the coefficients, training error, and test
error for the Ridge Regression model and displays the results in a
table. Training Error:0.197 Testing Error:0.191

**Ridge Solution Path**

``` r
fit_ridge.rem <- glmnet(x_tr_std_rem, y_tr_rem, alpha = 0)
plot_glmnet(fit_ridge.rem)
```

![](<figures/unnamed-chunk-20-1.png>)
In this chunk a Ridge Regression model is fitted on the standardized
training data and plots the solution path, which shows the coefficients
for different values of the regularization parameter (lambda).

**Lasso Regression With 5 fold CV**

``` r
# Perform Lasso Regression with 5-fold CV
set.seed(0)
lasso_model_rem <- cv.glmnet(x_tr_rem, y_tr_rem, alpha = 1, nfolds = 5)
lasso_coef_rem <- coef(lasso_model_rem, s = lasso_model_rem$lambda.min)
lasso_tr_pred_rem <- predict(lasso_model_rem, newx = x_tr_rem)
lasso_train_error_rem <- mean((lasso_tr_pred_rem - y_tr_rem)^2)
lasso_te_pred_rem <- predict(lasso_model_rem, newx = x_te_rem)
lasso_test_error_rem <- mean((lasso_te_pred_rem - y_te_rem)^2)
lasso_results_rem <- data.frame(Method = "Lasso Regression with 5-fold CV - Removed Data",
                           Coefficients = as.numeric(lasso_coef_rem),
                           Test_Error = lasso_test_error_rem,
                           Train_Error = lasso_train_error_rem)
# Print the table
print(lasso_results_rem)
```

    ##                                            Method  Coefficients Test_Error
    ## 1  Lasso Regression with 5-fold CV - Removed Data  4.009315e-02  0.1879905
    ## 2  Lasso Regression with 5-fold CV - Removed Data -8.594402e-02  0.1879905
    ## 3  Lasso Regression with 5-fold CV - Removed Data  1.819249e-02  0.1879905
    ## 4  Lasso Regression with 5-fold CV - Removed Data  1.090002e-01  0.1879905
    ## 5  Lasso Regression with 5-fold CV - Removed Data  1.165743e-01  0.1879905
    ## 6  Lasso Regression with 5-fold CV - Removed Data  8.037419e-03  0.1879905
    ## 7  Lasso Regression with 5-fold CV - Removed Data -8.839264e-02  0.1879905
    ## 8  Lasso Regression with 5-fold CV - Removed Data -6.094961e-02  0.1879905
    ## 9  Lasso Regression with 5-fold CV - Removed Data -6.423712e-02  0.1879905
    ## 10 Lasso Regression with 5-fold CV - Removed Data  4.314193e-02  0.1879905
    ## 11 Lasso Regression with 5-fold CV - Removed Data  8.428470e-02  0.1879905
    ## 12 Lasso Regression with 5-fold CV - Removed Data  7.373165e-03  0.1879905
    ## 13 Lasso Regression with 5-fold CV - Removed Data -4.447195e-02  0.1879905
    ## 14 Lasso Regression with 5-fold CV - Removed Data -6.543507e-16  0.1879905
    ## 15 Lasso Regression with 5-fold CV - Removed Data  2.176809e-02  0.1879905
    ## 16 Lasso Regression with 5-fold CV - Removed Data -1.619064e-02  0.1879905
    ##    Train_Error
    ## 1    0.1948304
    ## 2    0.1948304
    ## 3    0.1948304
    ## 4    0.1948304
    ## 5    0.1948304
    ## 6    0.1948304
    ## 7    0.1948304
    ## 8    0.1948304
    ## 9    0.1948304
    ## 10   0.1948304
    ## 11   0.1948304
    ## 12   0.1948304
    ## 13   0.1948304
    ## 14   0.1948304
    ## 15   0.1948304
    ## 16   0.1948304

This code chunk performs Lasso Regression with 5-fold cross-validation
on the training data. It calculates the coefficients, training error,
and test error for the Lasso Regression model and displays the results
in a table. Training Error:0.195 Testing Error:0.188

**lasso Solution Path**

``` r
fit_lasso.rem <- glmnet(x_tr_std_rem, y_tr_rem)
plot_glmnet(fit_lasso.rem)
```

![](<figures/unnamed-chunk-22-1.png>)
In this chunk a Lasso Regression model is fitted on the standardized
training data and plots the solution path, which shows the coefficients
for different values of the regularization parameter (lambda).
**Features Selected : Gender , Age, Height , Family history , Water
intake , Exercise , Daily meal , Snack , Transport , Veg intake**

*Comparison Tables For Errors For ALL Dataset*

``` r
# Combine the results from all three comparison tables
Results_combined <- rbind(
  data.frame(Dataset_Type = rep("Rounded Full Data", 2),
             Model = c("Ridge Regression (CV)", "Lasso (CV)"),
             Train_Error = c(ridge_train_error_rd, lasso_train_error_rd),
             Test_Error = c(ridge_test_error_rd, lasso_test_error_rd)),
  data.frame(Dataset_Type = rep("Rounded Subset", 2),
             Model = c("Ridge Regression (CV)", "Lasso (CV)"),
             Train_Error = c(ridge_train_error_sub, lasso_train_error_sub),
             Test_Error = c(ridge_test_error_sub, lasso_test_error_sub)),
  data.frame(Dataset_Type = rep("Removed Data", 2),
             Model = c("Ridge Regression (CV)", "Lasso (CV)"),
             Train_Error = c(ridge_train_error_rem, lasso_train_error_rem),
             Test_Error = c(ridge_test_error_rem, lasso_test_error_rem))
)

# Print the combined results table
kable(Results_combined, format = "markdown", caption = "Feature Selection Errors Combined For All Dataset")
```

| Dataset_Type      | Model                 | Train_Error | Test_Error |
|:------------------|:----------------------|------------:|-----------:|
| Rounded Full Data | Ridge Regression (CV) |   0.1122895 |  0.1110041 |
| Rounded Full Data | Lasso (CV)            |   0.1130901 |  0.1090025 |
| Rounded Subset    | Ridge Regression (CV) |   0.1076455 |  0.1573882 |
| Rounded Subset    | Lasso (CV)            |   0.1109308 |  0.1715808 |
| Removed Data      | Ridge Regression (CV) |   0.1966080 |  0.1906612 |
| Removed Data      | Lasso (CV)            |   0.1948304 |  0.1879905 |

Feature Selection Errors Combined For All Dataset

Combining the training and test errors for Ridge Regression and Lasso
Regression models from all three datasets (rounded full data, rounded
subset, and removed data) into a single table. It uses the kable
function from the knitr package to display the table in a markdown
format. The table shows the training and test errors for each model and
dataset, allowing for easy comparison across different scenarios.

## Model Selection

### ROUNDED FULL DATA

**Logistic Regression Model**

``` r
# Fit logistic regression model for obesity_data_rounded
fit_mod_rounded <-  glm(binary_obese ~ Age + fam_history + high_cal_freq + water_intake + alc_freq + daily_meal + snack + transport + exercise + smoke,data = tr_data_round, family = "binomial")
# Summary of the model
summary(fit_mod_rounded)
```

    ## 
    ## Call:
    ## glm(formula = binary_obese ~ Age + fam_history + high_cal_freq + 
    ##     water_intake + alc_freq + daily_meal + snack + transport + 
    ##     exercise + smoke, family = "binomial", data = tr_data_round)
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   -3.79693    0.64897  -5.851 4.90e-09 ***
    ## Age            0.19743    0.01937  10.193  < 2e-16 ***
    ## fam_history    2.45685    0.19134  12.840  < 2e-16 ***
    ## high_cal_freq  0.78930    0.21683   3.640 0.000272 ***
    ## water_intake   0.20657    0.11914   1.734 0.082951 .  
    ## alc_freq       0.54336    0.14710   3.694 0.000221 ***
    ## daily_meal    -0.30704    0.09950  -3.086 0.002030 ** 
    ## snack         -2.10621    0.17346 -12.142  < 2e-16 ***
    ## transport      0.26473    0.07650   3.460 0.000539 ***
    ## exercise      -0.40005    0.08879  -4.505 6.62e-06 ***
    ## smoke         -0.75125    0.54470  -1.379 0.167834    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1961.6  on 1687  degrees of freedom
    ## Residual deviance: 1114.9  on 1677  degrees of freedom
    ## AIC: 1136.9
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
# Predict outcomes for the training dataset
train_pred_rounded <- predict(fit_mod_rounded, newdata = tr_data_round, type = "response")
# Predict outcomes for the testing dataset
test_pred_rounded <- predict(fit_mod_rounded, newdata = te_data_round, type = "response")
# Convert probabilities to binary predictions using a threshold of 0.5
train_pred_binary_rounded <- ifelse(train_pred_rounded >= 0.5, 1, 0)
test_pred_binary_rounded <- ifelse(test_pred_rounded >= 0.5, 1, 0)
# Calculate training error
train_error_rounded <- mean(train_pred_binary_rounded != tr_data_round$binary_obese)
# Calculate testing error
test_error_rounded <- mean(test_pred_binary_rounded != te_data_round$binary_obese)
# Print errors
print(paste("Training Error Rounded Dataset:", train_error_rounded))
```

    ## [1] "Training Error Rounded Dataset: 0.117298578199052"

``` r
print(paste("Testing Error Rounded Dataset:", test_error_rounded))
```

    ## [1] "Testing Error Rounded Dataset: 0.120567375886525"

In chunck above a logistic regression model is fitted on the rounded
full dataset using the selected features. It predicts the outcomes for
both the training and testing datasets and converts the predicted
probabilities to binary predictions using a threshold of 0.5. It then
calculates and prints the training and testing errors for the logistic
regression model on the rounded full dataset. Training Error :0.117
Testing Error :0.120

**Decision Tree**

**Decision Tree Cross-Validation**

``` r
ob.tree <- tree(binary_obese ~ Age + water_intake + transport + fam_history + exercise + snack + daily_meal + alc_freq + high_cal_freq +smoke, data = obesity_data_rounded)
set.seed(0)
cv.ob <- cv.tree(ob.tree)
#names(cv.sal)
cv.ob_df <- data.frame(size = cv.ob$size, deviance = cv.ob$dev)
best_size <- cv.ob$size[which.min(cv.ob$dev)]

ggplot(cv.ob_df, mapping = aes(x = size, y = deviance)) + 
  geom_point(size = 3) + 
  geom_line() +
  geom_vline(xintercept = best_size, col = "firebrick1")
```

![](<figures/unnamed-chunk-25-1.png>)
This code chunk builds a decision tree model on the rounded full dataset
using the selected features. It performs cross-validation on the
decision tree model to determine the optimal size (number of terminal
nodes) that minimizes the deviance (error). The code then plots the
deviance against the number of terminal nodes, with a vertical line
indicating the optimal size. This plot helps visualize the process of
finding the optimal tree size through cross-validation.

**Visualizing the pruned classification tree**

``` r
ob.tree.final <- prune.tree(ob.tree, best = best_size) #The subtree with best_size terminal nodes
plot(ob.tree.final)
text(ob.tree.final, cex=0.7 ,adj=1)
```

![](<figures/unnamed-chunk-26-1.png>)

``` r
# Compute training and test errors for Full Data
train_error_tree_full <- mean((predict(ob.tree.final, newdata = tr_data_round) - tr_data_round$binary_obese)^2)
test_error_tree_full <- mean((predict(ob.tree.final, newdata = te_data_round) - te_data_round$binary_obese)^2)
#Print the results 
print(paste( "Training Error Full Dataset:",train_error_tree_full))
```

    ## [1] "Training Error Full Dataset: 0.0939743411028188"

``` r
print(paste( "Testing Error Full Dataset:",test_error_tree_full))
```

    ## [1] "Testing Error Full Dataset: 0.0921094651726222"

Pruning the decision tree model to the optimal size determined by
cross-validation and plots the pruned tree. It then computes and prints
the training and testing errors for the pruned decision tree model on
the rounded full dataset. Training Error : 0.094 Testing Error : 0.0921

### ROUNDED SUBSET

**Logistic Regression Model**

``` r
# Fit logistic regression model for obesity_data_round.subset
fit_mod_subset <- glm(binary_obese ~ Age + fam_history + tech_use + alc_freq + transport + snack + smoke + exercise + high_cal_freq + daily_meal, family = binomial, data = tr_dat)
# Summary of the model
summary(fit_mod_subset)
```

    ## 
    ## Call:
    ## glm(formula = binary_obese ~ Age + fam_history + tech_use + alc_freq + 
    ##     transport + snack + smoke + exercise + high_cal_freq + daily_meal, 
    ##     family = binomial, data = tr_dat)
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   -4.44940    1.45208  -3.064  0.00218 ** 
    ## Age            0.23374    0.04576   5.108 3.26e-07 ***
    ## fam_history    3.01022    0.41149   7.315 2.57e-13 ***
    ## tech_use       0.33214    0.26271   1.264  0.20613    
    ## alc_freq       0.57676    0.31043   1.858  0.06318 .  
    ## transport      0.18406    0.16194   1.137  0.25570    
    ## snack         -2.72999    0.41867  -6.521 7.00e-11 ***
    ## smoke         -1.33030    1.01879  -1.306  0.19163    
    ## exercise      -0.48349    0.18465  -2.618  0.00884 ** 
    ## high_cal_freq  1.16135    0.45681   2.542  0.01101 *  
    ## daily_meal    -0.18677    0.21039  -0.888  0.37469    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 479.04  on 423  degrees of freedom
    ## Residual deviance: 248.88  on 413  degrees of freedom
    ## AIC: 270.88
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
# Predict outcomes for the training dataset
train_pred_subset <- predict(fit_mod_subset, newdata = tr_dat, type = "response")
# Predict outcomes for the testing dataset
test_pred_subset <- predict(fit_mod_subset, newdata = te_dat, type = "response")
# Convert probabilities to binary predictions using a threshold of 0.5
train_pred_binary_subset <- ifelse(train_pred_subset >= 0.5, 1, 0)
test_pred_binary_subset <- ifelse(test_pred_subset >= 0.5, 1, 0)
# Calculate training error
train_error_subset <- mean(train_pred_binary_subset != tr_dat$binary_obese)
# Calculate testing error
test_error_subset <- mean(test_pred_binary_subset != te_dat$binary_obese)
# Print errors
print(paste("Training Error Subset Dataset:", train_error_subset))
```

    ## [1] "Training Error Subset Dataset: 0.10377358490566"

``` r
print(paste("Testing Error Subset Dataset:", test_error_subset))
```

    ## [1] "Testing Error Subset Dataset: 0.14018691588785"

In chunck above a logistic regression model is fitted on the rounded
subset dataset using the selected features. It predicts the outcomes for
both the training and testing datasets and converts the predicted
probabilities to binary predictions using a threshold of 0.5. It then
calculates and prints the training and testing errors for the logistic
regression model on the rounded subset dataset. training Error:0.104
Testing Error :0.140

**Decision Tree**

**Decision Tree Cross-Validation**

``` r
ob.sub.tree <- tree(binary_obese ~ smoke + Age + tech_use + daily_meal + fam_history + high_cal_freq + snack + alc_freq + exercise + transport, data = obesity_data_round.subset)
set.seed(0)
cv.ob.sub <- cv.tree(ob.sub.tree)
cv.ob_subdf <- data.frame(size = cv.ob.sub$size, deviance = cv.ob.sub$dev)
best_size_sub <- cv.ob.sub$size[which.min(cv.ob.sub$dev)]

ggplot(cv.ob_subdf, mapping = aes(x = size, y = deviance)) + 
  geom_point(size = 3) + 
  geom_line() +
  geom_vline(xintercept = best_size_sub, col = "darkturquoise")
```

![](<figures/unnamed-chunk-28-1.png>)
This code chunk builds a decision tree model on the rounded subset
dataset using the selected features. It performs cross-validation on the
decision tree model to determine the optimal size (number of terminal
nodes) that minimizes the deviance (error). The code then plots the
deviance against the number of terminal nodes, with a vertical line
indicating the optimal size. This plot helps visualize the process of
finding the optimal tree size through cross-validation.

**Visualizing the pruned classification tree**

``` r
ob.subtree.final <- prune.tree(ob.sub.tree, best = best_size_sub) #The subtree with best_size terminal nodes
plot(ob.subtree.final)
text(ob.subtree.final, cex=.7 ,adj=1)
```

![](<figures/unnamed-chunk-29-1.png>)

``` r
# Compute training and test errors for Rounded Subset Data
train_error_tree_sub <- mean((predict(ob.subtree.final, newdata = tr_dat) - tr_dat$binary_obese)^2)
test_error_tree_sub <- mean((predict(ob.subtree.final, newdata = te_dat) - te_dat$binary_obese)^2)
#Print The results
print(paste("Training Error Rounded Subset:" ,train_error_tree_sub))
```

    ## [1] "Training Error Rounded Subset: 0.0776502159546119"

``` r
print(paste("Testing Error Rounded Subset:" ,test_error_tree_sub))
```

    ## [1] "Testing Error Rounded Subset: 0.098511863513792"

Pruning the decision tree model to the optimal size determined by
cross-validation and plots the pruned tree. It then computes and prints
the training and testing errors for the pruned decision tree model on
the rounded subset dataset. Training Error : 0.078 Testing Error : 0.099

### REMOVED DATA

**Logistic Regression Model**

``` r
# Fit logistic regression model for obesity_data_removed
fit_mod_removed <- glm(binary_obese ~ Gender + Age + Height + fam_history + water_intake + exercise + daily_meal + snack + transport + veg_intake, family = binomial, data = tr_data)
# Summary of the model
summary(fit_mod_removed)
```

    ## 
    ## Call:
    ## glm(formula = binary_obese ~ Gender + Age + Height + fam_history + 
    ##     water_intake + exercise + daily_meal + snack + transport + 
    ##     veg_intake, family = binomial, data = tr_data)
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  -3.06579    2.94845  -1.040  0.29844    
    ## Gender       -0.54757    0.31940  -1.714  0.08646 .  
    ## Age           0.10327    0.02039   5.064  4.1e-07 ***
    ## Height        1.17792    1.68204   0.700  0.48374    
    ## fam_history   0.71996    0.24900   2.891  0.00383 ** 
    ## water_intake  0.52392    0.17807   2.942  0.00326 ** 
    ## exercise     -0.32688    0.12255  -2.667  0.00764 ** 
    ## daily_meal   -0.37213    0.12761  -2.916  0.00354 ** 
    ## snack        -0.41370    0.16624  -2.489  0.01283 *  
    ## transport    -0.12668    0.09859  -1.285  0.19882    
    ## veg_intake   -0.53201    0.20356  -2.614  0.00896 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 568.54  on 423  degrees of freedom
    ## Residual deviance: 459.90  on 413  degrees of freedom
    ## AIC: 481.9
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
# Predict outcomes for the training dataset
train_pred_removed <- predict(fit_mod_removed, newdata = tr_data, type = "response")
# Predict outcomes for the testing dataset
test_pred_removed <- predict(fit_mod_removed, newdata = te_data, type = "response")
# Convert probabilities to binary predictions using a threshold of 0.5
train_pred_binary_removed <- ifelse(train_pred_removed >= 0.5, 1, 0)
test_pred_binary_removed <- ifelse(test_pred_removed >= 0.5, 1, 0)
# Calculate training error
train_error_removed <- mean(train_pred_binary_removed != tr_data$binary_obese)
# Calculate testing error
test_error_removed <- mean(test_pred_binary_removed != te_data$binary_obese)
# Print errors
print(paste("Training Error Removed Dataset:", train_error_removed))
```

    ## [1] "Training Error Removed Dataset: 0.268867924528302"

``` r
print(paste("Testing Error Removed Dataset:", test_error_removed))
```

    ## [1] "Testing Error Removed Dataset: 0.261682242990654"

In chunck above a logistic regression model is fitted on the removed
dataset using the selected features. It predicts the outcomes for both
the training and testing datasets and converts the predicted
probabilities to binary predictions using a threshold of 0.5. It then
calculates and prints the training and testing errors for the logistic
regression model on the removed dataset. Training Error :0.269 Testing
Error :0.262

**Decision Tree**

**Decision Tree Cross-Validation**

``` r
ob.rem.tree <- tree(binary_obese ~ Age + Gender + Height + fam_history + veg_intake + snack + water_intake + daily_meal + transport + exercise, data = obesity_data_removed)
set.seed(0)
cv.rem.ob <- cv.tree(ob.rem.tree)
#names(cv.sal)
cv.ob.rem_df <- data.frame(size = cv.rem.ob$size, deviance = cv.rem.ob$dev)
best_size_rem <- cv.rem.ob$size[which.min(cv.rem.ob$dev)]

ggplot(cv.ob.rem_df, mapping = aes(x = size, y = deviance)) + 
  geom_point(size = 3) + 
  geom_line() +
  geom_vline(xintercept = best_size_rem, col = "forestgreen")
```

![](<figures/unnamed-chunk-31-1.png>)
This code chunk builds a decision tree model on the rounded subset
dataset using the selected features. It performs cross-validation on the
decision tree model to determine the optimal size (number of terminal
nodes) that minimizes the deviance (error). The code then plots the
deviance against the number of terminal nodes, with a vertical line
indicating the optimal size. This plot helps visualize the process of
finding the optimal tree size through cross-validation.

**Visualizing the pruned classification tree**

``` r
ob.remtree.final <- prune.tree(ob.rem.tree, best = best_size_rem) #The subtree with best_size terminal nodes
plot(ob.remtree.final)
text(ob.remtree.final, cex=.7 ,adj=1)
```

![](<figures/unnamed-chunk-32-1.png>)

``` r
# Compute training and test errors for Removed Data
train_error_tree_rem <- mean((predict(ob.remtree.final, newdata = tr_data) - tr_data$binary_obese)^2)
test_error_tree_rem <- mean((predict(ob.remtree.final, newdata = te_data) - te_data$binary_obese)^2)
#Print The results
print(paste("Training Error Removed Dataset:", train_error_tree_rem))
```

    ## [1] "Training Error Removed Dataset: 0.177707877369466"

``` r
print(paste("Testing Error Removed Dataset:", test_error_tree_rem ))
```

    ## [1] "Testing Error Removed Dataset: 0.180240111428212"

Pruning the decision tree model to the optimal size determined by
cross-validation and plots the pruned tree. It then computes and prints
the training and testing errors for the pruned decision tree model on
the rounded subset dataset. Training Error : 0.178 Testing Error : 0.181

**Comparision Table For All Training And Test Erros**

**Logistic Regression**

``` r
# Combine the training and test errors from all three datasets
rounded_data <- data.frame(
   Dataset_Type = "Rounded Full Data",
   Model = "Logistic Regression",
   Training_Error = c(train_error_rounded),
   Testing_Error = c(test_error_rounded)
)
subset_data <- data.frame(
  Dataset_Type = "Rounded Subset Data",
   Model = "Logistic Regression",
   Training_Error = c(train_error_subset),
   Testing_Error = c(test_error_subset)
)
removed_data <- data.frame(
   Dataset_Type = "Removed Full Data",
   Model = "Logistic Regression",
   Training_Error = c(train_error_removed),
   Testing_Error = c(test_error_removed)
)
# Combine the data frames
combined_errors <- rbind(rounded_data, subset_data, removed_data)
# Display the combined errors table
kable(combined_errors, format = "markdown", caption = "Logistic Errors For All Dataset")
```

| Dataset_Type        | Model               | Training_Error | Testing_Error |
|:--------------------|:--------------------|---------------:|--------------:|
| Rounded Full Data   | Logistic Regression |      0.1172986 |     0.1205674 |
| Rounded Subset Data | Logistic Regression |      0.1037736 |     0.1401869 |
| Removed Full Data   | Logistic Regression |      0.2688679 |     0.2616822 |

Logistic Errors For All Dataset

Combining the training and testing errors for the logistic regression
models from all three datasets (rounded full data, rounded subset, and
removed data) into a single table. It creates data frames for each
dataset, then combines them using rbind. The kable function from the
knitr package is used to display the table in a markdown format. The
table shows the training and testing errors for the logistic regression
model on each dataset, allowing for easy comparison across different
scenarios.

**Decision Tree**

``` r
# Create data frame for individual datasets with decision tree training and test errors
tree_errors_decision.tree <- data.frame(
  Dataset = c(" Rounded Full Data", "Rounded Subset", "Removed Data"),
  Model = "Decision Tree",
  Training_Error = c(train_error_tree_full, train_error_tree_sub, train_error_tree_rem),
  Test_Error = c(test_error_tree_full, test_error_tree_sub, test_error_tree_rem)
)

# Display decision tree errors table
kable(tree_errors_decision.tree, format = "markdown", caption = "Decision Tree Errors For All Dataset")
```

| Dataset           | Model         | Training_Error | Test_Error |
|:------------------|:--------------|---------------:|-----------:|
| Rounded Full Data | Decision Tree |      0.0939743 |  0.0921095 |
| Rounded Subset    | Decision Tree |      0.0776502 |  0.0985119 |
| Removed Data      | Decision Tree |      0.1777079 |  0.1802401 |

Decision Tree Errors For All Dataset

Creating a data frame tree_errors_decision.tree that contains the
training and testing errors for the decision tree models on all three
datasets (rounded full data, rounded subset, and removed data). It then
uses the kable function from the knitr package to display the table in a
markdown format. The table shows the training and testing errors for the
decision tree model on each dataset, allowing for easy comparison across
different scenarios.

## Model Evaluation

**ROC CURVE FOR DIFFERENT MODELS**

``` r
# Fit logistic regression models and compute ROC curves and AUCs
# Full Model
logit_full <- glm(binary_obese ~ Age + fam_history + high_cal_freq + water_intake + alc_freq + daily_meal + snack + transport + exercise + smoke, family = binomial, data = tr_data_round)
logit.pred_full <- predict(logit_full, type = "response")
rocobj_logit_full <- roc(tr_data_round$binary_obese, logit.pred_full)
auc_logit_full <- auc(rocobj_logit_full)

# Rounded Subset Model
logit_sub <- glm(binary_obese ~ Age + fam_history + tech_use + alc_freq + transport + snack + smoke + exercise + high_cal_freq + daily_meal, family = binomial, data = tr_dat)
logit.pred_sub <- predict(logit_sub, type = "response")
rocobj_logit_sub <- roc(tr_dat$binary_obese, logit.pred_sub)
auc_logit_sub <- auc(rocobj_logit_sub)

# Removed Data Model
logit_rem <- glm(binary_obese ~ Gender + Age + Height + fam_history + water_intake + exercise + daily_meal + snack + transport + veg_intake,family = binomial, data = tr_data)
logit.pred_rem <- predict(logit_rem, type = "response")
rocobj_logit_rem <- roc(tr_data$binary_obese, logit.pred_rem)
auc_logit_rem <- auc(rocobj_logit_rem)

# Fit decision tree models and compute ROC curves and AUCs
# Full Data
ob.tree_full <- tree(binary_obese ~ Age + fam_history + high_cal_freq + water_intake + alc_freq + daily_meal + snack + transport + exercise + smoke, data = obesity_data_rounded)
ob.tree_pred_full <- predict(ob.tree_full, newdata = te_data_round)
rocobj_tree_full <- roc(te_data_round$binary_obese, ob.tree_pred_full)
auc_tree_full <- auc(rocobj_tree_full)

# Rounded Subset Data
ob.sub.tree <- tree(binary_obese ~ Age + fam_history + tech_use + alc_freq + transport + snack + smoke + exercise + high_cal_freq + daily_meal, data = obesity_data_round.subset)
ob.sub.tree_pred <- predict(ob.sub.tree, newdata = te_dat)
rocobj_tree_sub <- roc(te_dat$binary_obese, ob.sub.tree_pred)
auc_tree_sub <- auc(rocobj_tree_sub)

# Removed Data
ob.rem.tree <- tree(binary_obese ~ Gender + Age + Height + fam_history + water_intake + exercise + daily_meal + snack + transport + veg_intake, data = obesity_data_removed)
ob.rem.tree_pred <- predict(ob.rem.tree, newdata = te_data)
rocobj_tree_rem <- roc(te_data$binary_obese, ob.rem.tree_pred)
auc_tree_rem <- auc(rocobj_tree_rem)

# Create a list of ROC objects and corresponding AUC values for logistic regression models
rocobjs_logit <- list(
  Full_Model_Logit = rocobj_logit_full,
  Rounded_Subset_Model_Logit = rocobj_logit_sub,
  Removed_Data_Model_Logit = rocobj_logit_rem
)
methods_auc_logit <- c(
  paste("Logitistic - Rounded Full Data (AUC =", round(auc_logit_full, 3), ")"),
  paste("Logitistic - Rounded Subset Data (AUC =", round(auc_logit_sub, 3), ")"),
  paste("Logitistic - Removed Data (AUC =", round(auc_logit_rem, 3), ")")
)

# Create a list of ROC objects and corresponding AUC values for decision tree models
rocobjs_tree <- list(
  Decision_Tree_Full = rocobj_tree_full,
  Decision_Tree_Rounded_Subset = rocobj_tree_sub,
  Decision_Tree_Removed = rocobj_tree_rem
)
methods_auc_tree <- c(
  paste("Decision Tree - Rounded Full Data (AUC =", round(auc_tree_full, 3),")" ),
  paste("Decision Tree - Rounded Subset Data (AUC =", round(auc_tree_sub, 3),")" ),
  paste("Decision Tree - Removed Data (AUC =", round(auc_tree_rem, 3), ")")
)

# Plot ROC curves for all logistic regression and decision tree models
ggroc(c(rocobjs_logit, rocobjs_tree), alpha = 0.5) +
  scale_color_discrete(labels = c(methods_auc_logit, methods_auc_tree)) +
  labs(title = "ROC Curves for Logistic Regression and Decision Tree Models")
```

![](<figures/unnamed-chunk-35-1.png>) 
For Model Evaluation , fitting a logistic regression and decision tree
models for all three datasets (rounded full data, rounded subset, and
removed data) using the selected features. It then computes the receiver
operating characteristic (ROC) curves and the area under the curve (AUC)
for each model and dataset combination. The ROC curve is a graphical
representation of the trade-off between the true positive rate
(sensitivity) and the false positive rate (1 - specificity) at different
classification thresholds. The AUC is a single scalar value that
represents the overall performance of the model, with higher values
indicating better performance. These metrics are useful for evaluating
and comparing the performance of different models and datasets. Also ,
createing lists of ROC objects and corresponding AUC values for both
logistic regression and decision tree models across all three datasets.
It then uses the ggroc function from the pROC package to plot the ROC
curves for all models and datasets, with the AUC values included in the
legend labels. The resulting plot provides a visual comparison of the
performance of different models and datasets, with higher curves and
larger AUC values indicating better performance.

**Confusion Matrix**

``` r
# Full Dataset
predicted_classes_full <- ifelse(test_pred_binary_rounded >= 0.5, "1", "0")
conf_matrix_full <- confusionMatrix(as.factor(predicted_classes_full), as.factor(te_data_round$binary_obese), positive = "1")
print("Confusion Matrix for Full Dataset:")
```

    ## [1] "Confusion Matrix for Full Dataset:"

``` r
print(conf_matrix_full)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0  72  16
    ##          1  35 300
    ##                                           
    ##                Accuracy : 0.8794          
    ##                  95% CI : (0.8445, 0.9089)
    ##     No Information Rate : 0.747           
    ##     P-Value [Acc > NIR] : 1.119e-11       
    ##                                           
    ##                   Kappa : 0.6611          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.01172         
    ##                                           
    ##             Sensitivity : 0.9494          
    ##             Specificity : 0.6729          
    ##          Pos Pred Value : 0.8955          
    ##          Neg Pred Value : 0.8182          
    ##              Prevalence : 0.7470          
    ##          Detection Rate : 0.7092          
    ##    Detection Prevalence : 0.7920          
    ##       Balanced Accuracy : 0.8111          
    ##                                           
    ##        'Positive' Class : 1               
    ## 

``` r
# Rounded Subset
predicted_classes_subset <- ifelse(test_pred_binary_subset >= 0.5, "1", "0")
conf_matrix_subset <- confusionMatrix(as.factor(predicted_classes_subset), as.factor(te_dat$binary_obese), positive = "1")
print("Confusion Matrix for Rounded Subset:")
```

    ## [1] "Confusion Matrix for Rounded Subset:"

``` r
print(conf_matrix_subset)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  0  1
    ##          0 27  3
    ##          1 12 65
    ##                                           
    ##                Accuracy : 0.8598          
    ##                  95% CI : (0.7793, 0.9194)
    ##     No Information Rate : 0.6355          
    ##     P-Value [Acc > NIR] : 2.132e-07       
    ##                                           
    ##                   Kappa : 0.6817          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.03887         
    ##                                           
    ##             Sensitivity : 0.9559          
    ##             Specificity : 0.6923          
    ##          Pos Pred Value : 0.8442          
    ##          Neg Pred Value : 0.9000          
    ##              Prevalence : 0.6355          
    ##          Detection Rate : 0.6075          
    ##    Detection Prevalence : 0.7196          
    ##       Balanced Accuracy : 0.8241          
    ##                                           
    ##        'Positive' Class : 1               
    ## 

``` r
# Removed Subset
predicted_classes_removed <- ifelse(test_pred_binary_removed >= 0.5, "1", "0")
conf_matrix_removed <- confusionMatrix(as.factor(predicted_classes_removed), as.factor(te_data$binary_obese), positive = "1")
print("Confusion Matrix for Removed Subset:")
```

    ## [1] "Confusion Matrix for Removed Subset:"

``` r
print(conf_matrix_removed)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  0  1
    ##          0 59 17
    ##          1 11 20
    ##                                           
    ##                Accuracy : 0.7383          
    ##                  95% CI : (0.6445, 0.8185)
    ##     No Information Rate : 0.6542          
    ##     P-Value [Acc > NIR] : 0.03982         
    ##                                           
    ##                   Kappa : 0.3986          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.34470         
    ##                                           
    ##             Sensitivity : 0.5405          
    ##             Specificity : 0.8429          
    ##          Pos Pred Value : 0.6452          
    ##          Neg Pred Value : 0.7763          
    ##              Prevalence : 0.3458          
    ##          Detection Rate : 0.1869          
    ##    Detection Prevalence : 0.2897          
    ##       Balanced Accuracy : 0.6917          
    ##                                           
    ##        'Positive' Class : 1               
    ## 

This code chunk generates and prints the confusion matrices for the
logistic regression models on all three datasets (rounded full data,
rounded subset, and removed data). The confusion matrix is a table that
summarizes the performance of a classification model by showing the
counts of true positives, true negatives, false positives, and false
negatives. It provides a detailed breakdown of the model’s predictions
compared to the actual class labels. For each dataset, the code first
converts the predicted probabilities from the logistic regression model
into binary predictions (0 or 1) using a threshold of 0.5. It then
creates a confusion matrix using the `confusionMatrix` function from the
`caret` package, comparing the predicted classes with the actual binary
outcome variable (`binary_obese`). The resulting confusion matrices are
printed, providing valuable information about the model’s performance,
such as accuracy, sensitivity, specificity, and precision, for each
dataset.

Overall, this code allows for a comprehensive evaluation and comparison
of the logistic regression models across different datasets by examining
various performance metrics, including errors, ROC curves, AUC values,
and confusion matrices.

**Cross Validation All Dataset**

``` r
# Define a function to calculate misclassification error
calc_misclassification <- function(pred, actual) {
  mean(pred != actual)
}

# Full Dataset
# Fit logistic regression model for Full Dataset
fit_mod_full <- glm(binary_obese ~ Age + fam_history + high_cal_freq + water_intake + alc_freq + daily_meal + snack + transport + exercise + smoke, data = obesity_data_rounded, family = binomial)
# Perform 5-fold cross-validation and calculate misclassification error
set.seed(0)
CV <- 5
n_all <- nrow(obesity_data_rounded)
fold_ind_full <- sample(1:CV, n_all, replace = TRUE)
cv_error_logistic_full <- mean(sapply(1:CV, function(j) {
  cv_er_log_full <- glm(binary_obese ~ Age + fam_history + high_cal_freq + water_intake + alc_freq + daily_meal + snack + transport + exercise + smoke, data = obesity_data_rounded[fold_ind_full != j, ], family = binomial)
  pred_cv_log_full <- ifelse(predict(cv_er_log_full, newdata = obesity_data_rounded[fold_ind_full == j, ], type = "response") >= 0.5, 1, 0)
  calc_misclassification(pred_cv_log_full, obesity_data_rounded$binary_obese[fold_ind_full == j])
}))

# Rounded Subset
# Fit logistic regression model for Rounded Subset
fit_mod_subset <- glm(binary_obese ~ Age + fam_history + tech_use + alc_freq + transport + snack + smoke + exercise + high_cal_freq + daily_meal, family = binomial, data = tr_dat)
# Perform 5-fold cross-validation and calculate misclassification error
set.seed(0)
CV <- 5
n_all <- nrow(tr_dat)
fold_ind_subset <- sample(1:CV, n_all, replace = TRUE)
cv_error_logistic_subset <- mean(sapply(1:CV, function(j) {
  cv_er_log_subset <- glm(binary_obese ~ Age + fam_history + tech_use + alc_freq + transport + snack + smoke + exercise + high_cal_freq + daily_meal, data = tr_dat[fold_ind_subset != j, ], family = binomial)
  pred_cv_log_subset <- ifelse(predict(cv_er_log_subset, newdata = tr_dat[fold_ind_subset == j, ], type = "response") >= 0.5, 1, 0)
  calc_misclassification(pred_cv_log_subset, tr_dat$binary_obese[fold_ind_subset == j])
}))

# Removed Subset
# Fit logistic regression model for Removed Subset
fit_mod_removed <- glm(binary_obese ~ Gender + Age + Height + fam_history + water_intake + exercise + daily_meal + snack + transport + veg_intake, family = binomial, data = tr_data)
# Perform 5-fold cross-validation and calculate misclassification error
set.seed(0)
CV <- 5
n_all <- nrow(tr_data)
fold_ind_removed <- sample(1:CV, n_all, replace = TRUE)
cv_error_logistic_removed <- mean(sapply(1:CV, function(j) {
  cv_er_log_removed <- glm(binary_obese ~ Gender + Age + Height + fam_history + water_intake + exercise + daily_meal + snack + transport + veg_intake, data = tr_data[fold_ind_removed != j, ], family = binomial)
  pred_cv_log_removed <- ifelse(predict(cv_er_log_removed, newdata = tr_data[fold_ind_removed == j, ], type = "response") >= 0.5, 1, 0)
  calc_misclassification(pred_cv_log_removed, tr_data$binary_obese[fold_ind_removed == j])
}))
```

**Misclassification Errors Table**

``` r
# Create data frame for individual datasets with decision tree training and test errors
misclassification_error <- data.frame(
  Dataset = c(" Rounded Full Data", "Rounded Subset", "Removed Data"),
  Model = "CV",
  Missclassification_Error = c(cv_error_logistic_full,cv_error_logistic_subset, cv_error_logistic_removed)
)

# Display decision tree errors table
kable(misclassification_error, format = "markdown", caption = "Cross Validation Errors For All Dataset")
```

| Dataset           | Model | Missclassification_Error |
|:------------------|:------|-------------------------:|
| Rounded Full Data | CV    |                0.1228651 |
| Rounded Subset    | CV    |                0.1222502 |
| Removed Data      | CV    |                0.3043132 |

Cross Validation Errors For All Dataset

Cross-validation is crucial in model evaluation as it helps assess the
model’s generalization performance by estimating how well it would
perform on unseen data, thereby reducing the risk of overfitting and
providing more reliable performance metrics.Here, training logistic
regression models on various datasets to predict obesity, then performs
5-fold cross-validation to assess their predictive accuracy. The
misclassification errors are calculated for each model and dataset
combination, and the results are presented in a table for comparison.
5-fold CV Error Logistic Model (Full Dataset): 0.123 5-fold CV Error
Logistic Model (Rounded Subset): 0.122 5-fold CV Error Logistic Model
(Removed Subset): 0.304
