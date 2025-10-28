# Load necessary libraries
library(MASS)
library(ggplot2)
library(gamlss)

# Load full dataset
data <- read.csv("Data_final.csv")

# Convert categorical variables to factors (for full dataset)
data$SubItemType_C <- factor(data$SubItemType_C)
data$Construction_Material_C <- factor(data$Construction_Material_C)
data$Item_Type_C <- factor(data$Item_Type_C)
data$FloorsType_C <- factor(data$FloorsType_C)
data$postcode_coast_flg <- factor(data$postcode_coast_flg)

# Set reference levels for categorical variables
data$floodvulnerability <- factor(data$floodvulnerability, levels = c("1", "2", "3", "4", "5"))
data$floodvulnerability <- relevel(data$floodvulnerability, ref = "5")
data$windvulnerability <- factor(data$windvulnerability, levels = c("1", "2", "3"))

## ------------------------ EDA on Full Data ------------------------

# 1. Zero vs. Non-Zero Analysis

# Calculate mean and variance of TClaimsNo
mean_TClaimsNo <- mean(data$TClaimsNo)
variance_TClaimsNo <- var(data$TClaimsNo)

# Print the results
cat("Mean of TClaimsNo:", mean_TClaimsNo, "\n")
cat("Variance of TClaimsNo:", variance_TClaimsNo, "\n")

# Count zero vs non-zero values in TClaimsNo
zero_count <- sum(data$TClaimsNo == 0)
nonzero_count <- sum(data$TClaimsNo > 0)

# Display the counts
cat("Zeroes:", zero_count, "Non-Zeroes:", nonzero_count, "\n")

# Percentage of zeroes
zero_percentage <- (zero_count / length(data$TClaimsNo)) * 100
cat("Percentage of zeroes:", round(zero_percentage, 2), "%\n")

# Create a separate factor variable for plotting ( to avoid modifying data) 
claim_category <- factor(ifelse(data$TClaimsNo == 0, "Zero Claims", "Non-Zero Claims"))

# Plot 
ggplot(data, aes(x = claim_category, fill = claim_category)) +
  geom_bar() +
  labs(title = "Distribution of Zero vs. Non-Zero Claims",
       x = "Claim Category",
       y = "Count of Policyholders") +
  theme_minimal() +
  scale_fill_manual(values = c("Zero Claims" = "steelblue", "Non-Zero Claims" = "orange"))


# 2. Response Variable vs. Numerical Predictors (Scatter Plots)

# Select numerical predictors
numerical_vars <- c("builtYear", "nrOfFloors", "postcode_perimeter", "postcode_alt_mean", "TExposure")

# Generate scatter plots
par(mfrow = c(2, 3)) # Arrange plots in a grid
for (var in numerical_vars) {
  plot(data[[var]], data$TClaimsNo,
       main = paste("TClaimsNo vs.", var),
       xlab = var,
       ylab = "TClaimsNo",
       pch = 19, col = "blue")
}
par(mfrow = c(1, 1)) # Reset plotting grid

# 3. Response Variable vs. Categorical Predictors (Bar Charts of Zero Claim Proportions)

# Select categorical predictors
categorical_vars <- c("SubItemType_C", "Construction_Material_C", "Item_Type_C", "FloorsType_C", "floodvulnerability", "windvulnerability")

# Create bar charts for categorical predictors vs. proportion of zero claims
par(mfrow = c(2, 3)) # Arrange plots in a grid
for (var in categorical_vars) {
  prop_table <- prop.table(table(data[[var]], data$TClaimsNo == 0), margin = 1)
  
  barplot(prop_table[, "TRUE"],
          main = paste("Proportion of Zero Claims by", var),
          xlab = var,
          ylab = "Proportion of Zero Claims",
          col = "lightblue",
          las = 2)
}

par(mfrow = c(1, 1)) # Reset plotting grid

# 4. True vs. Fitted Values Function

plot_true_vs_fitted <- function(model, model_name, data) {
  if (inherits(model, "glm")) {
    fitted_values <- fitted(model)  
  } else if (inherits(model, "gamlss")) {
    fitted_values <- fitted(model, what = "mu")  
  } else {
    stop("Unsupported model type")
  }
  
  plot_data <- data.frame(True = data$TClaimsNo, Fitted = fitted_values)
  
  ggplot(plot_data, aes(x = Fitted, y = True)) +
    geom_point(color = "blue", alpha = 0.5) +
    geom_smooth(method = "lm", color = "red", linetype = "dashed") +
    labs(title = paste("True vs. Fitted Values -", model_name),
         x = "Fitted Values",
         y = "True Values") +
    theme_minimal()
}

# 5. QQ Plots for Residual Normality Check

plot_qq_glm <- function(model, model_name) {
  # Extract Pearson residuals
  residuals_poisson <- residuals(model, type = "pearson")
  
  # Generate QQ Plot
  qqnorm(residuals_poisson, main = paste("QQ Plot -", model_name))
  qqline(residuals_poisson, col = "red", lwd = 2)
}

plot_qq_gamlss <- function(model, model_name) {
  # Extract residuals using "simple" type (for GAMLSS models)
  residuals <- resid(model, type = "simple")  
  
  # Generate QQ Plot
  qqnorm(residuals, main = paste("QQ Plot -", model_name))
  qqline(residuals, col = "red", lwd = 2)
}

## ------------------------ Taking a sample of the data ------------------------

# Set seed and take a sample of 30,000 observations
set.seed(7561)
sample_size <- 30000
sample_data <- data[sample(1:nrow(data), sample_size), ]
sample_data <- data.frame(sample_data)

## ------------------------ AIC-Based Variable Selection- Poisson ------------------------

# Fit the initial Poisson model using the full dataset
poisModel_glm <- glm(TClaimsNo ~ SubItemType_C + builtYear + nrOfFloors + Construction_Material_C + 
                       Item_Type_C + FloorsType_C + Floors_No + floodvulnerability + windvulnerability +
                       postcode_area + postcode_perimeter + postcode_coast_len + postcode_coast_flg + 
                       postcode_alt_mean + postcode_rgh_mean + postcode_slo_mean + offset(log(TExposure)), 
                     family = poisson, data = sample_data)

summary(poisModel_glm)


# --------------------------
# Forward Selection (AIC)
# --------------------------
base_model <- glm(TClaimsNo ~ 1 + offset(log(TExposure)), data = sample_data, family = poisson)
current_vars <- c()
candidates <- setdiff(names(sample_data), c("TClaimsNo", "TExposure")) 
old_aic <- AIC(base_model)
improvement <- TRUE

while (improvement) {
  improvement <- FALSE
  best_aic <- old_aic
  best_var <- NULL
  
  for (v in candidates) {
    test_vars <- c(current_vars, v)
    form <- as.formula(paste("TClaimsNo ~", paste(test_vars, collapse = " + "), "+ offset(log(TExposure))"))
    model_temp <- glm(form, data = sample_data, family = poisson)
    this_aic <- AIC(model_temp)
    
    if (this_aic < best_aic) {
      best_aic <- this_aic
      best_var <- v
    }
  }
  
  if (!is.null(best_var)) {
    current_vars <- c(current_vars, best_var)
    candidates <- setdiff(candidates, best_var)
    old_aic <- best_aic
    improvement <- TRUE
  }
}

# Final Forward AIC Model
forward_formula <- as.formula(paste("TClaimsNo ~", paste(current_vars, collapse = " + "), "+ offset(log(TExposure))"))
forward_AIC_model <- glm(forward_formula, data = sample_data, family = poisson)
summary(forward_AIC_model)


#--------------------------
# Backward Selection (AIC)
#--------------------------
current_vars <- setdiff(names(sample_data), c("TClaimsNo", "TExposure"))
old_aic <- AIC(poisModel_glm)
improvement <- TRUE

while (improvement) {
  improvement <- FALSE
  best_aic <- old_aic
  worst_var <- NULL
  
  for (v in current_vars) {
    candidate_vars <- setdiff(current_vars, v)
    form <- as.formula(paste("TClaimsNo ~", paste(candidate_vars, collapse = " + "), "+ offset(log(TExposure))"))
    model_temp <- glm(form, data = sample_data, family = poisson)
    this_aic <- AIC(model_temp)
    
    if (this_aic < best_aic) {
      best_aic <- this_aic
      worst_var <- v
    }
  }
  
  if (!is.null(worst_var)) {
    current_vars <- setdiff(current_vars, worst_var)
    old_aic <- best_aic
    improvement <- TRUE
  }
}

# Final Backward AIC Model
backward_formula <- as.formula(paste("TClaimsNo ~", paste(current_vars, collapse = " + "), "+ offset(log(TExposure))"))
backward_AIC_model <- glm(backward_formula, data = sample_data, family = poisson)
summary(backward_AIC_model)

# Stepwise Selection using AIC (Both directions)
stepAIC_model <- stepAIC(poisModel_glm, direction = "both", trace = FALSE)
summary(stepAIC_model)

## ------------------------ BIC-Based Variable Selection- Poisson ------------------------

#--------------------------
# Forward Selection (BIC)
#--------------------------
k_bic <- log(nrow(sample_data))
base_model <- glm(TClaimsNo ~ 1 + offset(log(TExposure)), data = sample_data, family = poisson)
current_vars <- c()
candidates <- setdiff(names(sample_data), c("TClaimsNo", "TExposure"))
old_bic <- AIC(base_model, k = k_bic)
improvement <- TRUE

while (improvement) {
  improvement <- FALSE
  best_bic <- old_bic
  best_var <- NULL
  
  for (v in candidates) {
    test_vars <- c(current_vars, v)
    form <- as.formula(paste("TClaimsNo ~", paste(test_vars, collapse = " + "), "+ offset(log(TExposure))"))
    model_temp <- glm(form, data = sample_data, family = poisson)
    this_bic <- AIC(model_temp, k = k_bic)
    
    if (this_bic < best_bic) {
      best_bic <- this_bic
      best_var <- v
    }
  }
  
  if (!is.null(best_var)) {
    current_vars <- c(current_vars, best_var)
    candidates <- setdiff(candidates, best_var)
    old_bic <- best_bic
    improvement <- TRUE
  }
}

# Final Forward BIC Model
forward_formula <- as.formula(paste("TClaimsNo ~", paste(current_vars, collapse = " + "), "+ offset(log(TExposure))"))
forward_BIC_model <- glm(forward_formula, data = sample_data, family = poisson)
summary(forward_BIC_model)
BIC(forward_BIC_model)

#--------------------------
# Backward Selection (BIC)
#--------------------------
current_vars <- setdiff(names(sample_data), c("TClaimsNo", "TExposure"))
old_bic <- AIC(poisModel_glm, k = k_bic)
improvement <- TRUE

while (improvement) {
  improvement <- FALSE
  best_bic <- old_bic
  worst_var <- NULL
  
  for (v in current_vars) {
    candidate_vars <- setdiff(current_vars, v)
    form <- as.formula(paste("TClaimsNo ~", paste(candidate_vars, collapse = " + "), "+ offset(log(TExposure))"))
    model_temp <- glm(form, data = sample_data, family = poisson)
    this_bic <- AIC(model_temp, k = k_bic)
    
    if (this_bic < best_bic) {
      best_bic <- this_bic
      worst_var <- v
    }
  }
  
  if (!is.null(worst_var)) {
    current_vars <- setdiff(current_vars, worst_var)
    old_bic <- best_bic
    improvement <- TRUE
  }
}

# Final Backward BIC Model
backward_formula <- as.formula(paste("TClaimsNo ~", paste(current_vars, collapse = " + "), "+ offset(log(TExposure))"))
backward_BIC_model <- glm(backward_formula, data = sample_data, family = poisson)
summary(backward_BIC_model)
BIC(backward_BIC_model)

# Stepwise Selection using BIC (Both directions)
stepBIC_model <- stepAIC(poisModel_glm, direction = "both", k = log(nrow(sample_data)), trace = FALSE)
summary(stepBIC_model)
BIC(stepBIC_model)


## ------------------------ AIC-Based Variable Selection-NB ------------------------

library(gamlss)

# Fit the initial NB GAMLSS model
nb_gamlss_model <- gamlss(TClaimsNo ~ SubItemType_C + builtYear + nrOfFloors + Construction_Material_C + 
                            Item_Type_C + FloorsType_C + Floors_No + floodvulnerability + windvulnerability +
                            postcode_area + postcode_perimeter + postcode_coast_len + postcode_coast_flg + 
                            postcode_alt_mean + postcode_rgh_mean + postcode_slo_mean + offset(log(TExposure)), 
                          family = NBI, data = sample_data)

# Summary of the initial model
summary(nb_gamlss_model)

# Define full formula with all predictors
full_formula_nb <- TClaimsNo ~ SubItemType_C + builtYear + nrOfFloors + Construction_Material_C + 
  Item_Type_C + FloorsType_C + Floors_No + floodvulnerability + windvulnerability +
  postcode_area + postcode_perimeter + postcode_coast_len + postcode_coast_flg + 
  postcode_alt_mean + postcode_rgh_mean + postcode_slo_mean + offset(log(TExposure))

#--------------------------
# Forward Selection (AIC) - NB
#--------------------------
base_model_nb <- gamlss(TClaimsNo ~ 1 + offset(log(TExposure)), family = NBI, data = sample_data)
current_vars_nb <- c()
candidates_nb <- setdiff(names(sample_data), c("TClaimsNo", "TExposure"))
old_aic_nb <- AIC(base_model_nb)
improvement <- TRUE

while (improvement) {
  improvement <- FALSE
  best_aic_nb <- old_aic_nb
  best_var_nb <- NULL
  
  for (v in candidates_nb) {
    test_vars_nb <- c(current_vars_nb, v)
    form_nb <- as.formula(paste("TClaimsNo ~", paste(test_vars_nb, collapse = " + "), "+ offset(log(TExposure))"))
    model_temp_nb <- gamlss(form_nb, family = NBI, data = sample_data)
    this_aic_nb <- AIC(model_temp_nb)
    
    if (this_aic_nb < best_aic_nb) {
      best_aic_nb <- this_aic_nb
      best_var_nb <- v
    }
  }
  
  if (!is.null(best_var_nb)) {
    current_vars_nb <- c(current_vars_nb, best_var_nb)
    candidates_nb <- setdiff(candidates_nb, best_var_nb)
    old_aic_nb <- best_aic_nb
    improvement <- TRUE
  }
}

# Final Forward AIC Model - NB
forward_formula_nb <- as.formula(paste("TClaimsNo ~", paste(current_vars_nb, collapse = " + "), "+ offset(log(TExposure))"))
forward_AIC_nb_model <- gamlss(forward_formula_nb, family = NBI, data = sample_data)
summary(forward_AIC_nb_model)

#--------------------------
# Backward Selection (AIC) - NB
#--------------------------
full_model_nb <- gamlss(full_formula_nb, family = NBI, data = sample_data)
current_vars_nb <- setdiff(names(sample_data), c("TClaimsNo", "TExposure"))
old_aic_nb <- AIC(full_model_nb)
improvement <- TRUE

while (improvement) {
  improvement <- FALSE
  best_aic_nb <- old_aic_nb
  worst_var_nb <- NULL
  
  for (v in current_vars_nb) {
    candidate_vars_nb <- setdiff(current_vars_nb, v)
    form_nb <- as.formula(paste("TClaimsNo ~", paste(candidate_vars_nb, collapse = " + "), "+ offset(log(TExposure))"))
    model_temp_nb <- gamlss(form_nb, family = NBI, data = sample_data)
    this_aic_nb <- AIC(model_temp_nb)
    
    if (this_aic_nb < best_aic_nb) {
      best_aic_nb <- this_aic_nb
      worst_var_nb <- v
    }
  }
  
  if (!is.null(worst_var_nb)) {
    current_vars_nb <- setdiff(current_vars_nb, worst_var_nb)
    old_aic_nb <- best_aic_nb
    improvement <- TRUE
  }
}

# Final Backward AIC Model - NB
backward_formula_nb <- as.formula(paste("TClaimsNo ~", paste(current_vars_nb, collapse = " + "), "+ offset(log(TExposure))"))
backward_AIC_nb_model <- gamlss(backward_formula_nb, family = NBI, data = sample_data)
summary(backward_AIC_nb_model)

### Stepwise Selection using AIC (Both Directions)
stepAIC_nb <- stepGAIC(nb_gamlss_model, direction = "both", k = 2, trace = FALSE)
summary(stepAIC_nb)


## ------------------------ BIC-Based Variable Selection-NB ------------------------

#--------------------------
# Forward Selection (BIC) - NB
#--------------------------
k_bic_nb <- log(nrow(sample_data))
base_model_nb <- gamlss(TClaimsNo ~ 1 + offset(log(TExposure)), family = NBI, data = sample_data)
current_vars_nb <- c()
candidates_nb <- setdiff(names(sample_data), c("TClaimsNo", "TExposure"))
old_bic_nb <- AIC(base_model_nb, k = k_bic_nb)
improvement <- TRUE

while (improvement) {
  improvement <- FALSE
  best_bic_nb <- old_bic_nb
  best_var_nb <- NULL
  
  for (v in candidates_nb) {
    test_vars_nb <- c(current_vars_nb, v)
    form_nb <- as.formula(paste("TClaimsNo ~", paste(test_vars_nb, collapse = " + "), "+ offset(log(TExposure))"))
    model_temp_nb <- gamlss(form_nb, family = NBI, data = sample_data)
    this_bic_nb <- AIC(model_temp_nb, k = k_bic_nb)
    
    if (this_bic_nb < best_bic_nb) {
      best_bic_nb <- this_bic_nb
      best_var_nb <- v
    }
  }
  
  if (!is.null(best_var_nb)) {
    current_vars_nb <- c(current_vars_nb, best_var_nb)
    candidates_nb <- setdiff(candidates_nb, best_var_nb)
    old_bic_nb <- best_bic_nb
    improvement <- TRUE
  }
}

# Final Forward BIC Model - NB
forward_BIC_nb_formula <- as.formula(paste("TClaimsNo ~", paste(current_vars_nb, collapse = " + "), "+ offset(log(TExposure))"))
forward_BIC_nb_model <- gamlss(forward_BIC_nb_formula, family = NBI, data = sample_data)
summary(forward_BIC_nb_model)
BIC(forward_BIC_nb_model)

#--------------------------
# Backward Selection (BIC) - NB
#--------------------------
full_model_nb <- gamlss(full_formula_nb, family = NBI, data = sample_data)
current_vars_nb <- setdiff(names(sample_data), c("TClaimsNo", "TExposure"))
old_bic_nb <- AIC(full_model_nb, k = k_bic_nb)
improvement <- TRUE

while (improvement) {
  improvement <- FALSE
  best_bic_nb <- old_bic_nb
  worst_var_nb <- NULL
  
  for (v in current_vars_nb) {
    candidate_vars_nb <- setdiff(current_vars_nb, v)
    form_nb <- as.formula(paste("TClaimsNo ~", paste(candidate_vars_nb, collapse = " + "), "+ offset(log(TExposure))"))
    model_temp_nb <- gamlss(form_nb, family = NBI, data = sample_data)
    this_bic_nb <- AIC(model_temp_nb, k = k_bic_nb)
    
    if (this_bic_nb < best_bic_nb) {
      best_bic_nb <- this_bic_nb
      worst_var_nb <- v
    }
  }
  
  if (!is.null(worst_var_nb)) {
    current_vars_nb <- setdiff(current_vars_nb, worst_var_nb)
    old_bic_nb <- best_bic_nb
    improvement <- TRUE
  }
}

# Final Backward BIC Model - NB
backward_BIC_nb_formula <- as.formula(paste("TClaimsNo ~", paste(current_vars_nb, collapse = " + "), "+ offset(log(TExposure))"))
backward_BIC_nb_model <- gamlss(backward_BIC_nb_formula, family = NBI, data = sample_data)
summary(backward_BIC_nb_model)
BIC(backward_BIC_nb_model)


### Stepwise Selection using BIC (Both Directions)
stepBIC_nb <- stepGAIC(nb_gamlss_model, direction = "both", k = log(nrow(sample_data)), trace = FALSE)
summary(stepBIC_nb)
BIC(stepBIC_nb)

## ------------------------ Zero-Inflated Poisson Model (ZIP) ------------------------

# Load necessary library
library(gamlss)

# Fit the Zero-Inflated Poisson Model (ZIP) using the sampled dataset
zip_model <- gamlss(TClaimsNo ~ SubItemType_C + builtYear + Floors_No + postcode_alt_mean + 
                      offset(log(TExposure)), 
                    sigma.formula = ~1, # Keeping zero-inflation parameter constant
                    family = ZIP,  #Zero-Inflated Poisson family
                    data = sample_data)
  
# Display model summary
summary(zip_model)

# Model diagnostics
plot(zip_model)

## ------------------------ Zero-Inflated Negative Binomial Model (ZINB) ------------------------

# Load necessary library
library(gamlss)

# Fit the Zero-Inflated Negative Binomial Model (ZINB) using the sampled dataset
zinb_model <- gamlss(TClaimsNo ~ SubItemType_C + builtYear + Floors_No + postcode_alt_mean + 
                       offset(log(TExposure)), 
                     sigma.formula = ~1,  # Dispersion parameter treated as constant
                     nu.formula = ~1,  # Zero-inflation parameter treated as constant
                     sigma.start = 1.9656,
                     family = ZINBI,  # Zero-Inflated NB Type I family
                     data = sample_data)

# Display model summary
summary(zinb_model)

# Model diagnostics
plot(zinb_model)

#data desc
#zeroes vs non zeroes
#plot response variable y against x
#see which combination of x's lead to extra zeroes

#hurdle models
#BIC ends up w fewer covariates, for purposes of explaining the data with more covariates we can look into aic