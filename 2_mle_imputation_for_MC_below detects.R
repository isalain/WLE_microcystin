
########################
# Maximum likelihood estimation (MLE) Imputation
########################
library(dplyr)
library(fitdistrplus)
# 1. Extract numeric values from pMicr, handling "bdl" and "<" values
# And creating a new column with the original data preserved
RawData$pMicr_numeric <- gsub("<", "", RawData$pMicr)
RawData$pMicr_numeric <- gsub("bdl", "0.1", RawData$pMicr_numeric)
RawData$pMicr_numeric <- as.numeric(RawData$pMicr_numeric)
RawData$pMicr_censored <- grepl("<|bdl", RawData$pMicr)

# 2. Creating a dataset for distribution fitting
# Only include rows where pMicr_numeric has valid values
fit_data <- RawData[!is.na(RawData$pMicr_numeric), ]

# 3. Creating censored data for fitdistcens function
censored_data <- data.frame(
  left = ifelse(fit_data$pMicr_censored, NA, fit_data$pMicr_numeric),
  right = ifelse(fit_data$pMicr_censored, 0.1, fit_data$pMicr_numeric)
)

# 4. Fitting three candidate distributions
lnorm_fit <- fitdistcens(censored_data, "lnorm")
gamma_fit <- fitdistcens(censored_data, "gamma")
weibull_fit <- fitdistcens(censored_data, "weibull")

# 5. Compare fits
aic_bic <- data.frame(
  Distribution = c("Lognormal", "Gamma", "Weibull"),
  AIC = c(lnorm_fit$aic, gamma_fit$aic, weibull_fit$aic),
  BIC = c(lnorm_fit$bic, gamma_fit$bic, weibull_fit$bic)
)
print(aic_bic)

# 6. MLE Imputation from lognormal distribution 
# Use lognormal parameters
mle_params <- lnorm_fit$estimate

# Count all censored values in the dataset
total_censored <- sum(RawData$pMicr_censored, na.rm = TRUE)
total_censored

# Generating random values for inputation
set.seed(123) 
#create many values for substitutions
subs <- rlnorm(total_censored * 100, mle_params[1], mle_params[2])

# Keeping only values below detection limit
subs <- subs[subs < 0.1] 
# Round to two decimal places
subs <- round(subs, 2)    

# Creating imputed dataset
# Start with original pMicr_numeric values
RawData$MLE_imputed_123 <- RawData$pMicr_numeric

# Get indices of censored values in the original dataset
censored_indices <- which(RawData$pMicr_censored)

# Only replace censored values 
# with indices that do not exceed the length of subs
if(length(censored_indices) <= length(subs)) {
  RawData$MLE_imputed_123[censored_indices] <- subs
} else {
  # If not enough imputed values, reuse available ones
  RawData$MLE_imputed_123[censored_indices] <- c(subs, rep(subs[length(subs)], length(censored_indices) - length(subs)))
}

# Compare original and imputed values for censored data
results_check <- RawData %>%
  dplyr::select(Site, pMicr, pMicr_numeric, pMicr_censored, MLE_imputed_123) %>%
  filter(pMicr_censored)

write.csv(RawData, "RawData_with_imputed.csv", row.names = FALSE)

str(RawData)
vars_to_check <- c("Temp", "Beam_att", "Turb", "Phyco", "TP", "TDP", "SRP", "NH3", "NOx", "MLE_imputed_123")
filtered_data <- RawData %>%
  filter(if_all(all_of(vars_to_check), ~!is.na(.)))

###########################################
### REMOVING EXTREME VALUES ###########
###################################
merged_data2 <- subset(filtered_data, 
                       (is.na(Chla) | Chla < 1000) & 
                         (is.na(Phyco) | Phyco < 1000) & 
                         (is.na(TP) | TP < 1000) & 
                         (is.na(MLE_imputed_123) | MLE_imputed_123 < 100) & 
                         (is.na(NH3) | NH3 < 500) & 
                         (is.na(TDP) | TDP < 200))
