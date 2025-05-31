#### Load Required Libraries ####
library(dplyr)
library(CalibrationCurves)
library(purrr)
library(tidyr)

#### Define Folder and Import Files ####
folder_path <- "/Users/ebbablomdahl/Thesis/Thesis/Results/random_forest"
pattern <- "^results_8_2_.*\\.csv$"

# List and import files
files_to_import <- list.files(path = folder_path, pattern = pattern, full.names = TRUE)

# Stratified sampling function
datasets <- lapply(files_to_import, function(file) {
  data <- read.csv(file)
  
  # Calculate the number of samples per 'center'
  centers <- unique(data$center)
  samples_per_center <- floor(500 / length(centers))
  
  # Perform stratified sampling
  sampled_data <- data %>%
    group_by(center) %>%
    slice_sample(n = samples_per_center, replace = TRUE) %>% 
    ungroup()
  
  # If the total sample is less than 500 due to rounding, sample extra randomly
  if (nrow(sampled_data) < 500) {
    additional_samples <- data %>%
      slice_sample(n = 500 - nrow(sampled_data))
    sampled_data <- bind_rows(sampled_data, additional_samples)
  }
  
  sampled_data
})

names(datasets) <- gsub(".csv", "", basename(files_to_import))

#### Function to Calculate Calibration Metrics and Create Plots ####
calculate_calibration <- function(data, model_name) {
  data$outcome1 <- ifelse(data$outcome1 == "X1", 1, 0)
  intercepts <- numeric(100)
  slopes <- numeric(100)
  
  # Set up an empty base R plot for each model
  plot(NA, NA, xlim = c(0, 1), ylim = c(0, 1),
       main = paste("Flexible Calibration Plot -", model_name),
       xlab = "Predicted Probability", ylab = "Observed Proportion")
  
  # Add red reference line
  abline(0, 1, col = "red", lwd = 1)
  
  for (i in 1:100) {
    col_name <- paste0("probabilities_", i)
    
    # Adjust extreme probabilities
    data[[col_name]] <- pmax(pmin(data[[col_name]], 0.9999), 0.0001)
    
    # Suppress automatic plotting
    pdf(NULL)
    flex_curve <- val.prob.ci.2(data[[col_name]], as.integer(data$outcome1))
    dev.off()
    
    # Store intercept and slope
    intercepts[i] <- flex_curve$Calibration$Intercept[1]
    slopes[i] <- flex_curve$Calibration$Slope[1]
    
    # Plot the flexible calibration curve with faint black color
    lines(flex_curve$CalibrationCurves$FlexibleCalibration$x, 
          flex_curve$CalibrationCurves$FlexibleCalibration$y, 
          col = rgb(0, 0, 0, 0.2))
  }
  
  list(
    calibration_data = tibble(run = 1:100, intercept = intercepts, slope = slopes),
    model_name = model_name
  )
}

#### Set Plot Layout: One Plot per Model ####
num_models <- length(datasets)
cols <- ceiling(sqrt(num_models))
rows <- ceiling(num_models / cols)
par(mfrow = c(rows, cols))  # Grid layout

#### Apply Function and Collect Results ####
calibration_results_list <- lapply(names(datasets), function(name) {
  result <- calculate_calibration(datasets[[name]], name)
  result$calibration_data$model <- name
  result
})

# Reset plotting layout to default
par(mfrow = c(1, 1))

# Combine calibration results into one dataframe
calibration_results <- bind_rows(lapply(calibration_results_list, `[[`, "calibration_data"))

#### Plot: Distribution of Slopes for Each Model ####
library(ggplot2)
ggplot(calibration_results, aes(x = slope)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  facet_wrap(~ model, scales = "free") +
  theme_minimal() +
  labs(title = "Calibration Slopes Distribution", x = "Slope", y = "Frequency")

#### Plot: Mean Slope with 95% CI ####
slope_summary <- calibration_results %>%
  group_by(model) %>%
  summarise(mean_slope = mean(slope),
            ci_low = quantile(slope, 0.025),
            ci_high = quantile(slope, 0.975),
            .groups = "drop")

ggplot(slope_summary, aes(x = model, y = mean_slope)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +
  theme_minimal() +
  labs(title = "Mean Calibration Slope with 95% CI", x = "Model", y = "Mean Slope") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### Boxplot for Slopes ####
ggplot(calibration_results, aes(x = model, y = slope)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot of Calibration Slopes", x = "Model", y = "Slope") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### Plot: Distribution of Intercepts for Each Model ####
ggplot(calibration_results, aes(x = intercept)) +
  geom_histogram(bins = 30, fill = "darkgreen", color = "black") +
  facet_wrap(~ model, scales = "free") +
  theme_minimal() +
  labs(title = "Calibration Intercepts Distribution", x = "Intercept", y = "Frequency")

#### Plot: Mean Intercept with 95% CI ####
intercept_summary <- calibration_results %>%
  group_by(model) %>%
  summarise(mean_intercept = mean(intercept),
            ci_low = quantile(intercept, 0.025),
            ci_high = quantile(intercept, 0.975))

ggplot(intercept_summary, aes(x = model, y = mean_intercept)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +
  theme_minimal() +
  labs(title = "Mean Calibration Intercept with 95% CI", x = "Model", y = "Mean Intercept") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### Boxplot for Intercepts ####
ggplot(calibration_results, aes(x = model, y = intercept)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot of Calibration Intercepts", x = "Model", y = "Intercept") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
