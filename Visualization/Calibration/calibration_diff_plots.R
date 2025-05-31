#### test a few ways to plot flexible calibration curves ####
#### Load Required Libraries ####
library(dplyr)
library(CalibrationCurves)
library(purrr)
library(tidyr)

#import center info data
center_info <- read.csv("/Users/ebbablomdahl/Thesis/Thesis/center_info.csv")

#### Define Folder and Import Files ####
folder_path <- "/Users/ebbablomdahl/Thesis/Thesis/Results/xgboost"
pattern <- "^results_8_2_1000_TRUE.*\\.csv$"

# List and import files
files_to_import <- list.files(path = folder_path, pattern = pattern, full.names = TRUE)
#files_to_import <- files_to_import[3]

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
  # data$outcome1 <- ifelse(data$outcome1 == "X1", 1, 0) # for random forest otherwise remove 
  intercepts <- numeric(100)
  slopes <- numeric(100)
  
  # Set up an empty base R plot for each model
  plot(NA, NA, xlim = c(0, 1), ylim = c(0, 1),
       type="n",
       main = paste("Flexible Calibration Plot -", model_name),
       xlab = "Predicted Probability", ylab = "Observed Proportion")
  
  # Add red reference line
  # abline(0, 1, col = "red", lwd = 1)
  
  random_runs <- sample(1:100, 100)
  
  # Store curves
  all_curves <- list()
  
  for (i in random_runs) {
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
          col = rgb(0, 0, 0, 0.2), 
          lwd=1,
          type = "l"
          )
    
    
    # Store flexible calibration curve
    all_curves[[length(all_curves) + 1]] <- tibble(
      run = i,
      model = model_name,
      x = flex_curve$CalibrationCurves$FlexibleCalibration$x,
      y = flex_curve$CalibrationCurves$FlexibleCalibration$y
    )
  }
  
  # Add red reference line
  abline(0, 1, col = "red", lwd = 1)
  
  list(
    calibration_data = tibble(run = 1:100, intercept = intercepts, slope = slopes),
    model_name = model_name, curve_data = bind_rows(all_curves)
  )
}


# Apply function to all datasets
results_list <- lapply(names(datasets), function(name) {
  calculate_calibration(datasets[[name]], name)
})

########### this plot #########################

### do with ggplot ###
# use this plot for plotting for the overall overview

# Combine all ggplot curve data
curve_data_all <- bind_rows(lapply(results_list, function(x) x$curve_data))

# Plot with ggplot
ggplot(curve_data_all, aes(x = x, y = y, group = run)) +
  geom_line(alpha = 0.2, color = "black") +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  facet_wrap(~model, scales = "free") +
  labs( #title = "8 onco vs 2 non-onco - 100 pateints/center - No group-level predictor",
       x = "Predicted Probability",
       y = "Observed Proportion") +
  theme_minimal() +
  theme(strip.text = element_blank(), 
        plot.title = element_text(face = "bold", size = 10))

#### plot center specific #### 

# start with stacking all test runs and then plot one curve per center or one curve per onco and non-onco

data_stacked <- lapply(files_to_import, function(file) {
  data <- read.csv(file)
  
  #### Only for RF!!!#####
  # data$outcome1 <- ifelse(data$outcome1 == "X1", 1, 0) # for random forest otherwise remove 
  
  # remove unecessary columns (need outcome1, probabilities, center, oncocenter)
  data <- data %>%
    select(-lCA125, -Age, -bilateral, -llesdmax, -loc10, -propsol, -papnr,
           -Ascites, -Shadows, -colscore, -wallreg, -papflow, -lesdmax, -starts_with("predictions_"))
  
  # if dataset doesn't contain oncocenter add it 
  if (!("oncocenter" %in% names(data))){
    data <- left_join(data, center_info, by = 'center')
  }# else{
    #### Only for RF!!!#####
  #   data$oncocenter <- ifelse(data$oncocenter == "X1", 1, 0) # for random forest otherwise remove
  # }
  
  # stack ontop of eachother 
  # Assuming your data frame is called df
  data <- data %>%
    pivot_longer(
      cols = starts_with("probabilities_"),
      names_to = "model",
      values_to = "probability"
    )
})

#### plot one curve per center ####

# Stack all test runs

# Combine into one long data frame
stacked_df <- bind_rows(data_stacked)

# Sample up to 5000 per center
sampled_per_center <- stacked_df %>%
  group_by(center) %>%
  group_modify(~ slice_sample(.x, n = min(5000, nrow(.x)))) %>%
  ungroup()

# Adjust probability limits
sampled_per_center <- sampled_per_center %>%
  mutate(probability = pmax(pmin(probability, 0.9999), 0.0001))

# One calibration curve per center
calibration_curves <- sampled_per_center %>%
  group_by(center) %>%
  group_split() %>%
  map_dfr(function(df) {
    pdf(NULL)  # suppress plotting
    val <- val.prob.ci.2(df$probability, df$outcome1)
    dev.off()
    
    tibble(
      center = df$center[1],
      oncocenter = df$oncocenter[1],
      x = val$CalibrationCurves$FlexibleCalibration$x,
      y = val$CalibrationCurves$FlexibleCalibration$y
    )
  })


# Non-oncology centers
ggplot(filter(calibration_curves, oncocenter == 0), 
       aes(x = x, y = y, color = center)) +
  geom_line(size = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  labs(title = "Non-oncology Centers",
       x = "Predicted Probability", y = "Observed Proportion") +
  theme_minimal() +
  theme(legend.position = "right")

# Oncology centers
ggplot(filter(calibration_curves, oncocenter == 1), 
       aes(x = x, y = y, color = center)) +
  geom_line(size = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  labs(title = "Oncology Centers",
       x = "Predicted Probability", y = "Observed Proportion") +
  theme_minimal() +
  theme(legend.position = "right")




#### plot 100 cruves per center type ####

calculate_calibration <- function(data, model_name) {
  
  # if dataset doesn't contain oncocenter add it 
  if (!("oncocenter" %in% names(data))){
    data <- left_join(data, center_info, by = 'center')
  }
  
  # data$outcome1 <- ifelse(data$outcome1 == "X1", 1, 0)  # adjust for random forest
  
  intercepts <- numeric(100)
  slopes <- numeric(100)
  all_curves <- list()
  
  random_runs <- sample(1:100, 100)
  
  for (i in random_runs) {
    col_name <- paste0("probabilities_", i)
    data[[col_name]] <- pmax(pmin(data[[col_name]], 0.9999), 0.0001)
    
    # Split by oncocenter value: 0 = non-onco, 1 = onco
    for (onco_val in unique(data$oncocenter)) {
      df_onco <- data %>% filter(oncocenter == onco_val)
      
      if (nrow(df_onco) < 10) next  # skip too small groups
      
      pdf(NULL)  # suppress plotting
      flex_curve <- val.prob.ci.2(df_onco[[col_name]], df_onco$outcome1)
      dev.off()
      
      # Store flexible calibration curve
      all_curves[[length(all_curves) + 1]] <- tibble(
        run = i,
        model = model_name,
        oncocenter = onco_val,
        x = flex_curve$CalibrationCurves$FlexibleCalibration$x,
        y = flex_curve$CalibrationCurves$FlexibleCalibration$y
      )
    }
  }
  
  return(bind_rows(all_curves))
}


# Apply function to all datasets
results_list <- lapply(names(datasets), function(name) {
  calculate_calibration(datasets[[name]], name)
})

# use this plot for plotting for the overall overview

# Combine all ggplot curve data
curve_data_all <- bind_rows(lapply(results_list, function(x) x))

ggplot(curve_data_all, aes(x = x, y = y, group = run)) +
  geom_line(alpha = 0.2, color = "black") +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  facet_wrap(~oncocenter) +
  labs(title = "Flexible Calibration Curves by Oncocenter Type",
       x = "Predicted Probability", y = "Observed Proportion") +
  theme_minimal()

#### get two separate plots #### 

# Filter data by oncocenter status
curve_data_onco <- curve_data_all %>% filter(oncocenter == TRUE)
curve_data_nononco <- curve_data_all %>% filter(oncocenter == FALSE)

# Plot for oncology centers
plot_onco <- ggplot(curve_data_onco, aes(x = x, y = y, group = run)) +
  geom_line(alpha = 0.2, color = "black") +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Oncology Centers",
       x = "Predicted Probability", y = "Observed Proportion") +
  theme_minimal()

# Plot for non-oncology centers
plot_nononco <- ggplot(curve_data_nononco, aes(x = x, y = y, group = run)) +
  geom_line(alpha = 0.2, color = "black") +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Non-Oncology Centers",
       x = "Predicted Probability", y = "Observed Proportion") +
  theme_minimal()

# Print plots
plot_onco
plot_nononco






