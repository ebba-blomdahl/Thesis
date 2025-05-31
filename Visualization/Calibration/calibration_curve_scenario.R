#### script to plot one calibration curve per scenario #### 
library(dplyr)
library(CalibrationCurves)
library(purrr)
library(tidyr)
library(ggplot2)
library(readr)
library(data.table)
library(stringr)
library(cowplot)

#import center info data
center_info <- read.csv("/Users/ebbablomdahl/Thesis/Thesis/center_info.csv")

# import all files 
folder_path <- "/Users/ebbablomdahl/Thesis/Thesis/Results/XGBoost"
pattern <- "^results_.*\\.csv$"

# List and import files
files_to_import <- list.files(path = folder_path, pattern = pattern, full.names = TRUE)

# Initialize an empty list to store results
final_results <- list()

for (file in files_to_import) {
  # Read the current file
  data <- read.csv(file)
  
  # Convert outcome to numeric
  # data$outcome1 <- ifelse(data$outcome1 == "X1", 1, 0)
  
  # Keep only outcome1 and all probability columns
  data <- data %>%
    select(outcome1, starts_with("probabilities_"))
  
  # Pivot to long format: each row becomes one probability value with its model
  data_long <- pivot_longer(
    data, 
    cols = starts_with("probabilities_"),
    names_to = "model",
    values_to = "probability"
  ) %>%
    mutate(
      model = gsub("probabilities_", "Model_", model),
      probability = pmax(pmin(probability, 0.9999), 0.0001)
    )
  
  # Sample 5000 values across all models and rows
  sampled <- data_long %>%
    slice_sample(n = min(5000, nrow(data_long)))
  
  # Store sampled values
  final_results[[file]] <- sampled
}

# Combine results from all files into one data frame (optional step)
final_results_df <- bind_rows(final_results, .id = "source_file")

# Save the final combined results if needed
write.csv(final_results_df,"/Users/ebbablomdahl/Thesis/Thesis/Results/calibration_curves/cc_xgboost", row.names = FALSE)
final_results_df <- read.csv("/Users/ebbablomdahl/Thesis/Thesis/Results/calibration_curves/cc_xgboost")


# Function to calculate the flexible calibration curve using val.prob.ci.2
calculate_flexible_curve <- function(df, file_name) {
  
  # Adjust extreme probabilities
  df$probability <- pmax(pmin(df$probability, 0.9999), 0.0001)
  
  # Suppress plotting from val.prob.ci.2
  pdf(NULL)
  flex_curve <- val.prob.ci.2(df$probability, df$outcome1)
  dev.off()
  
  # Return tibble for plotting
  tibble(
    x = flex_curve$CalibrationCurves$FlexibleCalibration$x,
    y = flex_curve$CalibrationCurves$FlexibleCalibration$y,
    source_file = file_name
  )
}

final_results_df <- final_results_df %>%
  mutate(source_name = str_extract(basename(source_file), "^[^\\.]+"))

calibration_curves <- final_results_df %>%
  group_by(source_name) %>%
  group_split() %>%
  map_dfr(~ calculate_flexible_curve(.x, unique(.x$source_name)))
 
ggplot(calibration_curves, aes(x = x, y = y, color = source_file)) +
  geom_line(linewidth = 0.6) +
  scale_color_manual(
    name = "Dataset",
    values = c(
      "results_5_5_100_FALSE" = "lightsteelblue1",            
      "results_5_5_100_TRUE" = "thistle1",            
      "results_5_5_100_FALSE_prev_large" = "palegreen1",   
      "results_5_5_100_FALSE_prev_small" = "wheat1",  
      "results_5_5_500_FALSE" = "lightsteelblue2",            
      "results_5_5_500_TRUE" = "thistle2",              
      "results_5_5_500_FALSE_prev_large" = "palegreen2",  
      "results_5_5_500_FALSE_prev_small" = "wheat2",  
      "results_5_5_1000_FALSE" = "lightsteelblue3",            
      "results_5_5_1000_TRUE" = "thistle3",             
      "results_5_5_1000_FALSE_prev_large" = "palegreen3", 
      "results_5_5_1000_FALSE_prev_small" = "wheat3",
      "results_8_2_100_FALSE" = "#0C4842",            
      "results_8_2_100_TRUE" = "#8F77B5",            
      "results_8_2_100_FALSE_prev_large" = "palegreen4",   
      "results_8_2_100_FALSE_prev_small" = "wheat4",  
      "results_8_2_500_FALSE" = "#336774",            
      "results_8_2_500_TRUE" = "#77428D",              
      "results_8_2_500_FALSE_prev_large" = "#808F7C",  
      "results_8_2_500_FALSE_prev_small" = "#B4A582",  
      "results_8_2_1000_FALSE" = "#006284",            
      "results_8_2_1000_TRUE" = "#B481BB",             
      "results_8_2_1000_FALSE_prev_large" = "#91B493", 
      "results_8_2_1000_FALSE_prev_small" = "#F6C555"  
      
    ),
    labels = c(
      "results_5_5_100_FALSE" = "5 vs 5 - 100 pat/center - no higher order variable",
      "results_5_5_100_TRUE" = "5 vs 5 - 100 pat/center - prev full dataset",
      "results_5_5_100_FALSE_prev_large" = "5 vs 5 - 100 pat/center - prev full training set",
      "results_5_5_100_FALSE_prev_small" = "5 vs 5 - 100 pat/center - prev small training set",
      "results_5_5_500_FALSE" = "5 vs 5 - 500 pat/center - no higher order variable",
      "results_5_5_500_TRUE" = "5 vs 5 - 500 pat/center - prev full dataset",
      "results_5_5_500_FALSE_prev_large" = "5 vs 5 - 500 pat/center - prev full training set",
      "results_5_5_500_FALSE_prev_small" = "5 vs 5 - 500 pat/center - prev small training set",
      "results_5_5_1000_FALSE" = "5 vs 5 - 1000 pat/center - no higher order variable",
      "results_5_5_1000_TRUE" = "5 vs 5 - 1000 pat/center - prev full dataset",
      "results_5_5_1000_FALSE_prev_large" = "5 vs 5 - 1000 pat/center - prev full training set",
      "results_5_5_1000_FALSE_prev_small" = "5 vs 5 - 1000 pat/center - prev small training set",
      "results_8_2_100_FALSE" = "8 vs 2 - 100 pat/center - no higher order variable",
      "results_8_2_100_TRUE" = "8 vs 2 - 100 pat/center - prev full dataset",
      "results_8_2_100_FALSE_prev_large" = "8 vs 2 - 100 pat/center - prev full training set",
      "results_8_2_100_FALSE_prev_small" = "8 vs 2 - 100 pat/center - prev small training set",
      "results_8_2_500_FALSE" = "8 vs 2 - 500 pat/center - no higher order variable",
      "results_8_2_500_TRUE" = "8 vs 2 - 500 pat/center - prev full dataset",
      "results_8_2_500_FALSE_prev_large" = "8 vs 2 - 500 pat/center - prev full training set",
      "results_8_2_500_FALSE_prev_small" = "8 vs 2 - 500 pat/center - prev small training set",
      "results_8_2_1000_FALSE" = "8 vs 2 - 1000 pat/center - no higher order variable",
      "results_8_2_1000_TRUE" = "8 vs 2 - 1000 pat/center - prev full dataset",
      "results_8_2_1000_FALSE_prev_large" = "8 vs 2 - 1000 pat/center - prev full training set",
      "results_8_2_1000_FALSE_prev_small" = "8 vs 2 - 1000 pat/center - prev small training set"
      
    )) + 
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Flexible Calibration Curves per File",
       x = "Predicted Probability",
       y = "Observed Proportion") +
  theme_minimal() +
  theme(legend.position = "none", legend.title = element_blank())

### try to separate the plots ###

# Filter data for 5vs5 and 8vs2 cases
calibration_curves_5vs5 <- calibration_curves %>%
  filter(grepl("5_5", source_file))

calibration_curves_8vs2 <- calibration_curves %>%
  filter(grepl("8_2", source_file))

# Plot for 5vs5 cases
plot_5vs5 <- ggplot(calibration_curves_5vs5, aes(x = x, y = y, color = source_file)) +
  geom_line(linewidth = 0.6) +
  scale_color_manual(
    name = "Dataset",
    values = c(
      "results_5_5_100_FALSE" = "lightsteelblue1",             
      "results_5_5_100_TRUE" = "thistle1",             
      "results_5_5_100_FALSE_prev_large" = "palegreen1",   
      "results_5_5_100_FALSE_prev_small" = "wheat1",   
      "results_5_5_500_FALSE" = "lightsteelblue2",             
      "results_5_5_500_TRUE" = "thistle2",               
      "results_5_5_500_FALSE_prev_large" = "palegreen3",   
      "results_5_5_500_FALSE_prev_small" = "wheat2",   
      "results_5_5_1000_FALSE" = "lightsteelblue3",             
      "results_5_5_1000_TRUE" = "thistle3",             
      "results_5_5_1000_FALSE_prev_large" = "palegreen4", 
      "results_5_5_1000_FALSE_prev_small" = "wheat3"
    ),
    labels = c(
      "results_5_5_100_FALSE" = "5 vs 5 - 100 pat/center - no higher order variable",
      "results_5_5_100_TRUE" = "5 vs 5 - 100 pat/center - prev full dataset",
      "results_5_5_100_FALSE_prev_large" = "5 vs 5 - 100 pat/center - prev full training set",
      "results_5_5_100_FALSE_prev_small" = "5 vs 5 - 100 pat/center - prev small training set",
      "results_5_5_500_FALSE" = "5 vs 5 - 500 pat/center - no higher order variable",
      "results_5_5_500_TRUE" = "5 vs 5 - 500 pat/center - prev full dataset",
      "results_5_5_500_FALSE_prev_large" = "5 vs 5 - 500 pat/center - prev full training set",
      "results_5_5_500_FALSE_prev_small" = "5 vs 5 - 500 pat/center - prev small training set",
      "results_5_5_1000_FALSE" = "5 vs 5 - 1000 pat/center - no higher order variable",
      "results_5_5_1000_TRUE" = "5 vs 5 - 1000 pat/center - prev full dataset",
      "results_5_5_1000_FALSE_prev_large" = "5 vs 5 - 1000 pat/center - prev full training set",
      "results_5_5_1000_FALSE_prev_small" = "5 vs 5 - 1000 pat/center - prev small training set"
    )
  ) +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  labs(x = "Predicted Probability",
       y = "Observed Proportion") +
  theme_minimal() + #+
  theme(legend.position = "none", legend.title = element_blank())

# Reorder the factor levels in the dataset for the variable used in color
calibration_curves_8vs2$source_file <- factor(
  calibration_curves_8vs2$source_file,
  levels = c(
    "results_8_2_100_FALSE",
    "results_8_2_500_FALSE",
    "results_8_2_1000_FALSE",
    "results_8_2_100_TRUE",
    "results_8_2_500_TRUE",
    "results_8_2_1000_TRUE",
    "results_8_2_100_FALSE_prev_large",
    "results_8_2_500_FALSE_prev_large",
    "results_8_2_1000_FALSE_prev_large",
    "results_8_2_100_FALSE_prev_small",
    "results_8_2_500_FALSE_prev_small",
    "results_8_2_1000_FALSE_prev_small"
  )
)

# Plot for 8vs2 cases
plot_8vs2 <- ggplot(calibration_curves_8vs2, aes(x = x, y = y, color = source_file)) +
  geom_line(linewidth = 0.6) +
  scale_color_manual(
    name = "Dataset",
    values = c(
      "results_8_2_100_FALSE" = "lightsteelblue1",             
      "results_8_2_100_TRUE" = "thistle1",             
      "results_8_2_100_FALSE_prev_large" = "palegreen1",   
      "results_8_2_100_FALSE_prev_small" = "wheat1",   
      "results_8_2_500_FALSE" = "lightsteelblue2",             
      "results_8_2_500_TRUE" = "thistle2",               
      "results_8_2_500_FALSE_prev_large" = "palegreen3",   
      "results_8_2_500_FALSE_prev_small" = "wheat2",   
      "results_8_2_1000_FALSE" = "lightsteelblue3",             
      "results_8_2_1000_TRUE" = "thistle3",             
      "results_8_2_1000_FALSE_prev_large" = "palegreen4", 
      "results_8_2_1000_FALSE_prev_small" = "wheat3"
    ),
    labels = c(
      "results_8_2_100_FALSE" = "100 pat/center - no higher order variable",
      "results_8_2_500_FALSE" = "500 pat/center - no higher order variable",
      "results_8_2_1000_FALSE" = "1000 pat/center - no higher order variable",
      "results_8_2_100_TRUE" = "100 pat/center - prev full dataset",
      "results_8_2_500_TRUE" = "500 pat/center - prev full dataset",
      "results_8_2_1000_TRUE" = "1000 pat/center - prev full dataset",
      "results_8_2_100_FALSE_prev_large" = "100 pat/center - prev full training set",
      "results_8_2_500_FALSE_prev_large" = "500 pat/center - prev full training set",
      "results_8_2_1000_FALSE_prev_large" = "1000 pat/center - prev full training set",
      "results_8_2_100_FALSE_prev_small" = "100 pat/center - prev small training set",
      "results_8_2_500_FALSE_prev_small" = "500 pat/center - prev small training set",
      "results_8_2_1000_FALSE_prev_small" = "1000 pat/center - prev small training set"
    )
  ) +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  labs(x = "Predicted Probability",
       y = "Observed Proportion") +
  theme_minimal() +
  theme(legend.position = "none", legend.title = element_blank())

# Print both plots
plot_5vs5
plot_8vs2

# plot legend
# Extract the legend 
legend <- get_legend(plot_8vs2)
plot_grid(legend)
