#### script to calculate the net benefit - new way ####
library(data.table)
library(stringr)
library(ggplot2)
library(forcats)
library(cowplot)
library(dplyr)


### import data ###
# List all dataset file paths
# define folder where the data is located 
folder_path <- "/Users/ebbablomdahl/Thesis/Thesis/Results/random_forest"

# Define which sets to import and change depenidign on which comparisment is wanted 
pattern <- "^results_.*\\.csv$"

# List all files matching the pattern
file_list <- list.files(path = folder_path, pattern = pattern, full.names = TRUE)

# Extract dataset parameters from filenames
dataset_params <- str_extract(basename(file_list), "^[^\\.]+")  # Extracts filename without extension

calc_net_benefit <- function(df, dataset_name){
  threshold <- 0.1
  models <- paste0("Model_", 1:100)
  results <- data.table()
  
  # for random forest 
  # df$outcome1 <- ifelse(df$outcome1 == "X1", 1, 0)
  
  # for each model 
  for(j in 1:100){
    prob_col <- paste0("probabilities_", j)
    # for each center 
    for(c in unique(df$center)){
      subdata <- df[center == c]  # Subset data for center
      probs <- subdata[[prob_col]]
      outcome <- subdata$outcome1
      
      # calculate the net benefit per center 
      N <- length(outcome)
      pred_pos <- probs >= threshold
      TP <- sum(pred_pos & (outcome == 1))
      FP <- sum(pred_pos & (outcome == 0))
      net_benefit_value <- (TP/N) - (FP/N) * (threshold/(1-threshold))
      
      results <- rbind(results, data.table(
        Dataset = dataset_name, 
        Model = models[j],
        center = c,
        NB = net_benefit_value
      ))
      
    }
  }
  return(results)
}


# Run for all files
all_net_benefits <- rbindlist(lapply(seq_along(file_list), function(i) {
  cat("\nProcessing file:", i, "-", file_list[i], "\n")
  df <- fread(file_list[i])
  calc_net_benefit(df, dataset_params[i])
}))

# save the net benefit 
# write.csv(all_net_benefits, "/Users/ebbablomdahl/Thesis/Thesis/Results/net_benefits/net_benefit_xgboost.csv", row.names = FALSE)
all_net_benefits <- read.csv("/Users/ebbablomdahl/Thesis/Thesis/Results/net_benefits/net_benefit_xgboost.csv")
all_net_benefits <- as.data.table(all_net_benefits)

### the net benefit over all centers ###

# IQR of NB across centers for each run (Dataset + Model)
nb_iqr_by_run <- all_net_benefits[, .(NB_IQR = IQR(NB)), by = .(Dataset, Model)]


#### final plot ####

# Add Category: 100 / 500 / 1000
all_nb <- nb_iqr_by_run %>%
  filter(str_detect(Dataset, "^results_5_5_100_|^results_5_5_500_|^results_5_5_1000_|^results_8_2_100_|^results_8_2_500_|^results_8_2_1000_")) %>%
  mutate(
    size_group = case_when(
      str_detect(Dataset, "_100_") ~ "100",
      str_detect(Dataset, "_500_") ~ "500",
      str_detect(Dataset, "_1000_") ~ "1000"
    ),
    base_group = case_when(
      str_detect(Dataset, "^results_5_5_") ~ "5_5",
      str_detect(Dataset, "^results_8_2_") ~ "8_2"
    ),
    ending_type = case_when(
      str_ends(Dataset, "_FALSE") ~ "_FALSE",
      str_ends(Dataset, "_TRUE") ~ "_TRUE",
      str_ends(Dataset, "_prev_large") ~ "_prev_large",
      str_ends(Dataset, "_prev_small") ~ "_prev_small",
      TRUE ~ "Other"
    ),
    # Combine base + size to make facets
    facet_group = paste0(base_group, "_", size_group),
    
    # Order factors for plotting
    facet_group = factor(facet_group, levels = c(
      "5_5_100", "5_5_500", "5_5_1000",
      "8_2_100", "8_2_500", "8_2_1000"
    )),
    ending_type = factor(ending_type, levels = c("_FALSE", "_TRUE", "_prev_large", "_prev_small"))
  )



all_nb <- all_nb %>%
  mutate(
    facet_group = fct_recode(facet_group,
                             "5 Onco vs. 5 Non-onco - 100 patients/center" = "5_5_100",
                             "5 Onco vs. 5 Non-onco - 500 patients/center" = "5_5_500",
                             "5 Onco vs. 5 Non-onco - 1000 patients/center" = "5_5_1000",
                             "8 Onco vs. 2 Non-onco - 100 patients/center" = "8_2_100",
                             "8 Onco vs. 2 Non-onco - 500 patients/center" = "8_2_500",
                             "8 Onco vs. 2 Non-onco - 1000 patients/center" = "8_2_1000"
    )
  )

all_nb <- all_nb %>%
  mutate(
    dataset_label = case_when(
      str_ends(Dataset, "_FALSE") ~ "No higher-order variable",
      str_ends(Dataset, "_TRUE") ~ "Type of center",
      str_ends(Dataset, "_prev_large") ~ "Prevalence full training set",
      str_ends(Dataset, "_prev_small") ~ "Prevalence small training set",
      TRUE ~ Dataset  # fallback
    ),
    dataset_label = factor(dataset_label, levels = c(
      "No higher-order variable",
      "Type of center",
      "Prevalence full training set",
      "Prevalence small training set"
    ))
  )


# Get unique facet groups
facet_levels <- unique(all_nb$facet_group)
#facet_levels <- facet_levels[5]
# Loop through each facet group and plot
for (group in facet_levels) {
  df_subset <- filter(all_nb, facet_group == group)
  
  p <- ggplot(df_subset, aes(x = dataset_label, y = NB_IQR, fill = ending_type)) +
    geom_boxplot() +
    scale_fill_manual(
      values = c(
        "_FALSE" = "lightsteelblue1",    
        "_TRUE" = "thistle",       
        "_prev_large" = "palegreen3", 
        "_prev_small" = "wheat1"
      ),
      labels = c(
        "_FALSE" = "No higher-order variable",
        "_TRUE" = "Type of center",
        "_prev_large" = "Prevalence full training set",
        "_prev_small" = "Prevalence small training set"
      ),
      name = "Higher-order variable"
    ) +
    labs(x = "Design", y = "Inter Quartile Range - Net Benefit", title = "none") +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(), 
      axis.title.x = element_blank(), 
      plot.title = element_blank(),
      legend.position = "none"
    ) + 
    ylim(0.13, 0.23)
  
  print(p)  # Display the plot
}
