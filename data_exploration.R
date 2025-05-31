#### script for data exploration ####
load("/Users/ebbablomdahl/Thesis/Code/IOTAsynthpop_data3_prepared.RData")
library(dplyr)
library(ggplot2)
library(patchwork)

### over all data exploration ###

# Create missing indicator
df <- all_data %>%
  mutate(lCA125_missing = is.na(lCA125))

# outcome (prevalence) function of type of center and center 

# Define plots
p1 <- df %>%
  group_by(oncocenter) %>%
  summarise(p_prev = mean(outcome1) * 100) %>%
  ggplot(aes(x = factor(oncocenter), y = p_prev, fill = factor(oncocenter))) +
  geom_col() +
  # geom_col(fill = "lightsteelblue1") +
  labs(x = "Type of Center", y = "Prevalence (%)", fill = 'Type of center') +
  scale_x_discrete(labels = c("0" = "Non-Oncology Center", "1" = "Oncology Center")) + 
  scale_fill_manual(values = c("lightsteelblue1", "thistle"), labels = c("Non-oncology center", "Oncology center"))  # choose colors


p2 <- df %>%
  group_by(center) %>%
  summarise(p_prev = mean(outcome1) * 100) %>%
  ggplot(aes(x = factor(center), y = p_prev)) +
  geom_col(fill = "steelblue") +
  labs(x = "Center", y = "Prevalence") +
  theme(axis.text.x = element_text(angle = 90))

p2 <- df %>%
  group_by(center, oncocenter) %>%  # group by both
  summarise(p_prev = mean(outcome1) * 100, .groups = 'drop') %>%
  ggplot(aes(x = reorder(factor(center), p_prev), y = p_prev, fill = factor(oncocenter))) + 
  geom_col() +
  labs(x = "Center", y = "Prevalence (%)", fill = "Type of center") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = c("lightsteelblue1", "thistle"), labels = c("Non-oncology center", "Oncology center"))  # choose colors

# order previous one according to prevalence and add box with type of center prevalence 

# Calculate overall prevalence by oncocenter
prev_summary <- df %>%
  group_by(oncocenter) %>%
  summarise(p = mean(outcome1) * 100)

# Extract prevalence values
prev_onco <- round(prev_summary$p[prev_summary$oncocenter == 1], 1)
prev_nononco <- round(prev_summary$p[prev_summary$oncocenter == 0], 1)

# Combine into text
text_label <- paste0("Prevalence:\n",
                     "Oncology centers: ", prev_onco, "%\n",
                     "Non-oncology centers: ", prev_nononco, "%")

# plot
p2 <- df %>%
  group_by(center, oncocenter) %>%
  summarise(p_prev = mean(outcome1) * 100, .groups = 'drop') %>%
  ggplot(aes(x = reorder(factor(center), p_prev), y = p_prev, fill = factor(oncocenter))) +
  geom_col() +
  labs(x = "Center", y = "Prevalence (%)", fill = "Type of center") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  scale_fill_manual(values = c("lightsteelblue1", "thistle"),
                    labels = c("Non-oncology center", "Oncology center")) +
  # annotate("text", x = Inf, y = Inf, label = text_label, hjust = 1.1, vjust = 1.1,
  #          size = 4, fontface = "italic", color = "black")
  # annotate("label", x = Inf, y = Inf, label = text_label, hjust = 1.1, vjust = 1.1,
  #                    size = 4, fill = "white", label.size = 0.5)
  annotate("label",
           x = 2, y = max(df$outcome1) * 70,  # Adjust x and y for position
           label = text_label,
           hjust = 0,  # left-align text
           vjust = 1,  # top-align vertically
           size = 4,
           fill = "white", label.size = 0.5)

p2

# Combine plots side by side
(p1 | p2) + 
  plot_annotation(title = "Prevalence of Malignant Tomour by Type of Center and Center")


#### missing data plots ####

df %>%
  group_by(oncocenter) %>%
  summarise(n_missing = sum(lCA125_missing)) %>%
  ggplot(aes(x = factor(oncocenter), y = n_missing)) +
  geom_col(fill = "steelblue") +
  labs(x = "Echogenicity", y = "Count of Missing lCA125")

# maybe best to do percentage
# interesting parameters: oncocenter, center, outcome1
df %>%
  group_by(oncocenter) %>%
  summarise(p_missing = mean(lCA125_missing) * 100) %>%
  ggplot(aes(x = factor(oncocenter), y = p_missing)) +
  geom_col(fill = "darkorange") +
  labs(x = "Echogenicity", y = "% Missing lCA125")

df %>%
  mutate(age_bin = cut(Age, breaks = seq(10, 90, by = 10))) %>%
  group_by(age_bin) %>%
  summarise(n_missing = sum(lCA125_missing)) %>%
  ggplot(aes(x = age_bin, y = n_missing)) +
  geom_col(fill = "seagreen") +
  labs(x = "Age Bin", y = "Count of Missing lCA125") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# interesting parameters: age, llesdmax, 
df %>%
  mutate(age_bin = cut(lCAmi, breaks = seq(-0.5, 40, by = 0.5))) %>%
  group_by(age_bin) %>%
  summarise(p_missing = mean(lCA125_missing) * 100) %>%
  ggplot(aes(x = age_bin, y = p_missing)) +
  geom_col(fill = "firebrick") +
  labs(x = "Age Bin", y = "% Missing lCA125") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# plot fo the most interesting ones maybe 

# Define plots
p1 <- df %>%
  group_by(oncocenter) %>%
  summarise(p_missing = mean(lCA125_missing) * 100) %>%
  ggplot(aes(x = factor(oncocenter), y = p_missing, fill = factor(oncocenter))) +
  geom_col() + 
  # geom_col(fill = "steelblue") +
  labs(x = "Type of Center", y = "Missing Values (%)", fill = "Type of center") +
  scale_x_discrete(labels = c("0" = "Non-Oncology Center", "1" = "Oncology Center")) + 
  scale_fill_manual(values = c("lightsteelblue1", "thistle"), labels = c("Non-oncology center", "Oncology center"))  # choose colors


p2 <- df %>%
  group_by(center, oncocenter) %>%
  summarise(p_missing = mean(lCA125_missing) * 100, .groups = 'drop') %>%
  ggplot(aes(x = factor(center), y = p_missing, fill = factor(oncocenter))) +
  geom_col() +
  # geom_col(fill = "steelblue") +
  labs(x = "Center", y = "Missing Values (%)", fill = 'Type of center') +
  theme(axis.text.x = element_text(angle = 90)) + 
  scale_fill_manual(values = c("lightsteelblue1", "thistle"), labels = c("Non-oncology center", "Oncology center"))  # choose colors

p3 <- df %>%
  group_by(outcome1) %>%
  summarise(p_missing = mean(lCA125_missing) * 100) %>%
  ggplot(aes(x = factor(outcome1), y = p_missing, fill = factor(outcome1))) +
  geom_col() +
  theme_minimal() + 
  labs(x = "Outcome", y = "Missing Values (%)", fill = 'Outcome') + 
  scale_x_discrete(labels = c("0" = "Benign", "1" = "Malignant")) + 
  scale_fill_manual(values = c("palegreen3", "wheat1"), labels = c("Benign", "Malignant"))  # choose colors


# Combine plots side by side
(p1 | p2 | p3) + 
  plot_annotation(title = "Percentage of Missing lCA125 Values by Oncology Center, Center, and Outcome")

### add plot 1 as text in plot 3 ###
# Calculate overall prevalence by oncocenter
missing_summary <- df %>%
  group_by(oncocenter) %>%
  summarise(m = mean(lCA125_missing) * 100)

# Extract prevalence values
missing_onco <- round(missing_summary$m[missing_summary$oncocenter == 1], 1)
missing_nononco <- round(missing_summary$m[missing_summary$oncocenter == 0], 1)

# Combine into text
text_label <- paste0("Percentage of missing values:\n",
                     "Oncology centers: ", missing_onco, "%\n",
                     "Non-oncology centers: ", missing_nononco, "%")

p4 <- df %>%
  group_by(center, oncocenter) %>%
  summarise(p_missing = mean(lCA125_missing) * 100, .groups = 'drop') %>%
  ggplot(aes(x = reorder(factor(center), p_missing), y = p_missing, fill = factor(oncocenter))) +
  geom_col() +
  labs(x = "Center", y = "Missing Values (%)", fill = "Type of center") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = c("lightsteelblue1", "thistle"),
                    labels = c("Non-oncology center", "Oncology center")) +
  annotate("label",
           x = 2, y = max(df$outcome1) * 90,  # Adjust x and y for position
           label = text_label,
           hjust = 0,  # left-align text
           vjust = 1,  # top-align vertically
           size = 4,
           fill = "white", label.size = 0.5)

p4

#### for table #### 

# how many of each kind of

sum(is.na(all_data$CA125))

median(all_data$Age[all_data$center == "SSW"], na.rm = TRUE)
quantile(all_data$Age[all_data$center == "SSW"], probs = c(0.25, 0.75), na.rm = TRUE)
median(all_data$CA125[all_data$center == "BI0"], na.rm = TRUE)
quantile(all_data$CA125[all_data$center == "BI0"], probs = c(0.25, 0.75), na.rm = TRUE)

IQR(all_data$CA125[all_data$outcome1 == 1], na.rm = TRUE)


sum(all_data$papflow == 1 & all_data$outcome1 == 1, na.rm = TRUE)



missing <- df %>%
  group_by(outcome1) %>%
  summarise(p_missing = mean(lCA125_missing) * 100, .groups = 'drop')







