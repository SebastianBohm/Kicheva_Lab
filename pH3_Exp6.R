# Load necessary libraries
library(ggplot2)
library(dplyr)
library(stringr)

# Reading the dataset
data1 <- read.table("C:/Users/SebastianBoehm/OneDrive/Dokumente/A_Kicheva/Raw_data/Imaging_06_justMMC/Exp6_CellPro/CellProlif_Batch.icsx_[ibrx_2024-06-14T08-55-56.878]_Volume.csv", header = TRUE, sep = ",")

# Filter out rows where time is 72 or 216
data1 <- data1 %>% filter(!(time %in% c(72, 216)))

# Group by Original.Image.Name and calculate the count of each group
data1 <- data1 %>%
  group_by(Original.Image.Name) %>%
  mutate(Count = n()) %>%
  ungroup()

# Remove duplicate rows based on all columns
data1 <- data1 %>% distinct()

# Summing Volume for each dataset and retaining the 'Count' column
combined_data <- data1 %>%
  group_by(Original.Image.Name, time, Count) %>%  # Retain Count while summarizing Volume
  summarise(Volume = sum(Volume)) %>%
  ungroup()

# Duplicate rows where time = 98 and modify the Original.Image.Name to "mmc0.2"
duplicate_98 <- combined_data %>% filter(time == 98) %>% mutate(Original.Image.Name = "mmc0.2")

combined_data <- data1 %>% mutate(Original_Name = Original.Image.Name)

# Add the duplicated rows back to the original data
combined_data <- bind_rows(combined_data, duplicate_98)

# Apply renaming directly to Original.Image.Name for different conditions
combined_data$Original.Image.Name <- ifelse(grepl("control", combined_data$Original.Image.Name, ignore.case = TRUE), "Control",
                                            ifelse(grepl("mmc0.2", combined_data$Original.Image.Name, ignore.case = TRUE), "MMC0.2",
                                                   ifelse(grepl("^mmc0.3", combined_data$Original.Image.Name), "MMC0.3",
                                                          ifelse(grepl("^mmc0.5", combined_data$Original.Image.Name), "MMC0.5",
                                                                 combined_data$Original.Image.Name))))
combined_data <- combined_data %>%
  filter(!(Count > 1500 & time == 98))

# Rename Original.Image.Name to Condition
combined_data <- combined_data %>% rename(Condition = Original.Image.Name)

# Summary statistics for count (mean and SD)
summary_stats_count <- combined_data %>%
  group_by(Condition, time) %>%
  summarise(mean_value = mean(Count, na.rm = TRUE),
            sd_value = sd(Count, na.rm = TRUE),
            n = n())


# Calculate confidence intervals (CI) for count
summary_stats_count <- summary_stats_count %>%
  mutate(ci_lower = mean_value - qt(0.975, df = n - 1) * sd_value / sqrt(n),
         ci_upper = mean_value + qt(0.975, df = n - 1) * sd_value / sqrt(n))


# Print the final data
print(summary_stats_count)


my_plot <- ggplot(summary_stats_count, aes(x = factor(time, levels = c(72, 98, 120, 144, 168, 192)), y = mean_value, color = Condition, group = Condition)) + 
  geom_line(size=2) + 
  geom_point(size=4) + 
  theme_classic(base_size = 25) + 
  labs(title = "", 
       x = "Time (hours)", 
       y = "Count (pH3 positive cells)") + 
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, size=1) + 
  scale_x_discrete(labels = c("72" = "-24h", "98" = "0h", "120" = "24h", "144" = "48h", "168" = "72h", "192"="96h")) +
  scale_y_continuous(labels = scales::comma_format(), limits = c(0, NA)) +  
  scale_color_manual(values = c("Control" = "#0072B2", "MMC0.2" = "#E69F00", "MMC0.3" = "PINK", "MMC0.5"="forestGreen")) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +  
  geom_point(data = combined_data, aes(x = factor(time, levels = c(72, 98, 120, 144, 168, 192)), y = Count, color = Condition, group = Condition), size = 1.5, alpha = 0.5, position = position_jitter(width = 0.1, height = 0))

# Save the plot as a jpg file
ggsave("C:/Users/SebastianBoehm/Downloads/Absolute_Count_Exp6_02.tif", plot = my_plot, width = 10, height = 7, dpi = 300)



# Merge the datasets by time for Control and MMC0.2 to align rows
merged_data <- merge(
  subset(summary_stats_count, Condition == "Control"),
  subset(summary_stats_count, Condition == "MMC0.2"),
  by = "time",
  suffixes = c("_control", "_mmc0.2")
)

# Calculate normalized values and percentage difference
control_mean <- mean(merged_data$mean_value_control)
mmc0.2_mean <- mean(merged_data$mean_value_mmc0.2)

merged_data$normalized_value_control <- (merged_data$mean_value_control - control_mean) / control_mean * 100
merged_data$normalized_value_mmc0.2 <- (merged_data$mean_value_mmc0.2 - mmc0.2_mean) / mmc0.2_mean * 100

merged_data$perc_diff_control <- merged_data$mean_value_mmc0.2 / merged_data$mean_value_control

# Calculate error propagation
merged_data$error_propagation <- merged_data$perc_diff_control * sqrt((merged_data$sd_value_mmc0.2 / merged_data$mean_value_mmc0.2)^2 + (merged_data$sd_value_control / merged_data$mean_value_control)^2)

# Plotting the normalized count with significance stars
my_plot <- ggplot(merged_data, aes(x = factor(time), y = perc_diff_control, group = 1)) +
  geom_line(size = 2, color = "#E69F00") +
  geom_point(size = 4, color = "#E69F00") +
  theme_classic(base_size = 25) +
  geom_errorbar(aes(ymin = perc_diff_control - error_propagation, ymax = perc_diff_control + error_propagation), 
                width = 0.2, color = "#E69F00", size = 1) +
  labs(x = "Time (hours)", y = "Normalized (Count)") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "#0072B2", size = 1) +
  scale_x_discrete(labels = c("72" = "-24h", "98" = "0h", "120" = "24h", "144" = "48h", "168" = "72h", "192" = "96h")) +
  expand_limits(y = 0) +
  theme(legend.position = "bottom") +
  labs(caption = "  ")

# Save the normalized count plot as a jpeg file
ggsave("C:/Users/SebastianBoehm/Downloads/Normalized_Count_exp6.tif", plot = my_plot, width = 10, height = 7, dpi = 300)

# Save the combined_data DataFrame as a CSV file
write.csv(combined_data, "C:/Users/SebastianBoehm/OneDrive/Dokumente/A_Kicheva/HM1/Proliferation/combined_data_Exp6.csv", row.names = FALSE)







