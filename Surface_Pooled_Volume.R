# Load necessary libraries
library(ggplot2)
library(dplyr)
library(stringr)

# Reading the datasets
data1 <- read.table("C:/Users/SebastianBoehm/OneDrive/Dokumente/A_Kicheva/HM1/Volume/combined_data_Exp2.csv", header = TRUE, sep = ",")
data2 <- read.table("C:/Users/SebastianBoehm/OneDrive/Dokumente/A_Kicheva/HM1/Volume/combined_data_Exp3.csv", header = TRUE, sep = ",")
data3 <- read.table("C:/Users/SebastianBoehm/OneDrive/Dokumente/A_Kicheva/HM1/Volume/combined_data_Exp4.csv", header = TRUE, sep = ",")
data4 <- read.table("C:/Users/SebastianBoehm/OneDrive/Dokumente/A_Kicheva/HM1/Volume/combined_data_Exp5.csv", header = TRUE, sep = ",")
data5 <- read.table("C:/Users/SebastianBoehm/OneDrive/Dokumente/A_Kicheva/HM1/Volume/combined_data_Exp6.csv", header = TRUE, sep = ",")

# Assuming your data frames are named data1, data2, data3, and data5
# and you want to reorder columns to the order: Condition, Volume, time, Count, Original_Name

# For data1
data1 <- data1[, c("Condition", "Volume", "time", "Original_Name")]

# For data2
data2 <- data2[, c("Condition", "Volume", "time", "Original_Name")]

# For data3
data3 <- data3[, c("Condition", "Volume", "time", "Original_Name")]

# For data5
data5 <- data5[, c("Condition", "Volume", "time", "Original_Name")]

combined_data <- rbind(data1, data2, data3, data4, data5)

combined_data <- combined_data %>% filter(!(Condition %in% c("MMC0.3", "MMC0.5")))

# Assuming combined_data is your data frame and it has columns 'time' and 'condition'

# Filter rows where time is 98
time_98_rows <- which(combined_data$time == 98)

# Randomly assign "MMC0.2" or "Control" to those rows
combined_data$Condition[time_98_rows] <- sample(c("MMC0.2", "Control"), length(time_98_rows), replace = TRUE)

# Check the updated data for time 98
combined_data[combined_data$time == 98, ]


# Print the renamed data
print(combined_data)

filtered_data <- combined_data[combined_data$time == 168 & combined_data$Condition == "Control", ]

# Plot the histogram
options(scipen = 999)
hist(filtered_data$Volume, 
     main = "Histogram of Surface Volume (time=72 + Condition=Control)", 
     xlab = "Volume", 
     col = "#0072B2", 
     border = "black", 
     breaks = 20,
     xlim = range(filtered_data$Volume))

# Calculate confidence intervals (CI) for volume
summary_stats_volume <- summary_stats_volume %>%
  mutate(ci_lower = mean_value - qt(0.975, df = n - 1) * sd_value / sqrt(n),
         ci_upper = mean_value + qt(0.975, df = n - 1) * sd_value / sqrt(n))

# Remove the row where Condition is MMC0.2 and time is 72
summary_stats_volume <- summary_stats_volume %>%
  filter(!(Condition == "MMC0.2" & time == 72))

# Define and save the plot with error bars and confidence intervals
my_plot <- ggplot(summary_stats_volume, aes(x = factor(time, levels = c(72, 98, 120, 144, 168, 192)), y = mean_value, color = Condition, group = Condition)) + 
  geom_line(size=2) + 
  geom_point(size=4) + 
  theme_classic(base_size = 25) + 
  labs(title = "Colonie growth (Sox2)", 
       x = "Time (hours)", 
       y = "Volume (µm³)") + 
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, size=1) + 
  scale_x_discrete(labels = c("72" = "-24h", "98" = "0h", "120" = "24h", "144" = "48h", "168" = "72h", "192" = "96h")) +
  scale_y_continuous(labels = scales::comma_format(), limits = c(0, NA)) +  
  scale_color_manual(values = c("Control" = "#0072B2", "MMC0.2" = "#E69F00", "MMC0.3" = "PINK", "MMC0.5"="forestGreen")) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +  
  geom_point(data = combined_data, aes(x = factor(time, levels = c(72, 98, 120, 144, 168, 192)), y = Volume, color = Condition, group = Condition), size = 1.5, alpha = 0.2, position = position_jitter(width = 0.1, height = 0))

# Save the plot as a jpg file
ggsave("C:/Users/SebastianBoehm/Downloads/Absolute_Volume_Exp6_Sox2.tif", plot = my_plot, width = 10, height = 7, dpi = 300)

##### Normalized Plot (Volume)
# Calculate normalized values
filtered_data_control <- subset(summary_stats_volume, Condition == "Control")
filtered_data_mmc0.2 <- subset(summary_stats_volume, Condition == "MMC0.2")

filtered_data_control$time <- factor(filtered_data_control$time, levels = c(72, 98, 120, 144, 168, 192, 216))
filtered_data_mmc0.2$time <- factor(filtered_data_mmc0.2$time, levels = c(72, 98, 120, 144, 168, 192, 216))

control_mean <- mean(filtered_data_control$mean_value)
mmc0.2_mean <- mean(filtered_data_mmc0.2$mean_value)

filtered_data_control$normalized_value <- (filtered_data_control$mean_value - control_mean) / control_mean * 100
filtered_data_mmc0.2$normalized_value <- (filtered_data_mmc0.2$mean_value - mmc0.2_mean) / mmc0.2_mean * 100
# Merge the control and MMC0.2 data by 'time' to ensure alignment
merged_data <- merge(filtered_data_control, filtered_data_mmc0.2, by = "time", suffixes = c("_control", "_mmc0.2"))

# Calculate the percentage difference between MMC0.2 and Control
merged_data$perc_diff_control <- merged_data$mean_value_mmc0.2 / merged_data$mean_value_control

# Calculate percentage change over time for both conditions
merged_data$perc_time_mmc0.2 <- (merged_data$mean_value_mmc0.2 - lag(merged_data$mean_value_mmc0.2)) / lag(merged_data$mean_value_mmc0.2) * 100
merged_data$perc_time_control <- (merged_data$mean_value_control - lag(merged_data$mean_value_control)) / lag(merged_data$mean_value_control) * 100

# Error propagation
merged_data$error_propagation <- merged_data$perc_diff_control * sqrt((merged_data$sd_value_mmc0.2 / merged_data$mean_value_mmc0.2)^2 + (merged_data$sd_value_control / merged_data$mean_value_control)^2)

# Create normalized count plot using the merged data
my_plot_normalized <- ggplot(merged_data, aes(x = factor(time), y = perc_diff_control, group = 1)) +
  geom_line(size=2, color="#E69F00") + 
  geom_point(size=4, color="#E69F00") + 
  theme_classic(base_size = 25) + 
  geom_line(color = "#E69F00") +
  geom_errorbar(
    aes(ymin = perc_diff_control - error_propagation, ymax = perc_diff_control + error_propagation), 
    width = 0.2, 
    color = "#E69F00",
    size=1
  ) +
  labs(
    x = "Time (hours)", 
    y = "Normalized (Volume)"
  ) +
  geom_errorbar(
    aes(ymin = 1 - error_propagation, ymax = 1 + error_propagation), 
    width = 0.2, 
    size=1,
    color = "#0072B2"
  ) +
  theme(
    legend.position = "bottom") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "#0072B2", size=1) + 
  scale_x_discrete(labels = c("72" ="-24h" ,"98" = "0h", "120" = "24h", "144" = "48h", "168" = "72h", "192" = "96h", "216"="120h")) +
  expand_limits(y = 0) + 
  labs(caption = "  ")

# Save the normalized count plot as a jpeg file
ggsave("C:/Users/SebastianBoehm/Downloads/Normalized_Surface_exp5.tif", plot = my_plot_normalized, width = 10, height = 7, dpi = 300)


# Save the combined_data DataFrame as a CSV file
write.csv(combined_data, "C:/Users/SebastianBoehm/OneDrive/Dokumente/A_Kicheva/HM1/Volume/combined_data_pooled.csv", row.names = FALSE)

























# Load necessary libraries
library(ggplot2)
library(dplyr)
library(stringr)

# Perform t-tests for each time point and condition using combined_data
stat_test_volume <- combined_data %>%
  filter(Condition %in% c("Control", "MMC0.2")) %>%  # Focus on Control vs MMC0.2
  group_by(time) %>%
  summarise(
    n_control = sum(Condition == "Control" & !is.na(Volume)),  # Count of non-NA values in Control
    n_mmc0.2 = sum(Condition == "MMC0.2" & !is.na(Volume)),    # Count of non-NA values in MMC0.2
    p_value = ifelse(n_control >= 2 & n_mmc0.2 >= 2,  # Only run t-test if both groups have at least 2 observations
                     t.test(Volume[Condition == "Control"], 
                            Volume[Condition == "MMC0.2"])$p.value, 
                     NA)  # If not enough data, return NA for p-value
  )

# Add significance labels based on p-values
stat_test_volume <- stat_test_volume %>%
  mutate(significance = case_when(
    !is.na(p_value) & p_value < 0.001 ~ "***",
    !is.na(p_value) & p_value < 0.01  ~ "**",
    !is.na(p_value) & p_value < 0.05  ~ "*",
    TRUE                              ~ ""
  ))

# Merge the significance test results with the combined_data by 'time'
combined_data <- combined_data %>%
  left_join(stat_test_volume, by = "time")

# Define and save the plot with error bars, confidence intervals, and significance markers
my_plot <- ggplot(combined_data, aes(x = factor(time, levels = c(72, 98, 120, 144, 168, 192)), y = Volume, color = Condition, group = Condition)) + 
  geom_line(aes(group = Condition), size=2) + 
  geom_point(size=4) + 
  theme_classic(base_size = 25) + 
  labs(title = "Colonie growth (Sox2)", 
       x = "Time (hours)", 
       y = "Volume (µm³)") + 
  geom_errorbar(aes(ymin = Volume - sd(Volume), ymax = Volume + sd(Volume)), width = 0.2, size=1) + 
  scale_x_discrete(labels = c("72" = "-24h", "98" = "0h", "120" = "24h", "144" = "48h", "168" = "72h", "192" = "96h")) +
  scale_y_continuous(labels = scales::comma_format(), limits = c(0, NA)) +  
  scale_color_manual(values = c("Control" = "#0072B2", "MMC0.2" = "#E69F00", "MMC0.3" = "PINK", "MMC0.5"="forestGreen")) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +  
  geom_point(aes(x = factor(time), y = Volume, color = Condition), size = 1.5, alpha = 0.5, position = position_jitter(width = 0.1, height = 0)) +
  
  # Add significance stars
  geom_text(aes(x = factor(time), y = max(Volume) + 0.05 * max(Volume), label = significance), 
            color = "black", size = 6)

# Save the plot as a jpg file
ggsave("C:/Users/SebastianBoehm/Downloads/Absolute_Volume_Exp6_Sox2_with_significance.tif", plot = my_plot, width = 10, height = 7, dpi = 300)






