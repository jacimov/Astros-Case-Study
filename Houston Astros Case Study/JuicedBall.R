## Ball Defect Analysis
# Load required libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(car)
library(readr)
library(gridExtra)
library(broom)
library(knitr)
library(scales)
library(cluster)
library(factoextra)

# Preprocess Data
preprocess_data <- function(df) {
  df %>%
    mutate(
      date = ymd(paste(year, month, "01", sep = "-")),
      across(c(release_speed, plate_speed, hit_exit_speed, hit_spinrate,
               hit_vertical_angle, hit_bearing, hit_distance), as.numeric),
      season = case_when(
        month %in% c(3, 4, 5) ~ "Spring",
        month %in% c(6, 7, 8) ~ "Summer",
        month %in% c(9, 10) ~ "Fall",
        TRUE ~ "Winter"  # Adding a default case for completeness
      )
    )
}

# Load and preprocess the data
df <- read_csv("data_sample.csv")
df <- preprocess_data(df)

# Exploratory Data Analysis
perform_eda <- function(df) {
  print(summary(df))
  print(df %>% group_by(year) %>% summarise(mean_hit_distance = mean(hit_distance, na.rm = TRUE)))
}

#Plots
plot_trends <- function(df) {
  # Hit Distance Over Time
  p1 <- ggplot(df, aes(x = date, y = hit_distance)) +
    geom_point(alpha = 0.1, color = "darkblue") +
    geom_smooth(method = "loess", se = FALSE, color = "red") +
    labs(title = "Hit Distance Over Time",
         x = "Date", y = "Hit Distance (feet)",
         caption = "Smoothed trend line in red") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  # Exit Velocity vs Distance by Year
  p2 <- ggplot(df, aes(x = hit_exit_speed, y = hit_distance, color = factor(year))) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = "Exit Velocity vs Hit Distance",
         x = "Exit Velocity (mph)", y = "Hit Distance (feet)",
         color = "Year") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.position = "bottom")
  
  # Launch Angle Distribution by Year
  p3 <- ggplot(df, aes(x = hit_vertical_angle, fill = factor(year))) +
    geom_density(alpha = 0.7) +
    labs(title = "Launch Angle Distribution by Year",
         x = "Launch Angle (degrees)", y = "Density",
         fill = "Year") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.position = "bottom")
  
  # Home Run Rate by Year
  hr_rate <- df %>%
    group_by(year) %>%
    summarize(hr_rate = mean(event_result == "home_run", na.rm = TRUE))
  
  p4 <- ggplot(hr_rate, aes(x = factor(year), y = hr_rate)) +
    geom_bar(stat = "identity", fill = "darkgreen") +
    labs(title = "Home Run Rate by Year",
         x = "Year", y = "Home Run Rate") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    scale_y_continuous(labels = percent)
  
  # Arrange plots
  grid.arrange(p1, p2, p3, p4, ncol = 2)
}

# New analyses
additional_analyses <- function(df) {
  # Seasonal analysis
  season_model <- lm(hit_distance ~ year + season, data = df)
  print("Seasonal Effect on Hit Distance:")
  print(summary(season_model))
  
  # Pitch type analysis
  pitch_type_model <- lm(hit_distance ~ year + pitch_type, data = df)
  print("Pitch Type Effect on Hit Distance:")
  print(summary(pitch_type_model))
  
  # Spin rate analysis
  spin_rate_model <- lm(hit_distance ~ year + hit_spinrate, data = df)
  print("Spin Rate Effect on Hit Distance:")
  print(summary(spin_rate_model))
  
  # Pitcher type analysis
  pitcher_type_model <- lm(hit_distance ~ year + pitcher_throws, data = df)
  print("Pitcher Type Effect on Hit Distance:")
  print(summary(pitcher_type_model))
  
  # Batter side analysis
  batter_side_model <- lm(hit_distance ~ year + bat_side, data = df)
  print("Batter Side Effect on Hit Distance:")
  print(summary(batter_side_model))
  
  # Batter bearing analysis
  batter_bearing_model <- lm(hit_distance ~ year + hit_bearing, data = df)
  print("Batter Bearing Effect on Hit Distance:")
  print(summary(batter_bearing_model))
  
  # Visualization of hit distance by pitch type over years
  p_pitch_type <- ggplot(df, aes(x = factor(year), y = hit_distance, fill = pitch_type)) +
    geom_boxplot() +
    labs(title = "Hit Distance by Pitch Type Over Years",
         x = "Year", y = "Hit Distance (feet)",
         fill = "Pitch Type") +
    theme_minimal()
  
  print(p_pitch_type)
}

# Clustering analysis
cluster_analysis <- function(df) {
  # Select relevant numeric variables for clustering
  cluster_data <- df %>%
    select(hit_distance, hit_exit_speed, hit_bearing, hit_vertical_angle) %>%
    na.omit()
  
  # Normalize the data
  cluster_data_normalized <- scale(cluster_data)
  
  # Perform k-means clustering
  set.seed(123)
  kmeans_result <- kmeans(cluster_data_normalized, centers = 3)
  
  # Add cluster assignments to the original data
  df$cluster <- kmeans_result$cluster[match(1:nrow(df), as.numeric(rownames(cluster_data)))]
  
  # Define cluster labels
  cluster_labels <- c("Pull Hitters", "Up-the-Middle Hitters", "Pop-ups/Weak Contact")
  
  # Assign labels to clusters based on their characteristics
  cluster_centers <- kmeans_result$centers
  cluster_assignment <- order(abs(cluster_centers[, "hit_bearing"]), decreasing = TRUE)
  cluster_names <- cluster_labels[cluster_assignment]
  
  # Create a named vector for easy mapping
  cluster_name_map <- setNames(cluster_names, 1:3)
  
  # Add cluster names to the dataframe
  df$cluster_name <- cluster_name_map[as.character(df$cluster)]
  
  # Visualize clusters
  p1 <- fviz_cluster(kmeans_result, data = cluster_data_normalized,
                     geom = "point",
                     ellipse.type = "convex",
                     ggtheme = theme_minimal()) +
    labs(title = "K-means Clustering of Batted Balls",
         subtitle = "Based on hit distance, exit speed, bearing, and vertical angle") +
    scale_color_discrete(name = "Cluster", labels = cluster_names) +
    scale_shape_discrete(name = "Cluster", labels = cluster_names)
  
  # Analyze clusters over time
  cluster_year_summary <- df %>%
    group_by(year, cluster_name) %>%
    summarise(mean_distance = mean(hit_distance, na.rm = TRUE),
              mean_exit_speed = mean(hit_exit_speed, na.rm = TRUE),
              mean_bearing = mean(hit_bearing, na.rm = TRUE),
              mean_vertical_angle = mean(hit_vertical_angle, na.rm = TRUE),
              .groups = "drop")
  
  p2 <- ggplot(cluster_year_summary, aes(x = factor(year), y = mean_distance, color = cluster_name)) +
    geom_line(aes(group = cluster_name)) +
    geom_point() +
    labs(title = "Mean Hit Distance by Cluster Over Years",
         x = "Year", y = "Mean Hit Distance (feet)",
         color = "Cluster Type") +
    theme_minimal()
  
  # Visualize cluster characteristics
  cluster_summary <- df %>%
    group_by(cluster_name) %>%
    summarise(across(c(hit_distance, hit_exit_speed, hit_bearing, hit_vertical_angle),
                     list(mean = mean, sd = sd), na.rm = TRUE, .names = "{.col}_{.fn}"))
  
  cluster_summary_long <- cluster_summary %>%
    pivot_longer(cols = -cluster_name, 
                 names_to = c("variable", "stat"), 
                 names_pattern = "(.*)_(.*)",
                 values_to = "value")
  
  p3 <- ggplot(cluster_summary_long %>% filter(stat == "mean"), 
               aes(x = cluster_name, y = value, fill = cluster_name)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(data = cluster_summary_long %>% filter(stat == "sd"),
                  aes(ymin = value, ymax = value), width = 0.2, position = position_dodge(0.9)) +
    facet_wrap(~ variable, scales = "free_y", nrow = 2) +
    labs(title = "Cluster Characteristics",
         x = "Cluster Type", y = "Value",
         fill = "Cluster Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Print all plots
  print(p1)
  print(p2)
  print(p3)
  
  # Return the clustered data for further analysis if needed
  return(df)
}

# Main analysis
perform_eda(df)
plot_trends(df)
additional_analyses(df)
df_clustered <- cluster_analysis(df)
