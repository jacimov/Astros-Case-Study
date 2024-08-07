library(tidyr)
library(dplyr)

# Set up data
players <- c("Force-Field Fred", "Long-Ball Larry")
obp <- c(1.000, 0.250)
slg <- c(0.000, 1.000)
ops <- c(1.000, 1.250)
woba <- c(0.690, 0.525)

# Create dataframe
df <- data.frame(Player = players, OBP = obp, SLG = slg, OPS = ops, wOBA = woba)

# Reshape the data
df_long <- df %>%
  pivot_longer(cols = c(OBP, SLG, OPS, wOBA), 
               names_to = "Metric", 
               values_to = "Value")

# Create bar plot
ggplot(df_long, aes(x = Metric, y = Value, fill = Player)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Comparison of Force-Field Fred vs Long-Ball Larry",
       x = "Metric",
       y = "Value") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Next Plot
set.seed(123)
simulations <- 10000
pa_per_game <- 4

runs_fred <- numeric(simulations)
runs_larry <- numeric(simulations)

for (i in 1:simulations) {
  fred_runs <- sum(rbinom(pa_per_game, 1, 1.0))  # Fred always gets on base
  larry_runs <- sum(rbinom(pa_per_game, 1, 0.25)) * 4  # Larry's HRs, multiplied by 4 for run value
  runs_fred[i] <- fred_runs
  runs_larry[i] <- larry_runs
}

df_runs <- data.frame(
  Player = rep(c("Force-Field Fred", "Long-Ball Larry"), each = simulations),
  Runs = c(runs_fred, runs_larry)
)

ggplot(df_runs, aes(x = Runs, fill = Player)) +
  geom_density(alpha = 0.7) +
  labs(title = "Distribution of Runs Scored per Game",
       subtitle = "Based on 4 plate appearances per game",
       x = "Runs",
       y = "Density") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

