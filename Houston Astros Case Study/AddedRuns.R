library(tidyverse)
library(dplyr)
library(ggplot2)

set.seed(123)
simulations <- 10000
pa_per_season <- 600  # Assuming about 150 games with 4 PA per game

simulate_season <- function(player_type, team_obp = 0.320, team_slg = 0.414) {
  runs <- 0
  for (pa in 1:pa_per_season) {
    if (player_type == "Fred") {
      # Fred always walks
      runners <- sample(0:3, 1, prob = c(0.551, 0.2354, 0.1647, 0.0489))  # Estimate baserunners
      runs <- runs + rbinom(1, runners, team_obp)  # Chance to drive in runners
    } else if (player_type == "Larry") {
      if (rbinom(1, 1, 0.25) == 1) {  # 25% chance of homer
        runners <- sample(0:3, 1, prob = c(0.5, 0.25, 0.15, 0.1))  # Estimate baserunners
        runs <- runs + 1 + runners  # HR always scores self + runners
      }
    }
  }
  return(runs)
}

run_simulation <- function(team_obp, team_slg) {
  fred_runs <- replicate(simulations, simulate_season("Fred", team_obp, team_slg))
  larry_runs <- replicate(simulations, simulate_season("Larry", team_obp, team_slg))
  
  df_runs <- data.frame(
    Player = rep(c("Force-Field Fred", "Long-Ball Larry"), each = simulations),
    Runs = c(fred_runs, larry_runs)
  )
  
  return(df_runs)
}

# Run simulation with league average OBP and SLG
df_runs_avg <- run_simulation(0.320, 0.400)

# Plot results
ggplot(df_runs_avg, aes(x = Runs, fill = Player)) +
  geom_density(alpha = 0.7) +
  labs(title = "Distribution of Runs Scored per Season",
       subtitle = "Based on 600 plate appearances per season",
       x = "Runs",
       y = "Density") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# Calculate and print average runs
avg_runs <- df_runs_avg %>%
  group_by(Player) %>%
  summarize(Avg_Runs = mean(Runs))
print(avg_runs)

# Run simulations for different team performances
df_runs_low <- run_simulation(0.290, 0.350)  # Low-performing team
df_runs_high <- run_simulation(0.350, 0.450)  # High-performing team

# Calculate and print average runs for different scenarios
avg_runs_low <- df_runs_low %>%
  group_by(Player) %>%
  summarize(Avg_Runs = mean(Runs))
avg_runs_high <- df_runs_high %>%
  group_by(Player) %>%
  summarize(Avg_Runs = mean(Runs))

print("Low-performing team:")
print(avg_runs_low)
print("High-performing team:")
print(avg_runs_high)