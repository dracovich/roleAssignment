library(opendotaR)
library(dplyr)

# Using opendotaR to do the heavy lifting
game_list <- get_game_list(num_matches = 30000,
                           from_time = "20170101",
                           to_time = "20170501",
                           min_mmr = 2000,
                           num_open_profile = 7)

match_id_vec <- game_list$match_id

# We also load all the match_id's from our manually labeled data so that we have all the manual ones
manual_data <- read.csv("data/manual_lanes.csv", sep = ",")

match_id_vec <- unique(c(match_id_vec, manual_data$matchID))

# The JSON pruning (where we find the data we want to use) is found in an external file.
parsed_games <- get_games(match_id_vec,
                          output = "json_pruning.R")

# Output is a list, must use rbindlist to get data frame.
parsed_games_final <- rbindlist(parsed_games, fill = TRUE)

# Add extra info on sides.
parsed_games_final$side <- ifelse(parsed_games_final$player_slot < 10, "Radiant", "Dire")
parsed_games_final[is.na(parsed_games_final)] <- 0

# Save the file
saveRDS(parsed_games_final, "data/matches.RDS")