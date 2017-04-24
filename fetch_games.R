library(opendotaR)
library(dplyr)

# Using opendotaR to do the heavy lifting
game_list <- get_game_list(num_matches = 100,
                           from_time = "20170401",
                           to_time = "20170425",
                           min_mmr = 3000,
                           num_open_profile = 6)

match_id_vec <- game_list$match_id

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