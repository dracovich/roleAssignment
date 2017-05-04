library(reshape2)
library(jsonlite)
library(tidyr)
library(dplyr)
library(data.table)

lane_pos_list <- flatten(read_json$players$lane_pos)
pos_list <- list()

# Iterate through each player, getting the values we need.
for(j in 1:10) {
  # Number of bounty runes before 10 minute mark
  rune_list <- as.data.frame(read_json$players$runes_log[j])
  if (nrow(rune_list) > 0) {
    bounty_runes <- nrow(subset(rune_list, rune_list$key == 5 & rune_list$time <600))
  } else {
    bounty_runes <- 0
  }
  
  # Total number of wards bought the first 20 minutes
  purchase_log <- as.data.frame(read_json$players$purchase_log[j])
  
  if (nrow(purchase_log) > 0) {
    wards_only <- subset(purchase_log, 
                        (purchase_log$key == "ward_sentry" | purchase_log$key == "ward_observer") &
                          purchase_log$time < 1200)
    
    if (nrow(wards_only) > 0) {
      wards_bought <- nrow(wards_only)
    } else {
      wards_bought <- 0
    }
  } else {
    wards_bought <- 0
  }
  
  # Create a list of lane position first 10 minutes of game
  hero_lane_pos <- flatten(lane_pos_list[j,])
  lane_pos_long <- as.data.frame(gather(hero_lane_pos))
  if(length(lane_pos_long) > 0) {
    lane_pos_long <- subset(lane_pos_long, !is.na(lane_pos_long$value)) 
  } else {
    next
  }
  
  # Sometimes the list is all NA for some reason, which breaks the program.
  if (nrow(lane_pos_long) == 0) {
    next
  }
  
  # We get output in weird format, so i have to do some string work to get the x and y coordintes
  lane_pos_long$x <- as.numeric(substr(lane_pos_long$key, 
                                       1, 
                                       regexpr('\\.', lane_pos_long$key)-1))
  lane_pos_long$y <- as.numeric(substr(lane_pos_long$key, 
                                       regexpr('\\.', lane_pos_long$key)+1, 
                                       nchar(lane_pos_long$key)))
  
  lane_pos_final <- lane_pos_long[, c(3,4,2)]
  lane_pos_final$match_id <- match_id
  lane_pos_final$hero_id <- read_json$players$hero_id[j]
  lane_pos_final$player_slot <- read_json$players$player_slot[j]
  lane_pos_final$lane <- read_json$players$lane[j]
  # Calculate weighted mid point of the lane position
  mean_pos <- data.frame(mean_x = weighted.mean(lane_pos_final$x, lane_pos_final$value), 
                         mean_y = weighted.mean(lane_pos_final$y, lane_pos_final$value))
  
  lane_pos_final$med_dist <- sqrt((lane_pos_final$x - mean_pos$mean_x)^2 + (lane_pos_final$y - mean_pos$mean_y)^2)
  
  lane_pos_final$mean_x <- mean_pos$mean_x
  lane_pos_final$mean_y <- mean_pos$mean_y
  
  # Calculate distance of each point from the median
  lane_pos_final$avg_dist_med <- weighted.mean(lane_pos_final$med_dist, lane_pos_final$value)
  
  lane_pos_final$gold <- unlist(read_json$players$gold_t[j])[11] 
  lane_pos_final$xp <- unlist(read_json$players$xp_t[j])[11]
  lane_pos_final <- select(lane_pos_final, -med_dist)
  
  lane_pos_final$lastHits <- unlist(read_json$players$lh_t[j])[11]
  lane_pos_final$deny <- unlist(read_json$players$dn_t[j])[11]
  
  lane_pos_final$is_roaming <- unlist(read_json$players$is_roaming[j])
  
  # long to wide format
  final_wide <- dcast(lane_pos_final,
                     match_id + hero_id + player_slot + lane + is_roaming + mean_x + mean_y + 
                     avg_dist_med + gold + xp + lastHits + deny ~ x + y,
                     value.var = "value")
  
  pos_list[[j]] <- final_wide
}

output_list[[parsed_count]] <- rbindlist(pos_list, fill = TRUE)
