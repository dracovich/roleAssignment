# Load libraries
library(Rtsne)
library(caret)
library(dplyr)

# Fetch data
datamart <- readRDS("data/matches.RDS")
datamart <- subset(datamart, datamart$side == "Dire")

datamart$new_lane <- ifelse(datamart$is_roaming == TRUE, 4, datamart$lane)

# Remove non-relevant variables
used_data <- as.data.frame(select(datamart,
                                 -is_roaming,
                                 -xp,
                                 -gold,
                                 -lastHits,
                                 -deny))

# Some coordinates have too few datapoints to be able to be used.
nzv <- nearZeroVar(used_data, saveMetrics = TRUE)
zero_var <- nzv$zeroVar
datamart <- used_data[, !zero_var]

# Remove variables we don't wish to use
tsne_data <- dplyr::select(datamart, 
                           -match_id,
                           -hero_id,
                           -player_slot,
                           -lane, 
                           -new_lane, 
                           -mean_x, 
                           -mean_y, 
                           -avg_dist_med)

# tSNE doesn't handle duplicates well, so we must remove any duplicated rows both in the tsne data
# and in the datamrt (so we can label it correctly after).
duplicate_rows <- duplicated(tsne_data)
tsne_data <- tsne_data[!duplicate_rows, ]
datamart <- datamart[!duplicate_rows, ]

# Assign zero to all NA (Rtsne doesn't handle NA very gracefuly, just omits the rows without warning)
datamart[is.na(datamart)] <- 0
tsne_data[is.na(tsne_data)] <- 0

# run tSNE on the data.
tsne <- Rtsne(tsne_data, 
              dims = 3, 
              perplexity = 60, 
              verbose = TRUE, 
              max_iter = 600)

# Extract the tSNE coordinates
tsne_out <- as.data.frame(tsne$Y)
colnames(tsne_out) <- c("X","Y","Z")

# We need a final output including match_id, hero_id, and player_slot so we can extract
# the manually specified lanes
base_data <- select(datamart,
                    match_id,
                    hero_id,
                    player_slot)

# create combined output
combined_out <- cbind(base_data, tsne_out)

# Add lane to tSNE data
combined_out$odota_lane_num <- datamart$new_lane

# Add a name to each role
lane_list <- c(1, 2, 3, 4, 5)
lane_roles <- c("Off", 
               "Mid", 
               "Safe",
               "Roamer",
               "Jungle")

lane_df <- data.frame(lane_num = lane_list,
                      odota_lane_role = lane_roles,
                      stringsAsFactors = FALSE)

with_lane <- left_join(combined_out, 
                       lane_df, 
                       by = c("odota_lane_num" = "lane_num"))

saveRDS(with_lane, "data/tsne_output.RDS")
