# Loading libraries
library(caret)
library(nnet)
library(dplyr)
library(rgl)
library(car)
source("interaction_terms.R")

# Loading data
datamart <- readRDS("data/tsne_output.RDS")

manual_data <- read.csv("data/manual_lanes.csv")

datamart_manual <- datamart[datamart$match_id %in% manual_data$matchID, ]

# Add manual label on
datamart_clean <- left_join(datamart_manual,
                            select(manual_data, matchID, heroID, Lane),
                            by = c("match_id" = "matchID",
                                   "hero_id" = "heroID"))

# Creating a column for lane number so we can use colors
lane_list <- c(1, 2, 3, 4, 5)
lane_roles <- c("Off", 
                "Mid", 
                "Safe",
                "Roamer",
                "Jungle")

lane_df <- data.frame(lane_num = lane_list,
                      lane_role = lane_roles,
                      stringsAsFactors = FALSE)

datamart_final <- left_join(datamart_clean, lane_df, by = c("Lane" = "lane_role"))

# Plot the data in 3D with colors
width <- 800
open3d()
par3d(windowRect = 50 + c( 0, 0, width, width ) )
plot3d(select(datamart_final, X,Y,Z), 
       type="p", 
       radius=0.1, 
       axes=F, 
       col = as.factor(datamart_final$lane_num),
       expand = 0)
legend3d("topright", legend = lane_roles, pch = 16, col = as.factor(lane_list), cex=1, inset=c(0.02))
# Create an output GIF, comment out to not create a gif output
# movie3d( spin3d(rpm = 6), duration=10, fps = 10, dir="gif/", clean=FALSE, convert = FALSE)

# Since i want clean perhaps parabolic boundries i'm fitting a linear model with second
# degree interaction terms

# Start by creating interaction terms so we can get parabolic boundries as well
with_interaction <- interaction_terms(select(datamart_final, X, Y, Z), 2, 1)

with_interaction$odota_lane <- as.factor(datamart_final$odota_lane_role)
with_interaction$lane <- as.factor(datamart_final$Lane)

# Make training and test datasets
training_index <- createDataPartition(with_interaction$lane, p = 0.5, list = FALSE)

training <- with_interaction[training_index, ]
testing <- with_interaction[-training_index, ]

# Perform simple multinomial linear regression
learner <- multinom(lane ~ ., data = select(training, -odota_lane))
predictions <- predict(learner, newdata = training)

confusionMatrix(predictions, training$lane)

# Predict roles for the testing data.
testing$predicted_lane <- predict(learner, newdata = select(testing, -odota_lane))

learn_accuracy <- confusionMatrix(testing$predicted_lane, testing$lane)
odota_acccuracy <- confusionMatrix(testing$odota_lane, testing$lane)

learn_accuracy$overall[1]
learn_accuracy$table

odota_acccuracy$overall[1]
odota_acccuracy$table

# Now we impute the lane onto all data and plot to see if it looks pretty
datamart_interact <- interaction_terms(select(datamart, X, Y, Z), 2, 1)

datamart$predicted_lane <- predict(learner, newdata = datamart_interact)

# Add number col so we can color the plot
lane_df <- data.frame(predicted_num = lane_list,
                      lane_role = lane_roles)
datamart_predicted <- left_join(datamart, lane_df, by = c("predicted_lane" = "lane_role"))

# Visualize
width <- 800
open3d()
par3d(windowRect = 50 + c( 0, 0, width, width ) )
plot3d(select(datamart_predicted, X,Y,Z), 
       type="p", 
       radius=0.1, 
       axes=F, 
       col = as.factor(datamart_predicted$predicted_num),
       expand = 0)
legend3d("topright", legend = lane_roles, pch = 16, col = as.factor(lane_list), cex=1, inset=c(0.02))
#movie3d( spin3d(rpm = 6), duration=10, fps = 10, dir="gif/", clean=FALSE, convert = FALSE)
