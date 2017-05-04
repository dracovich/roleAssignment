# Load libraries
library(ggplot2)
library(rgl)
library(car)
library(dplyr)
library(magick)

# Load the processed tSNE output for X/Y/Z coordinates
tsne <- readRDS("data/tsne_output.RDS")

# Define the roles and color numbers
lane_list <- c(1, 2, 3, 4, 5)
lane_roles <- c("Off", 
                "Mid", 
                "Safe",
                "Roamer",
                "Jungle")

# Plot in 3D
width <- 800
open3d()
par3d(windowRect = 50 + c( 0, 0, width, width ) )
plot3d(select(tsne, X, Y, Z), 
       type="p", 
       radius=0.1, 
       axes=F, 
       col = as.factor(tsne$odota_lane_num),
       expand = 0)
legend3d("topright", legend = lane_roles, pch = 16, col = as.factor(lane_list), cex=1, inset=c(0.02))

# Create an output GIF
movie3d( spin3d(rpm = 6), duration=10, fps = 10, dir="gif/", clean=FALSE, convert = FALSE)

