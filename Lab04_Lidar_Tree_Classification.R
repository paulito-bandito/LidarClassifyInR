# 
# AUTHOR:   Paul Walter
# SUBJECT:  633 Remote Sensing: following a tutorial on classifying point clouds in R
# @ see https://r-lidar.github.io/lidRbook/itd-its.html
#

# load the raster and rgdal libraries
library(lidR)
library(RCSF)
library(dplyr)
library(ggplot2)

# open canopy height model
LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
las <- readLAS(LASfile, select = "xyzr", filter = "-drop_z_below 0")
las <- classify_ground(las, algorithm = csf())
chm <- grid_canopy(las, 0.5, pitfree(subcircle = 0.2))
plot(las, bg = "white", size = 4)

# https://r-lidar.github.io/lidRbook/gnd.html
LASfile <- system.file("extdata", "Topography.laz", package="lidR")
las <- readLAS(LASfile, select = "xyzrn")
las <- classify_ground(las, algorithm = pmf(ws = 5, th = 3))
plot(las, color = "Classification", size = 3, bg = "white") 

# show a cross section
plot_crossection <- function(las,
                             p1 = c(min(las@data$X), mean(las@data$Y)),
                             p2 = c(max(las@data$X), mean(las@data$Y)),
                             width = 4, colour_by = NULL)
{
  colour_by <- enquo(colour_by)
  data_clip <- clip_transect(las, p1, p2, width)
  p <- ggplot(data_clip@data, aes(X,Z)) + geom_point(size = 0.5) + coord_equal() + theme_minimal()
  
  if (!is.null(colour_by))
    p <- p + aes(color = !!colour_by) + labs(color = "")
  
  return(p)
}

p1 <- c(273420, 5274455)
p2 <- c(273570, 5274460)
plot_crossection(las, p1 , p2, colour_by = factor(Classification))