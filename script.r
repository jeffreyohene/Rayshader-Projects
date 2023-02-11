#Loading libraries...
library(pacman)
p_load(tidyverse, rgl, sf, rayshader, glue,
       colorspace, stars, NatParksPalettes)

#Loading geopackage file from www.kontur.io
gh <- st_read('gh_pop.gpkg')


#Plotting geopackage object, saved under p1
p1 <- gh |>
  ggplot()+
  geom_sf()

p1


#Bounding box for map object
bb <- st_bbox(gh)


#Setting up aspect ratio
yind <- st_distance(st_point(c(bb[['xmin']], bb[['ymin']])), 
                    st_point(c(bb[['xmin']], bb[['ymax']])))

xind <- st_distance(st_point(c(bb[['xmin']], bb[['ymin']])), 
                    st_point(c(bb[['xmax']], bb[['ymin']])))

if (yind > xind) {
  y_rat <- 1
  x_rat <- xind / yind
} else {
  x_rat <- 1
  y_ratio <- yind / xind
}

#Converting map object to raster
size <- 8000
rast <- st_rasterize(gh%||% 
                       select(population, geom), 
                     nx = floor(size * x_rat), ny = floor(size * y_rat))

#Conversion to matrix
mat <- matrix(rast$population, nrow = floor(size * x_rat), 
              ncol = floor(size * y_rat))