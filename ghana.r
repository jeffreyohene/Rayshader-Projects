# Loading required packages
library(pacman)
p_load(tidyverse, rgl, sf, rayshader, glue,
       colorspace, stars, NatParksPalettes, MetBrewer)

# Loading geopackage file from www.kontur.io
gh <- st_read('gh_pop.gpkg')

map <- 'GHA'

# Plotting geopackage object, saved under p1
p1 <- gh |>
  ggplot()+
  geom_sf()

p1


# Bounding box for map object
bb <- st_bbox(gh)


# Setting up aspect ratio
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

# Converting map object to raster
size <- 8000
rast <- st_rasterize(gh%||% 
                       select(population, geom), 
                     nx = floor(size * x_rat), ny = floor(size * y_rat))

# Conversion to matrix
mat <- matrix(rast$population, nrow = floor(size * x_rat), 
              ncol = floor(size * y_rat))


# Setting up color palette
pal <- "golden_brown"

c1 <- natparks.pals("Acadia", n = 10)
c2 <- natparks.pals("Redwood", n = 10)

# Adjusting colors
colors <- c(lighten(c2[1], .75),
            lighten(c2[1], .5),
            lighten(c2[1], .25), 
            c2[1], c1[10:6])

swatchplot(colors)

texture <- grDevices::colorRampPalette(colors, bias = 3)(256)

swatchplot(texture)


# Close RGL window
try(close3d())

# Plotting 3D Object
mat |> 
  height_shade(texture = texture) |> 
  plot_3d(heightmap = mat,
          zscale = 15,
          solid = FALSE,
          shadowdepth = 0,
          windowsize = c(800,800),
          phi = 45,
          zoom = .8,
          theta = 270)

# Checking directory
if (!dir.exists(glue("images/{map}"))) {
  dir.create(glue("images/{map}"))
}

# Setting output file for picture 
outfile <- str_to_lower(glue("images/{map}/{map}_{pal}.png"))

# Saving rds file for markup
saveRDS(list(
  map = map,
  pal = pal,
  colors = colors,
  outfile = outfile,
  coords = coords
), glue("images/{map}/header.rds"))


# Testing png file path
{
  if (!file.exists(outfile)) {
    png::writePNG(matrix(1), outfile)
  }
  
# Function to track time & render high quality image
  start_time <- Sys.time()
  cat(glue("Start Time: {start_time}"), "\n")
  render_highquality(outfile, 
    samples = 450, 
    preview = FALSE,
    light = TRUE,
    lightdirection = rev(c(310,310, 320,320)),
    lightcolor = c(colors[4], "white", colors[7], "white"),
    lightintensity = c(750, 50, 1000, 50),
    lightaltitude = c(10, 80, 10, 80),
    interactive = FALSE,
    width = 8000, height = 8000
  )
  end_time <- Sys.time()
  cat(glue("Total time: {end_time - start_time}"), "\n")
}