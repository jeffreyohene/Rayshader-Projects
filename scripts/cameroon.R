# Load necessary libraries
library(pacman)
p_load(
sf, tidyverse, tigris, stars, MetBrewer, colorspace, rayshader, scales,
NatParksPalettes, extrafont, magick, glue)

# List files in the current working directory
dir()
# Read geospatial data from the Gambia gpkg file
cam <- st_read('kontur_population_CM_20220630.gpkg')
# Plot data
cam |>
  ggplot() +
  geom_sf()

# Calculate the bounding box of the spatial data
bb <- st_bbox(cam)
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
size <- 10000

# Rasterize the spatial data
rast <- st_rasterize(cam |>
  select(population, geom),
  nx = floor(size * x_rat), ny = floor(size * y_rat))

# Convert the rasterized data to a matrix
mat <- matrix(rast$population, nrow = floor(size * x_rat), ncol = floor(size * y_rat))


# Set up a color palette
pal <- 'hiroshige'
c1 <- met.brewer('Hiroshige')
colors <- c1[c(5:1, 10:6)] |> rev()
swatchplot(colors)
texture <- grDevices::colorRampPalette(c1[5:1], bias = 2)(256)
swatchplot(texture)


# Call below helps if you want to keep readjusting your 3d object
try(rgl::rgl.close())


# Create the initial 3D object
mat |>
  height_shade(texture = texture) |>
  plot_3d(heightmap = mat,
  solid = FALSE,
  z = 35,
  shadowdepth = 0,
  windowsize = c(800,800),
  phi = 90,
  zoom = 1,
  theta = 0,
  background = 'white')


# Use this to adjust the view after building the window object
render_camera(phi = 35, zoom = .75, theta = -5)
# Define the output file for the 3D map
outfile <- 'cameroon_pop_density.png'

{
  start_time <- Sys.time()
  cat(crayon::cyan(start_time), '\n')
  if(!file.exists(outfile)) {
    png::writePNG(matrix(1), target = outfile)
  }
  
  render_highquality(
    outfile,
    samples = 450, 
    preview = FALSE,
    light = TRUE,
    lightdirection = rev(c(200, 200, 205, 205)),
    lightcolor = c("#A8C697", "white", colors[2], "white"),
    lightintensity = c(750, 50, 1000, 50),
    lightaltitude = c(10, 80, 10, 80),
    interactive = FALSE,
    width = 6000, height = 6000,
    ground_material = rayrender::microfacet(roughness = .4, 
                                            eta = c(1, .75, .1), 
                                            kappa = c(.1, .75, 1))
  )
  
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), '\n')
}


# List files in the current working directory
dir()

# Read the rendered image
img <- image_read('cameroon_pop_density.png')

# Define text colors
text_color <- colors[1]
text_color2 <- colors[2]
text_color3 <- colors[3]


# Add title and captions to the image
erste <- image_annotate(img, 'Carte de densité de population du', font = 'Roboto',
                        color = text_color, size = 100, weight = 600, gravity = 'north',
                        location = '-1800+850')

zweite <- image_annotate(erste, 'Cameroun', font = 'Marhey',
                         color = text_color, size = 500, weight = 600, gravity = 'north',
                         location = '-1105+1000')

dritte <- image_annotate(zweite, 'Viz by @jeffrstats | Data: kontur.io',
                         font = 'Roboto', location = '+1850+40',
                         color = text_color2, size = 80, gravity = 'south')



# Write the annotated image to a file
image_write(dritte, glue('cameroun_population_density_annotated.png'))
