# Load necessary libraries
library(pacman)

p_load(
  sf, tidyverse, tigris, stars, MetBrewer, colorspace, rayshader, scales,
  NatParksPalettes, extrafont, magick, glue
)

# List files in the current working directory
dir()

# Read geospatial data from the Gambia gpkg file
gam <- st_read('kontur_population_GM_20220630.gpkg')

# Plot data
gam |> 
  ggplot() +
  geom_sf()

# Calculate the bounding box of the spatial data
bb <- st_bbox(gam)
yind <- st_distance(st_point(c(bb[["xmin"]], bb[["ymin"]])), 
                    st_point(c(bb[["xmin"]], bb[["ymax"]])))
xind <- st_distance(st_point(c(bb[["xmin"]], bb[["ymin"]])), 
                    st_point(c(bb[["xmax"]], bb[["ymin"]])))

# Determine the aspect ratio for resizing
if (yind > xind) {
  y_rat <- 1
  x_rat <- xind / yind
} else {
  x_rat <- 1
  y_rat <- yind / xind
}

# Define the size for rasterization
size <- 7000

# Rasterize the spatial data
rast <- st_rasterize(gam |> 
                       select(population, geom),
                     nx = floor(size * x_rat), ny = floor(size * y_rat))

# Convert the rasterized data to a matrix
mat <- matrix(rast$population, nrow = floor(size * x_rat), ncol = floor(size * y_rat))

# Set up a color palette
pal <- "golden_brown"
c1 <- natparks.pals("Acadia", n = 10)
c2 <- natparks.pals("Redwood", n = 10)

# Define colors and textures
colors <- c(lighten(c2[1], .75),
            lighten(c2[1], .5),
            lighten(c2[1], .25), 
            c2[1], c1[10:6])

# Display a swatch plot of colors
swatchplot(colors)

# Generate a texture based on the color ramp
texture <- grDevices::colorRampPalette(colors[9:5])(256)

# Display a swatch plot of the texture
swatchplot(texture)

# Call below helps if you want to keep readjusting your 3d object
# try(rgl::rgl.close())

# Create the initial 3D object
mat |> 
  height_shade(texture = texture) |> 
  plot_3d(heightmap = mat,
          solid = FALSE,
          z = 75/10,
          shadowdepth = 0,
          windowsize = c(800,800),
          phi = 90, 
          zoom = 1,
          theta = 0, 
          background = "white")

# Use this to adjust the view after building the window object
render_camera(phi = 45, zoom = 1, theta = -3)

# Define the output file for the 3D map
outfile <- "gambia_population_density.png"

# Render the 3D map and save it to the output file
# We will use a custome function to track how many hours it took

{
  start_time <- Sys.time()
  cat(crayon::cyan(start_time), "\n")
  if(!file.exists(outfile)) {
    png::writePNG(matrix(1), target = outfile)
  }
  
  render_highquality(
    outfile,
    samples = 450, 
    preview = FALSE,
    light = TRUE,
    lightdirection = rev(c(200, 200, 205, 205)),
    lightcolor = c(colors[3], 'white', colors[7], 'white'),
    lightintensity = c(500, 75, 750, 75),
    lightaltitude = c(10, 80, 10, 80),
    interactive = FALSE,
    width = 10000, height = 10000,
    ground_material = rayrender::microfacet(roughness = .4, 
                                            eta = c(1, .75, .1), 
                                            kappa = c(.1, .75, 1))
  )
  
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), "\n")
}

# List files in the current working directory
dir()

# Read the rendered image
img <- image_read("gambia_population_density.png")

# Define text colors
text_color <- colors[1]
text_color2 <- colors[2]
text_color3 <- colors[3]

# Calculate area, population, and density
area = round(as.numeric(sum(st_area(gam)) / 10**6), 0)
pop = sum(gam$population)
density = round(pop / area, 0)

# Add title and captions to the image
erste <- image_annotate(img, "Population Density Map of", font = "Roboto",
                        color = text_color, size = 200, weight = 600, gravity = "north",
                        location = "-3400+750")

zweite <- image_annotate(erste, "The Gambia", font = "Marhey",
                         color = text_color, size = 800, weight = 600, gravity = "north",
                         location = "-2005+1000")

dritte <- image_annotate(zweite, "Viz by @jeffrstats | Data: kontur.io",
                         font = "Roboto", location = "+3850+40",
                         color = text_color2, size = 140, gravity = "south")

vierte <- image_annotate(dritte, "Banjul",
                         font = "Roboto", location = "-3450+4700",
                         color = text_color3, size = 140, gravity = "north")

fünfte <- image_annotate(vierte, "Serekunda",
                         font = "Roboto", location = "-4475+4500",
                         color = text_color3, size = 140, gravity = "north")

sechste <- image_annotate(fünfte, "Farafenni",
                          font = "Roboto", location = "-505+4650",
                          color = text_color3, size = 140, gravity = "north")

# Write the annotated image to a file
image_write(sechste, glue("gambia_population_density_annotated.png"))
