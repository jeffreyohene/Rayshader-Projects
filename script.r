#Loading libraries...
library(pacman)
p_load(tidyverse, rgl, sf, rayshader, glue,
       colorspace, stars, NatParksPalettes, MetBrewer)

#Loading geopackage file from www.kontur.io
gh <- st_read('gh_pop.gpkg')

map <- 'GHA'

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

# Color palette with 'NatParksPalettes' package
pal <- "golden_brown"

c1 <- natparks.pals("Acadia", n = 10)
c2 <- natparks.pals("Redwood", n = 10)

colors <- c(lighten(c2[1], .75),
            lighten(c2[1], .5),
            lighten(c2[1], .25), 
            c2[1], c1[10:6])

swatchplot(colors)



texture <- grDevices::colorRampPalette(colors, bias = 3)(256)

swatchplot(texture)

close3d()

# 3D Object
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

# Ensure dir exists for these graphics
if (!dir.exists(glue("images/{map}"))) {
  dir.create(glue("images/{map}"))
}

# Set up outfile where graphic will be saved.
# Note that I am not tracking the `images` directory, and this
# is because these files are big enough to make tracking them on
# GitHub difficult. 
outfile <- str_to_lower(glue("images/{map}/{map}_{pal}.png"))

# Now that everything is assigned, save these objects so we
# can use then in our markup script
saveRDS(list(
  map = map,
  pal = pal,
  colors = colors,
  outfile = outfile,
  coords = coords
), glue("images/{map}/header.rds"))


{
  # Test write a PNG to ensure the file path is good.
  # You don't want `render_highquality()` to fail after it's 
  # taken hours to render.
  if (!file.exists(outfile)) {
    png::writePNG(matrix(1), outfile)
  }
  # I like to track when I start the render
  start_time <- Sys.time()
  cat(glue("Start Time: {start_time}"), "\n")
  render_highquality(
    # We test-wrote to this file above, so we know it's good
    outfile, 
    # See rayrender::render_scene for more info, but best
    # sample method ('sobol') works best with values over 256
    samples = 450, 
    preview = FALSE,
    light = TRUE,
    lightdirection = rev(c(310,310, 320,320)),
    lightcolor = c(colors[4], "white", colors[7], "white"),
    lightintensity = c(750, 50, 1000, 50),
    lightaltitude = c(10, 80, 10, 80),
    # lightdirection = 0,
    # lightcolor = "white",
    # lightintensity = 100,
    # lightaltitude = 90,
    # All it takes is accidentally interacting with a render that takes
    # hours in total to decide you NEVER want it interactive
    interactive = FALSE,
    # scene_elements = map2_df(c(1, -1, 0, 0), 
    #                          c(0, 0, 1, -1), 
    #                          function(i, j) {
    #   rayrender::add_object(
    #     rayrender::sphere(z=ncol(mat) * .15 * j,
    #                       y= 1000,
    #                       x= nrow(mat) * .15 * i,
    #                       radius= nrow(mat) * .005,
    #                       material=rayrender::light(color=colors[7],
    #                                                 intensity=2000))
    #   )
    # }),
    # HDR lighting used to light the scene
    # environment_light = "assets/env/phalzer_forest_01_4k.hdr",
    # # environment_light = "assets/env/small_rural_road_4k.hdr",
    # # Adjust this value to brighten or darken lighting
    # intensity_env = 1.5,
    # # Rotate the light -- positive values move it counter-clockwise
    # rotate_env = 130,
    # This effectively sets the resolution of the final graphic,
    # because you increase the number of pixels here.
    # width = round(6000 * wr), height = round(6000 * hr),
    width = 8000, height = 8000
  )
  end_time <- Sys.time()
  cat(glue("Total time: {end_time - start_time}"), "\n")
}