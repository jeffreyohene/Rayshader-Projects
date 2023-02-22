library(tidyverse)
library(magick)
library(glue)

# Load `header` list with needed data
header <- readRDS("header.rds")
colors <- header$colors
swatchplot(colors)

text_color <- colors[4]

img <- image_read(header$outfile)


#annot <- glue("This map shows population density of ",
#              "{str_to_title('Ghana')}. ",
#              "Population estimates are bucketed ",
#              "into 400 meter (about 1/4 mile) hexagons.") |> 
  str_wrap(45)
cat(annot)



img |> 
 image_crop(geometry = "x6000+0+150", gravity = "center") |>
  image_annotate(text = "GHANA",
                 gravity = "north",
                 location = "+2300+400",
                 font = "Cinzel Decorative",
                 color = text_color,
                 size = 275, 
                 weight = 900,
                 kerning = 100) |>
  image_annotate(text = "Population Density of",
                 gravity = "north",
                 location = "+2300+300",
                 font = "Cinzel Decorative",
                 color = text_color,
                 size = 80, 
                 weight = 500,
                 kerning = 50) |>
  image_annotate(text = glue("Viz by Jeffrey Ohene (@jeffrstats) | ",
                           "Data: kontur.io"),
               gravity = "south",
               location = "+0+200", 
               font = "Cinzel Decorative",
               color = alpha(text_color, .5),
               size = 120, weight = 700,
               kerning = 20) |> 
  image_write("images/GHA/r_gh.png")

system(
  glue("convert -size 10000x10000 xc:none ",
       # Shadows
       "-gravity north -font Cinzel-Decorative-Bold ",
       "-pointsize 500 -kerning 100 -stroke '{colors[4]}' -fill '{colors[4]}' ", 
       "-annotate +2420+685 'New Jersey' ",
       "-background none -blur 50x15 +repage ",
       "-pointsize 300 -kerning 25 -stroke '{colors[4]}' -fill '{colors[4]}' ", 
       "-annotate +2365+1490 'Population Density' ",
       "-background none -blur 30x10 +repage ",
       # Foreground font
       "-font Cinzel-Decorative-Bold ",
       "-stroke '{colors[4]}' -strokewidth 5 -fill '{colors[7]}' ",
       "-pointsize 500 -kerning 100 -annotate +2400+700 'Ghana' ", 
       "-stroke '{colors[4]}' -fill '{colors[7]}' ",
       
       "-pointsize 300 -kerning 25 -strokewidth 2  ",
       "-annotate +2350+1500 'Population Density' ", 
       "images/new_jersey/titled_nj_pop.png +swap -gravity north ",
       "-composite images/GHA/titled_gh_pop_done.png")
)

image_read("images/new_jersey/titled_nj_pop_done.png") |> 
  image_scale(geometry = "36%x") |> 
  image_write("tracked_graphics/titled_nj_pop_small.png")