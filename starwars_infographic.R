# starwars_infographic

# Code written by Claude Opus 5.5.
# Code modified and reviewed by Ida.


# Questions:
## Who has the most and least athletic build?
### Calculate BMI of all characters and visualize with dotplot
### (drawn as lightsabers on a starry space background)

## Does build say anything about skills?
### Calculate n movies and n vehicles/star ships for "skills": films as a strip
### (episode I-VII), and the names of the starships and vehicles piloted


# Dataset: dplyr:: starwars

# Usage:
# Rscript starwars_infographic.R [output_file] [group] [palette]


# Arguments (optional, default in brackets):
## output_file (required)   .png, .pdf
## group       (optional)   all, human, nonhuman [all]
## palette     (optional)   classic, jedi, sith, colorblind [classic]


# Font (optional):
## The title uses the free font "Orbitron" (Google Fonts) if it is installed on
## the computer. Otherwise the normal sans-serif font is used.


# Examples:
## Rscript starwars_infographic.R starwars.png
## Rscript starwars_infographic.R humans.png human
## Rscript starwars_infographic.R sith.pdf all sith



library(tidyverse)


### 1. Argument settings

args <- commandArgs(trailingOnly = TRUE)


usage <- "Usage: Rscript starwars_infographic.R <output.png|.pdf> [all|human|nonhuman] [classic|jedi|sith|colorblind]"


if (length(args) < 1) {
  stop(usage) }


out_file <- args[1]

group_choice <- if (length(args) >= 2) args[2] else "all"

palette_choice <- if (length(args) >= 3) args[3] else "classic"


if(!group_choice %in% c("all", "human", "nonhuman")) {
  stop("group must be all, human or nonhuman.\n", usage)
}




### 2. Color palettes and space theme

# heavy = heaviest build, light = lightest build (the lightsaber colours),
# skill = films and piloted. Bright colours, since the background is dark.

palettes <- list(
  classic    = c(heavy = "#FF3B3B", light = "#3BA7FF", skill = "#FFE81F"),
  jedi       = c(heavy = "#3DFF6E", light = "#3BA7FF", skill = "#D9B77E"),
  sith       = c(heavy = "#FF1E1E", light = "#B8B8B8", skill = "#FF8A00"),
  colorblind = c(heavy = "#E69F00", light = "#56B4E9", skill = "#F0E442")   # Okabe-Ito
)


if(!palette_choice %in% names(palettes)) {
  stop("palette must be classic, jedi, sith or colorblind. \n", usage)
}


pal <- palettes[[palette_choice]]

group_cols <- c("Heaviest build" = unname(pal["heavy"]),
                "Lightest build" = unname(pal["light"]))

skill_col <- unname(pal["skill"])


# Colours for the space theme (the same for all palettes)
space_bg   <- "#05060F"   # background: deep space
text_main  <- "#E8E8E8"   # normal text
text_dim   <- "#8E94A8"   # less important text
title_col  <- "#FFE81F"   # yellow title
hilt_col   <- "#8C8C8C"   # lightsaber handle



### 3. Fonts

# Use Orbitron for the title if it is installed, otherwise the default font.
# systemfonts is installed together with the tidyverse (via ragg).
installed_fonts <- systemfonts::system_fonts()$family

title_font <- if ("Orbitron" %in% installed_fonts) "Orbitron" else "sans"
body_font  <- "sans"

message("Title font: ", title_font)

# Text size: all text in the infographic is multiplied by this number.
# 1 = original size, 1.25 = 25 % larger, and so on.
fs <- 1.25



### 4. Prepare the data

characters <- dplyr::starwars |>
  filter(!is.na(height),
         !is.na(mass)) |>
  mutate(bmi = mass/(height/100)^2,
         Starships =
           lengths(starships),
         Vehicles =
           lengths(vehicles)
  )


if (group_choice == "human")
  characters <- filter(characters, species == "Human")

if (group_choice == "nonhuman")
  characters <- filter(characters, species != "Human")



### 5. Find the extreme outliers and pick the top 5 and bottom 5


q <- quantile(characters$bmi,
              c(0.25, 0.75))

cutoff <- unname(q[2] + 3 * (q[2] - q[1]))

outliers   <- characters |> filter(bmi > cutoff) |> arrange(desc(bmi))
characters <- characters |> filter(bmi <= cutoff)
n_total    <- nrow(characters) + nrow(outliers)


# Report in the terminal what was found

message("BMI cutoff: ", round(cutoff, 1), ". Characters above it: ",
        if (nrow(outliers) == 0) "none" else paste(outliers$name, collapse = ", "))

if (nrow(characters) < 10) {
  stop("Fewer than 10 characters left below the cutoff.")
}

extremes <- bind_rows(
  slice_max(characters, bmi, n = 5, with_ties = FALSE) |> mutate(group = "Heaviest build"),
  slice_min(characters, bmi, n = 5, with_ties = FALSE) |> mutate(group = "Lightest build")
) |>
  mutate(name = fct_reorder(name, bmi))   # rows 1-5 = lightest, 6-10 = heaviest



### 6. Lay out the lanes (x-positions to the right of the dot plot)

#   x = 0 ...... dot_lim | film squares (I-VII) | piloted crafts | outlier box
#      lightsabers       |  ■ ■ □ ■ ■ ■ □      |  X-wing, ...   |

dot_max <- max(extremes$bmi)

dot_lim <- dot_max * 1.18                       # where the dot plot ends

hilt_end <- dot_lim * 0.035                     # end of the lightsaber handle

film_w <- dot_lim * 0.045                       # width of one film square

film_x <- dot_lim + dot_lim * 0.04 + film_w * (1:7 - 0.5)   # centre of each square

pilot_x0   <- max(film_x) + film_w / 2 + dot_lim * 0.07     # start of the piloted text

pilot_w    <- dot_lim * 0.65                                # width of the piloted lane

x_total    <- pilot_x0 + pilot_w



### 7. Part 1: stars and lightsabers

# Random stars in the background. set.seed() makes them the same every time.
# Stars are only kept where there is no text:
#  - only in the lightsaber area (not behind the film strip or piloted text)
#  - only in the gaps BETWEEN the rows
#  - not behind the headers at the top
star_col <- "white"     # try e.g. "#FFF3B0" for pale yellow stars

set.seed(1977)
stars <- tibble(
  x     = runif(600, 0, dot_lim),
  y     = runif(600, 0.4, 12),
  size  = runif(600, 0.03, 0.6),               # small stars
  alpha = runif(600, 0.08, 0.4)                # dim stars
) |>
  filter(abs(y - round(y)) > 0.4,              # far from the nearest row
         y < 10.3)                             # not behind the headers

p <- ggplot(extremes, aes(y = name)) +
  annotate("point", x = stars$x, y = stars$y, size = stars$size,
           alpha = stars$alpha, colour = star_col) +
  annotate("rect", xmin = 18.5, xmax = 25, ymin = -Inf, ymax = Inf,   # 'normal' human BMI
           fill = "white", alpha = 0.07) +
  
  # Lightsaber: grey handle, then a blade built from three layers:
  # a wide faint glow, a narrower stronger glow, and a thin white core
  geom_segment(aes(x = 0, xend = hilt_end, yend = name),
               colour = hilt_col, linewidth = 3.4) +
  geom_segment(aes(x = hilt_end, xend = bmi, yend = name, colour = group),
               linewidth = 7, alpha = 0.15, lineend = "round", show.legend = FALSE) +
  geom_segment(aes(x = hilt_end, xend = bmi, yend = name, colour = group),
               linewidth = 3.5, alpha = 0.4, lineend = "round") +
  geom_segment(aes(x = hilt_end, xend = bmi, yend = name),
               colour = "white", linewidth = 1.1, lineend = "round") +
  geom_text(aes(x = bmi, label = sprintf("%.1f", bmi), colour = group),
            hjust = -0.5, size = 3.8 * fs, fontface = "bold", family = body_font,
            show.legend = FALSE)



### 8. Part 2: film strip

film_order <- c("The Phantom Menace", "Attack of the Clones", "Revenge of the Sith",
                "A New Hope", "The Empire Strikes Back", "Return of the Jedi",
                "The Force Awakens")

appearances <- extremes |>
  select(name, films) |>
  unnest_longer(films) |>                       # one row per character and film
  mutate(episode = match(films, film_order), appears = TRUE) |>
  select(name, episode, appears)


film_tiles <- expand_grid(name = extremes$name, episode = 1:7) |>   # all combinations
  left_join(appearances, by = c("name", "episode")) |>
  mutate(appears = replace_na(appears, FALSE),
         x       = film_x[episode])


p <- p +
  geom_tile(data = film_tiles, aes(x = x, y = name, fill = appears),
            width = film_w * 0.78, height = 0.6, colour = skill_col, linewidth = 0.4) +
  annotate("text", x = film_x, y = 10.75,
           label = c("I", "II", "III", "IV", "V", "VI", "VII"),
           size = 3.3 * fs, fontface = "bold", colour = text_dim, family = body_font) +
  annotate("text", x = mean(film_x), y = 11.4, label = "FILMS (episode)",
           size = 3.9 * fs, fontface = "bold", colour = skill_col, family = title_font)



### 9. Part 3: piloted crafts, written out by name

# One text label per character, e.g.
#   "Starships: X-wing, Imperial shuttle"
#   "Vehicles: Snowspeeder"
# Long lists are wrapped onto several lines with str_wrap().

pilot_label <- function(starships, vehicles) {
  lines <- c(
    if (length(starships) > 0) paste0("Starships: ", paste(starships, collapse = ", ")),
    if (length(vehicles)  > 0) paste0("Vehicles: ",  paste(vehicles,  collapse = ", "))
  )
  if (length(lines) == 0) return("none")
  paste(str_wrap(lines, width = round(45 / fs)), collapse = "\n")   # shorter lines for larger text
}

pilot_text <- extremes |>
  mutate(label = map2_chr(starships, vehicles, pilot_label))   # starships, vehicles are list-columns

p <- p +
  geom_text(data = filter(pilot_text, label != "none"),
            aes(x = pilot_x0, y = name, label = label),
            hjust = 0, size = 3 * fs, lineheight = 0.9, colour = skill_col, family = body_font) +
  geom_text(data = filter(pilot_text, label == "none"),
            aes(x = pilot_x0, y = name, label = label),
            hjust = 0, size = 3.3 * fs, colour = text_dim, fontface = "italic", family = body_font) +
  annotate("text", x = pilot_x0, y = 11.4, label = "PILOTED",
           hjust = 0, size = 3.9 * fs, fontface = "bold", colour = skill_col, family = title_font)



### 10. Divider, scales, text and theme

x_breaks <- pretty(c(0, dot_max))

x_breaks <- x_breaks[x_breaks <= dot_lim]       # only show ticks under the dot plot


group_text <- switch(group_choice,
                     all      = "characters",
                     human    = "human characters",
                     nonhuman = "non-human characters (droids included)")


p <- p +
  annotate("segment", x = 0, xend = x_total, y = 5.5, yend = 5.5,
           linetype = "dashed", colour = text_dim) +
  annotate("text", x = 21.75, y = 10.9, label = "Human\n'normal'",
           size = 3.2 * fs, colour = text_dim, lineheight = 0.9, vjust = 0, family = body_font) +
  scale_colour_manual(values = group_cols, name = NULL) +
  scale_fill_manual(values = c(`TRUE` = skill_col, `FALSE` = space_bg), guide = "none") +
  scale_x_continuous(breaks = x_breaks, expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(add = c(0.6, 2))) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, linewidth = 3))) +
  labs(
    title    = "Who has the most (and least) athletic build in the galaxy?",
    subtitle = paste0("The five heaviest and five lightest builds by Body Mass Index (BMI), ",
                      "among the ", n_total, " Star Wars ", group_text,
                      " with known height and mass.\n",
                      "Does build say anything about screen time or piloting skills?"),
    caption  = paste0("Data: starwars, dplyr R package. Films: episode I (The Phantom Menace) ",
                      "to VII (The Force Awakens); filled square = appears in that film.\n",
                      "BMI = mass / height² (kg/m²). Shaded band: 'normal' human BMI (18.5-25). ",
                      "It ignores muscle, armour, metal parts and Force sensitivity."),
    x = "BMI",
    y = NULL
  ) +
  theme_minimal(base_size = 13 * fs, base_family = body_font) +
  theme(
    plot.background       = element_rect(fill = space_bg, colour = NA),
    panel.background      = element_rect(fill = space_bg, colour = NA),
    legend.key            = element_rect(fill = space_bg, colour = NA),
    text                  = element_text(colour = text_main),
    plot.title            = element_text(family = title_font, face = "bold", size = 20 * fs,
                                         colour = title_col),
    plot.title.position   = "plot",
    plot.subtitle         = element_text(colour = text_main, margin = margin(b = 10)),
    plot.caption          = element_text(hjust = 0, colour = text_dim, size = 9 * fs),
    plot.caption.position = "plot",
    legend.position       = "top",
    legend.justification  = "left",
    legend.text           = element_text(colour = text_main),
    panel.grid            = element_blank(),
    axis.text.x           = element_text(colour = text_dim),
    axis.text.y           = element_text(size = 12 * fs, colour = text_main),
    axis.ticks.x          = element_line(colour = text_dim),
    axis.title.x          = element_text(hjust = (dot_lim / 2) / x_total,  # centred under the sabers
                                         colour = text_dim),
    plot.margin           = margin(20, 20, 15, 20)
  )



### 11. Box with the outliers, outside the plot

if (nrow(outliers) > 0) {
  outlier_lines <- sprintf("%s\nBMI = %.0f", outliers$name, outliers$bmi)
  
  callout <- paste0(
    "NOT SHOWN\n(BMI above ", round(cutoff, 1), ")\n\n",
    paste(outlier_lines, collapse = "\n\n"),
    "\n\nToo big for the axis."
  )
  
  # Make the right margin wide enough for the longest name
  right_margin <- max(170, 8 * max(nchar(outliers$name))) * fs
  
  p <- p +
    annotate("label", x = x_total * 1.03, y = 5.5, label = callout,
             hjust = 0, size = 3.8 * fs, family = body_font,
             colour = title_col, fill = "#141726") +
    theme(plot.margin = margin(20, right_margin, 15, 20))
}

p <- p + coord_cartesian(xlim = c(0, x_total), clip = "off")   # clip = "off": draw outside


### 12. Save

# ragg (png) and cairo_pdf (pdf) can use fonts installed on the computer
save_device <- if (str_detect(out_file, "\\.pdf$")) cairo_pdf else ragg::agg_png

ggsave(out_file, p, width = 14, height = 8, dpi = 300, bg = space_bg, device = save_device)
message("Saved infographic to ", out_file)
