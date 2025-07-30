# thanks to these links for helping with color palettes and palette structure
# https://franklloydwright.org/wp-content/uploads/2023/04/FLW1955TaliesinPalette.html
# https://www.ppgpaints.com/color/color-collections/fallingwater/the-colors-of-fallingwater
# https://thedecorologist.com/frank-lloyd-wright-colors/
# https://www.standardpaintandflooring.com

#' Display a color palette based on Fallingwater or Taliesin schemes
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_identity theme_minimal geom_text theme element_blank labs
#' @importFrom dplyr filter
#' @importFrom dplyr %>%
#'
#' @param palette_name Character, choose between "fallingwater" or "taliesin"
#'
#' @return None; A ggplot of the color palette will be displayed
#' @examples
#' show_all_colors_flw(palette_name = 'fallingwater')
#' @export
show_all_colors_flw <- function(palette_name = c('fallingwater', 'taliesin')) {
  palette_name <- match.arg(palette_name)
  # fallingwater is 3 rows of 5 columns, but taliesin is 4 rows of 8 columns with extra
  fallingwater_colors <- c("#62584c", "#ad3a3f", "#a58f84", "#d1caba", "#e4d9c5",
                           "#d3bc9c", "#c9baa3", "#e2b270", "#5e563f", "#4d663e",
                           "#5f686d", "#2e5069", "#814e4a", "#6e5745", "#cca884")

  taliesin_colors <- c("#8d8851", "#cb484d", "#daad38", "#d08149", "#414149", "#b16356", "#95af54", "#326ead",
                       "#168da3", "#db8574", "#e3b270", "#e9b794", "#a74f5b", "#76826a", "#9da073", "#977d70",
                       "#93493e", "#c47181", "#bcab7d", "#c7a987", "#dce6e5", "#6b9186", "#9b8c9f", "#f2dbbb",
                       "#fbb1a6", "#ceb99a", "#bfaa99", "#94afb6", "#acadaf", "#a0bcc0", "#e5ddc6", "#cabca2",
                       "#edc9c9", "#f2f3ee", "#ecdcc3", "#e5ddc8")

  if (palette_name == "fallingwater") {
    colors <- fallingwater_colors
    grid <- expand.grid(x = 1:5, y = 1:5)[1:length(colors), ]
  } else if (palette_name == "taliesin") {
    colors <- taliesin_colors
    grid <- expand.grid(x = 1:8, y = 1:5)[1:length(colors), ]
  }

  grid$color <- colors

  ggplot(grid, aes(x = x, y = y, fill = color)) +
    geom_tile(color = "black", size = 0.8) +
    scale_fill_identity() +
    geom_text(aes(label = color), color = "black", size = 5, fontface = "bold") +
    theme_minimal() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank()) +
    labs(title = "Available Colors")
}


#' Generate a color palette based on Fallingwater or Taliesin schemes
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_identity theme_minimal geom_text theme element_blank labs
#' @importFrom dplyr filter
#' @importFrom dplyr %>%
#'
#' @param palette_name Character, choose between "fallingwater" or "taliesin"
#' @param selected_color Hex color to center the palette around (run \code{\link{show_all_colors_flw}} to see options)
#'
#' @return A vector of neighboring colors; A ggplot object is attached as an attribute called "plot"
#' @examples
#' palette <- generate_palette_flw("fallingwater", "#ad3a3f")
#' palette # this will show the palette visually and display the hex codes in the R console
#' @export
generate_palette_flw <- function(palette_name = c('fallingwater', 'taliesin'), selected_color) {
  palette_name <- match.arg(palette_name)
  # fallingwater is 3 rows of 5 columns, but taliesin is 4 rows of 8 columns with extra
  fallingwater_colors <- c("#62584c", "#ad3a3f", "#a58f84", "#d1caba", "#e4d9c5",
                           "#d3bc9c", "#c9baa3", "#e2b270", "#5e563f", "#4d663e",
                           "#5f686d", "#2e5069", "#814e4a", "#6e5745", "#cca884")

  taliesin_colors <- c("#8d8851", "#cb484d", "#daad38", "#d08149", "#414149", "#b16356", "#95af54", "#326ead",
                       "#168da3", "#db8574", "#e3b270", "#e9b794", "#a74f5b", "#76826a", "#9da073", "#977d70",
                       "#93493e", "#c47181", "#bcab7d", "#c7a987", "#dce6e5", "#6b9186", "#9b8c9f", "#f2dbbb",
                       "#fbb1a6", "#ceb99a", "#bfaa99", "#94afb6", "#acadaf", "#a0bcc0", "#e5ddc6", "#cabca2",
                       "#edc9c9", "#f2f3ee", "#ecdcc3", "#e5ddc8")

  if (palette_name == "fallingwater") {
    colors <- fallingwater_colors
    grid <- expand.grid(x = 1:5, y = 1:5)[1:length(colors), ]
  } else if (palette_name == "taliesin") {
    colors <- taliesin_colors
    grid <- expand.grid(x = 1:8, y = 1:5)[1:length(colors), ]
  }

  # Assign colors
  grid$color <- colors

  # Find the position of the selected color
  selected_pos <- grid %>% filter(color == selected_color)
  if (nrow(selected_pos) == 0) {
    stop("selected_color not found in chosen palette")
  }

  # Identify neighbors (including diagonals)
  acceptable_palette <- grid %>%
    filter((abs(x - selected_pos$x) <= 1) &
             (abs(y - selected_pos$y) <= 1))  # Includes selected color

  # Extract color vector
  palette_vector <- acceptable_palette$color

  # Store the plot as an attribute
  attr(palette_vector, "plot") <- ggplot(grid, aes(x = x, y = y, fill = color)) +
    geom_tile(color = "black", size = 0.8) +
    scale_fill_identity() +
    geom_text(aes(label = color), color = "black", size = 5, fontface = "bold") +
    geom_tile(data = acceptable_palette, aes(x = x, y = y), color = "black", linewidth = 3, fill = NA) +
    theme_minimal() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank())

  invisible(palette_vector)  # Prevents automatic printing when calling generate palette
}
