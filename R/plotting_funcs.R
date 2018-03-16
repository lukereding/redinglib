library(tidyverse)

#' with_background
#'
#' Change the background color of a ggplot.
#' @param plot The plot you want to change.
#' @param fill The color you want to change it to. Default is black.
#' @param color The color you want to change the background to. Default is NA.
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' with_background(p, "pink")

with_background <- function(plot, fill = "black", color = "NA") {
  if(class(plot)[1] != "gg") {
    stop("is `plot` a ggplot2 plot?")
  }

  plot + theme(plot.background = element_rect(fill = fill, colour = color),
               panel.background = element_rect(fill = fill, colour = color))

}



#' Plot a histogram for every numeric variable in your dataset.
#'
#' Plot a histogram for every numeric variable in your dataset.
#' @param data A dataframe with numeric columns.
#' @export
#' @examples
#' plot_histograms(mtcars)

plot_histograms <- function(data) {
  data %>%
    select_if(is.numeric) %>%
    gather(variable, value) %>%
    ggplot(aes(x = value)) +
    geom_histogram() +
    facet_wrap(~variable, scale = "free") +
    theme_minimal()
}

#' Plot a bar plot of counts for every non-numeric variable in your dataset.
#'
#' Plot a bar plot of counts for every factor/character variable in your dataset.
#' @param data A dataframe.
#' @export
#' @examples
#' plot_counts(diamonds)

plot_counts <- function(data) {
  data %>%
    select_if(function(x) is.factor(x) || is.character(x)) %>%
    gather(variable, value) %>%
    ggplot(aes(x = value)) +
    geom_bar() +
    facet_wrap(~variable, scale = "free") +
    theme_minimal()
}


#' Plot continous x categorical relationships in your dataset.
#'
#' Plot the relationship between some categorcial variable and each numeric variable.
#' @param data A dataframe.
#' @param categorical_variable The bare (unquoted) name of the categorcial variable.
#' @export
#' @examples
#' plot_cat_relationship(diamonds, clarity)

plot_cat_relationship <- function(data, categorical_variable) {
  numeric_cols <- names(data)[map_lgl(data, is.numeric)]
  enquo_cat <- enquo(categorical_variable)

  data %>%
    select(!! enquo_cat, numeric_cols) %>%
    gather(variable, value, -!! enquo_cat) %>%
    ggplot(aes_string(x = quo_name(enquo_cat), y = "value", color = quo_name(enquo_cat))) +
    geom_boxplot() +
    facet_wrap(~variable, scales = "free") +
    coord_flip() +
    # scale_color_brewer(type = "qual", palette = "Dark2", guide = F) +
    ggtitle(paste0("relationships with ", quo_name(enquo_cat))) +
    theme_minimal()
}

#' Add axes to a ggplot
#'
#' Add axes to a ggplot.
#' @param width Width of the lines. Defaults to 0.4.
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' p + add_axes()

add_axes <- function(width = 0.4) {
 theme(axis.line = element_line(colour = "black", size = width))
}

#' Remove axes from a ggplot
#'
#' Remove axes from a plot.
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(mpg, wt)) + geom_point() + add_axes()
#' p + remove_axes()

remove_axes <- function() {
 theme(axis.line = element_blank())
}


#' Rotate x-axis labels
#'
#' Rotates x-axis labels on a ggplot.
#' @param angle Angle to rotate the labels. Defaults to 45.
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' p + rotate_labels()

rotate_labels <- function(angle = 45) {
 theme(axis.text.x = element_text(angle = angle, hjust = 1))
}

#' remove_ticks_x
#'
#' Remove tick marks from the x-axis.
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' p + remove_ticks_x()

remove_ticks_x <- function() {
 theme(axis.ticks.x = element_blank())
}

#' move_ticks_inside_x
#'
#' Move tick marks inside the x-axis.
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' p + move_ticks_inside_x()

move_ticks_inside_x <- function() {
 theme(axis.ticks.length = unit(-0.25, "cm"), axis.text.x = element_text(margin = margin(10, 5, 15, 5, "pt")))
}


#' move_ticks_inside_y
#'
#' Move tick marks inside the y-axis.
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' p + move_ticks_inside_y()

move_ticks_inside_y <- function() {
 theme(axis.ticks.length = unit(-0.25, "cm"), axis.text.y = element_text(margin = margin(10, 10, 15, 5, "pt")))
}


#' A simple plotting theme.
#'
#' My preferred plotting theme. No gridlines and larger axis labels.
#' @param font_size Base size of the font. Default is 14.
#' @param font_family Font family.
#' @param line_size Size of axis ticks. Defauts to 0.4.
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' p + theme_simple()

theme_simple <- function (font_size = 14, font_family = "", line_size = 0.4) {

  small_factor <- 0.75
  small_size <- font_size * small_factor

  theme_minimal(base_size=font_size, base_family=font_family) %+replace%
    theme(
      axis.text = element_text(size = small_size),
      axis.title = element_text(size = font_size),
      axis.line = element_line(size = line_size),
      strip.background = element_rect(fill = "grey85", color = NA),
      validate = TRUE,
      panel.grid = element_blank(),
      axis.ticks = element_line(size = line_size),
      legend.position = "bottom",
      plot.caption = element_text(size = small_size*0.85, margin = margin(4.95, 0,0,0), hjust = 1)
    )
}


#' A simple theme for showing continuous x categorical data.
#'
#' My preferred plotting theme for categorical x numeric data. Major gridlines, larger axis labels, and no ticks on the x-axis.
#' @param font_size Base size of the font. Default is 14.
#' @param font_family Font family.
#' @param line_size Size of axis ticks. Defauts to 0.4.
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(factor(cyl), wt)) + geom_boxplot()
#' p + theme_simple_hlines()

theme_simple_hlines <- function (font_size = 14, font_family = "", line_size = 0.4) {

  small_factor <- 0.75
  small_size <- font_size * small_factor

  theme_minimal(base_size=font_size, base_family=font_family) %+replace%
    theme(
      axis.text = element_text(size = small_size),
      axis.title = element_text(size = font_size),
      axis.line = element_line(size = line_size),
      strip.background = element_rect(fill = "grey85", color = NA),
      validate = TRUE,
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "bottom",
      plot.caption = element_text(size = small_size*0.85, margin = margin(4.95, 0,0,0), hjust = 1)
    )
}

#' coord_radar
#'
#' Coordinate plotting scheme for radar plots.
#' @param theta Default is "x".
#' @param start Determines where the circle starts. Default is 0.
#' @param direction Determines the direction of plotting. Defaults to 1. Change to -1 to go in the opposite direction.
#' @export
#' @examples
#' mtcars %>% rownames_to_column( var = "car" ) %>% mutate_at(2:12, funs(scale)) %>% sample_n(6) %>% gather(mpg:carb, key = "character", value = "value") %>% arrange(character) %>% ggplot(aes(x=character, y=value, group = car, color = car)) + geom_polygon(fill=NA, aes(color = car), size = 1.5) + geom_polygon(aes(fill = car), fill = NA, size = 2)+ coord_radar()

coord_radar <- function(theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") {
    "y"
  } else {
    "x"
  }
  ggproto(
    "CordRadar", CoordPolar, theta = theta, r = r, start = start,
    direction = sign(direction),
    is_linear = function(coord) TRUE
  )
}
