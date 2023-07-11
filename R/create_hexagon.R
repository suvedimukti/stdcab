#------------ fun02 - create hexagons -----------------------------------------#
#' Create Hexagons
#'
#' This function creates a hexagon polygon based on the provided pair of
#' center coordinates, size, and top type.
#'
#' @param center_x The x-coordinate of the hexagon center.
#' @param center_y The y-coordinate of the hexagon center.
#' @param size The size of the hexagon. It should be based on projection of input data
#' @param top_type The type of the hexagon top, either "flat" or "pointy".
#'
#' @return An 'sf' polygon representing the created hexagon.
#' @examples
#' # Create a flat-topped hexagon with center coordinates (419093.9, 3149336) and size 5km
#' flat_top <- create_hexagon(419093.9, 3149336, 50000, "flat")
#' # plot
#' plot((flat_top))
#' # create a pointy-topped hexagon with same arguments
#' pointy_top <- create_hexagon(419093.9, 3149336, 50000, "pointy")
#' # plot(pointy_top)
#'
#' @importFrom sf st_polygon
#' @export
create_hexagon <- function(center_x, center_y, size, top_type) {
  if (top_type == "flat" || top_type == "flat_topped") {
    angles <- seq(0, 300, by = 60)
  } else if (top_type == "pointy" || top_type == "pointed" || top_type == "pointed_top") {
    angles <- seq(30, 360, by = 60)
  } else {
    stop("Invalid top_type. Must be 'flat' or 'pointy'.")
  }

  vertices <- matrix(0, nrow = length(angles), ncol = 2)

  for (i in 1:length(angles)) {
    angle <- angles[i]
    vertices[i, 1] <- center_x + size * cos(angle * pi / 180)
    vertices[i, 2] <- center_y + size * sin(angle * pi / 180)
  }

  # Close the polygon by duplicating the first vertex
  vertices <- rbind(vertices, vertices[1, ])

  # Create an sf polygon
  polygon <- sf::st_polygon(list(vertices))

  return(polygon)
}
