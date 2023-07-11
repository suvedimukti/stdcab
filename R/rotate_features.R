#--------------------- fun-05 rotate features ---------------------------------#
#' Rotate Features
#'
#' This function rotates the features of an input spatial object by the
#' specified rotation angle and rotation base.
#'
#' @param input_sf The input spatial object to be rotated.
#' It can be an 'sf' object or a 'Spatial' object.
#' @param rotation_angle The angle of rotation in degrees.
#' @param rotation_base The rotation base. It can be "center" or a
#' coordinate specifying the rotation base.
#'
#' @return A list containing the rotation base and the rotated features as an 'sf' object.
#'
#' @examples
#' # Load sample data
#' data(landcover)
#'
#' # Rotate the landcover data by 45 degrees around the center
#'
#' rot_coords <- rotate_features(input_sf = landcover, rotation_angle = 45, rotation_base = "center")
#'
#' @import sf
#' @importFrom sf st_geometry
#' @importFrom sf st_centroid
#' @importFrom sf st_drop_geometry
#' @importFrom sf st_as_sf
#'
#' @export
#------------------------------------------------------------------------------#
rotate_features <- function(input_sf, rotation_angle = 45,
                            rotation_base = "center") {
  # Define rotation matrix
  rotation_matrix <- \(rotation_angle){
    matrix(
      c(
        cos(rotation_angle * pi / 180),
        sin(rotation_angle * pi / 180),
        -sin(rotation_angle * pi / 180),
        cos(rotation_angle * pi / 180)
      ),
      nrow = 2
    )
  }
  #---------------------------------------------------------------------------#
  if (class(sf::st_geometry(input_sf)[[1]])[2] == "POINT") {
    bb <- sf::st_bbox(input_sf)
    x1 <- bb[1]
    y1 <- bb[2]
    x2 <- bb[3]
    y2 <- bb[4]

    square <- function(x1, y1, x2, y2) {
      sf::st_polygon(list(matrix(c(x1, x2, x2, x1, x1, y1, y1, y2, y2, y1), 5)))
    }
    sq_poly <- square(x1, y1, x2, y2)
  }
  input_sf_geom <- sf::st_geometry(sq_poly)

  #----------------------------------------------------------------------------#
  if (!is.null(rotation_base) || (rotation_base == "center") ||
      rotation_base == "center" && class(sf::st_geometry(input_sf)[[1]])[2] == "POINT") {
    rotation_base <- sf::st_centroid(input_sf_geom)
    input_sf_geom <- sf::st_geometry(input_sf)
    input_fields <- sf::st_drop_geometry(input_sf)
  } else {
    input_sf_geom <- sf::st_geometry(input_sf)
    input_fields <- sf::st_drop_geometry(input_sf)
  }
  rotated_geom <- (input_sf_geom - rotation_base) * rotation_matrix(rotation_angle) + rotation_base
  rotated_geom_fields <- cbind(input_fields, rotated_geom)
  rotated_geom_sf <- sf::st_as_sf(rotated_geom_fields, crs = sf::st_crs(input_sf))

  return_list <- list()

  return_list[["rotation_base"]] <- rotation_base
  return_list[["rotated_geom"]] <- rotated_geom_sf
  return(return_list)
}
