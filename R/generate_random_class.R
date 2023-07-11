## ------------fun 00-create random class data ---------------------------------#

#' A function to create random multiclass data.
#' This function allows you to create random spatial data for a specified number of classes (num_class)
#' and replicates (num_rep)
#' @param data Input extent of data 'sf', 'sp', or 'Vect'
#' @param num_class number of required classes
#' @param num_rep replication for each class
#' @param return_sf (logical) if true returns 'sf' object using coordinate
#' system of input data
#'
#' @return If 'return_sf' is TRUE, the function returns an 'sf' object containing the generated classes.
#' If 'return_sf' is FALSE, the function returns a data frame containing the generated classes.
#'
#' @examples
#' # Load sample data
#' data("landcover")
#'
#' # Generate random classes with 5 classes and 100 replicates per class
#' samp_data <- generate_random_class(landcover, 5, 100,return_sf = FALSE)
#' head(samp_data)
#'
#' @export
generate_random_class <- function(data, num_class, num_rep, return_sf = TRUE) {
  if (inherits(data, c(
    "SpatialPoints", "SpatialPointsDataFrame",
    "SpatialPolygonsDataFrame", "SpatVector"
  ))) {
    data <- sf::st_as_sf(data)
  } else if (inherits(data, c("sf", "data.frame"))) {
    data <- data
  } else {
    stop("Input data should be a 'sf','sp',or'Vect'")
  }
  # calculate extent
  extent <- sf::st_bbox(data)
  # Calculate the min max x and y of data
  xmin <- extent[1]
  ymin <- extent[2]
  xmax <- extent[3]
  ymax <- extent[4]
  # create a empty list, data frame should also work, for the simplicity, I am suing
  # list
  class_list <- list()

  # Generate random classes
  for (i in 1:num_class) {
    set.seed(13 + i)
    # Generate random coordinates within the study area extent for the current class
    x_coords <- stats::runif(num_rep, xmin, xmax)
    y_coords <- stats::runif(num_rep, ymin, ymax)
    # Create a data frame for the current class
    class_df <- data.frame(
      x = x_coords,
      y = y_coords,
      class = factor(rep(i, num_rep)),
      stringsAsFactors = FALSE
    )
    class_list[[i]] <- class_df
  }
  # combine class_list to df.
  class_df <- do.call(rbind, class_list)
  if (return_sf == TRUE) {
    class_sf <- sf::st_as_sf(class_df, coords = c("x", "y"))
    sf::st_crs(class_sf) <- sf::st_crs(data)
    return(class_sf)
  } else {
    # Return the generated classes data frame
    return(class_df)
  }
}
