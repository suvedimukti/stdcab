#----- fun 03 - spatial_hexgrid-sample ----------------------------------------#
#' Spatial Hexagonal Grid Sampling
#'
#' This function generates a hexagonal grid and performs spatial sampling based
#' on the provided data and parameters.
#'
#' @param data The input data. It can be either an `sf` object or a `SpatialPoints` object.
#' @param sidelength The length of the hexagon's side.
#' @param top_type The type of the hexagon top, either "flat" or "pointy".
#' Default is "flat".
#' @param show_grid Logical value indicating whether to display the hexagonal grid.
#' Default is TRUE.
#' @param fold_selection The method for fold selection. Options include "default",
#' "systematic", or "random". Default is "default".
#' @param k The number of folds for fold selection. Required when
#' 'fold_selection' is "systematic" or "random". Default is NULL.
#'
#' @return A list containing the plots (if 'show_grid' is TRUE),
#' the hexagonal blocks (sf object), and the split objects.
#'
#' @examples
#' # Load sample data
#' data(landcover)
#'
#' # Generate a spatial hexagonal grid sampling with a sidelength of
#' # 10 km and 5 folds using systematic selection
#' hexgrid <- spatial_hexgrid_sample(landcover, sidelength = 10000,
#'             fold_selection = "systematic", k = 5)
#'
#'
#' @importFrom ggplot2 ggplot aes scale_colour_viridis_d geom_sf geom_sf_text labs ggtitle theme_bw element_text
#' @importFrom sf st_sfc st_crs st_bbox st_as_sf st_cast st_sf st_intersection st_centroid
#' @importFrom methods is
#' @importFrom rlang env_bind eval_tidy expr_label  new_quosure
#' @importFrom utils install.packages
#' @importFrom purrr map
#' @importFrom dplyr filter
#' @importFrom ggplot2 element_text theme
#' @export
spatial_hexgrid_sample <- function(data, sidelength, top_type = "flat", show_grid = TRUE,
                                   fold_selection = "default", k = NULL) {
  if (show_grid) {
    # Check for availability of ggplot2 and sf
    pkg <- c("ggplot2", "sf")
    pkgna <- names(which(sapply(sapply(pkg, find.package, quiet = TRUE), length) == 0))

    if (length(pkgna) > 0) {
      message(
        "This function requires ", print(paste0(pkg, collapse = ", ")), " package for plotting.",
        "\nWould you like to install it now?\n1: yes\n2: no"
      )

      user <- readline(prompt = paste0("Selection: "))

      if (tolower(user) %in% c("1", "yes", "y")) {
        utils::install.packages(pkgna)
      } else {
        stop("Please install ggplot2 package or set show_grid = FALSE.")
      }
    }

    invisible(lapply(pkg, library, character.only = TRUE))
  }

  if (!methods::is(data, "sf")) {
    if (methods::is(data, "SpatialPoints") || methods::is(data, "SpatialPointsDataFrame") ||
        methods::is(data, "SpatVector") || methods::is(data %in% c("POlygon", "Polygons", "SpatialPoygons"))) {
      data <- sf::st_as_sf(data)
      crs <- sf::st_crs(data)
    } else {
      stop("data should be a `spatial` (sp) or `sf` objects")
    }
  }

  bbox <- sf::st_bbox(data) # Calculate the bounding box
  size <- sidelength / 2

  if (top_type %in% c("flat", "flat_topped")) {
    # Calculate the required steps for grid construction
    horizontal_step <- 1.5 * size
    vertical_step <- sqrt(3) * size

    # Calculate the starting positions
    x_min <- min(bbox[1], bbox[3])
    x_max <- max(bbox[1], bbox[3])
    center_y_min <- min(bbox[2], bbox[4])
    center_y_max <- max(bbox[2], bbox[4])

    horizontal_skip <- ceiling(x_min / horizontal_step) - 1
    vertical_skip <- ceiling(center_y_min / vertical_step) - 1

    vertical_start <- vertical_skip * vertical_step
    horizontal_start <- horizontal_skip * horizontal_step

    horizontal_end <- x_max + horizontal_step
    vertical_end <- center_y_max + vertical_step

    if (vertical_start - (vertical_step / 2.0) < center_y_min) {
      start_y <- c(vertical_start + (vertical_step / 2.0), vertical_start)
    } else {
      start_y <- c(vertical_start - (vertical_step / 2.0), vertical_start)
    }

    vertical_start_idx <- abs(horizontal_skip) %% 2 + 1

    current_x <- horizontal_start
    current_center_y <- start_y[vertical_start_idx]
    vertical_start_idx <- (vertical_start_idx %% 2) + 1

    # Create an empty list to store hexagons
    hexagons <- list()

    while (current_x < horizontal_end) {
      while (current_center_y < vertical_end) {
        hexagons[[length(hexagons) + 1]] <- create_hexagon(
          current_x,
          current_center_y, size, top_type
        )
        current_center_y <- current_center_y + vertical_step
      }
      current_x <- current_x + horizontal_step
      current_center_y <- start_y[vertical_start_idx]
      vertical_start_idx <- (vertical_start_idx %% 2) + 1
    }
  } else if (top_type %in% c("pointy", "pointed", "pointed_top")) {
    # Calculate the required steps for grid construction
    horizontal_step <- sqrt(3) * size
    vertical_step <- size * 1.5

    # Calculate the starting positions
    x_min <- min(bbox[1], bbox[3])
    x_max <- max(bbox[1], bbox[3])
    y_min <- min(bbox[2], bbox[4])
    y_max <- max(bbox[2], bbox[4])

    horizontal_steps_to_skip <- ceiling(x_min / horizontal_step) - 1
    vertical_steps_to_skip <- ceiling(y_min / vertical_step) - 1

    vertical_start <- vertical_steps_to_skip * vertical_step

    # Create an empty list to store hexagons
    hexagons <- list()

    while (vertical_start <= y_max) {
      horizontal_start <- horizontal_steps_to_skip * horizontal_step +
        (vertical_steps_to_skip %% 2) * (horizontal_step / 2)
      current_center_x <- horizontal_start
      current_center_y <- vertical_start

      while (current_center_x <= x_max) {
        hexagons[[length(hexagons) + 1]] <- create_hexagon(
          current_center_x,
          current_center_y, size, top_type
        )
        current_center_x <- current_center_x + horizontal_step
      }

      vertical_steps_to_skip <- vertical_steps_to_skip + 1
      vertical_start <- vertical_start + vertical_step
    }
  } else {
    stop("Invalid top_type. Must be 'flat' or 'pointy'.")
  }

  sblock <- sf::st_sfc(hexagons, crs = sf::st_crs(data))
  sblock <- sf::st_cast(sblock, "MULTIPOLYGON")
  sblock <- sf::st_sf(geometry = sblock)
  sblock[, "gname"] <- 1:nrow(sblock)

  if (k > nrow(sblock) && !fold_selection == "default") {
    stop(
      "'k' is bigger than the number of grids when block selection is not default\n",
      "The number of total grid count is: ", nrow(sblock)
    )
  } else if (k < 2 && !fold_selection == "default") {
    stop("'k' must be 2 or higher for `systematic` and `random` selection")
  }

  if (fold_selection == "systematic") {
    sblock$sgname <- systematic_sequence(object = sblock, k = k)
  } else if (fold_selection == "random") {
    sblock$sgname <- 0L
    num <- floor(nrow(sblock) / k)
    sblock$sgname[seq_len(num * k)] <- sample(rep(seq_len(k), num), num * k)

    if (nrow(sblock) %% k != 0) {
      rest <- nrow(sblock) %% k
      unfold <- which(sblock$sgname == 0)
      sblock$sgname[unfold] <- sample(seq_len(k), rest, replace = FALSE)
    }
  } else {
    k <- NULL
    sblock$sgname <- sblock$gname
    warning("Grid selection is 'default', input value of k is ignored")
  }

  sblock[, "obs"] <- lengths(sf::st_intersects(sblock, data))
  sblock <- dplyr::filter(sblock, obs > 0L)
  data <- sf::st_intersection(sblock, data)

  if (show_grid == TRUE) {
    gp2 <- ggplot2::ggplot() +
      ggplot2::geom_sf(
        data = sblock, color = "purple",
        fill = "orangered4",
        alpha = 0.04,
        size = 0.7
      ) +
      ggplot2::geom_sf(data = data, ggplot2::aes(color = factor(sgname)), show.legend = FALSE) +
      ggplot2::scale_colour_viridis_d(option = "plasma") +
      ggplot2::geom_sf_text(ggplot2::aes(label = paste("(", obs, ",", sgname, ")")),
                            fun.geometry = sf::st_centroid, data = sblock, color = "blue"
      ) +
      ggplot2::labs(x = "", y = "") +
      ggplot2::ggtitle(paste0("Spatial hex grid ", ": ", toupper(fold_selection), " fold selection")) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          size = 14,
          face = "bold", hjust = 0.5
        ),
        text = ggplot2::element_text(size = 12),
        axis.title = ggplot2::element_text(face = "bold"),
        axis.text.x = ggplot2::element_text(size = 11)
      )
    plot(gp2)
  } else {
    gp2 <- NULL
  }

  split_objs <- spatial_grid_clustering_splits(data = data)
  v <- length(unique(data$sgname))
  split_objs$splits <- purrr::map(split_objs$splits, rm_out)
  cv_att <- list(v = v, repeats = 1)

  rsample::new_rset(
    splits = split_objs$splits,
    ids = split_objs[, grepl("^id", names(split_objs))],
    attrib = cv_att,
    subclass = c("spatial_clustering_cv", "rset")
  )

  return_list <- list(
    plots = gp2,
    blocks = sblock,
    splits = split_objs
  )
  return(return_list)
}

#---------- spatial_grid_clustring_splits -------------------------------------#
spatial_grid_clustering_splits <- function(data = data) {
  xy <- as.data.frame(sf::st_coordinates(data))
  data <- sf::st_drop_geometry(data)
  data <- cbind(data, xy)
  v <- length(unique(data$sgname))
  n <- nrow(data)
  folds <- data$sgname
  idx <- seq_len(n)
  indices <- split_unnamed(idx, folds)
  indices <- lapply(indices, default_complement, n = n)
  split_objs <- purrr::map(indices, rsample::make_splits,
                           data = data,
                           class = "spatial_clustering_split"
  )
  tibble::tibble(
    splits = split_objs,
    id = names0(length(split_objs), "Fold")
  )
}

#--------------- Systematic Sequence ------------------------------------------#
systematic_sequence <- function(object, k = k) {
  bl <- nrow(object) # Get the number of rows of the object
  if (bl %% k == 0) {
    size <- bl / k
    bref <- rep(1:k, size) # Regular sequence of size by k
  } else {
    size <- floor(bl / k)
    bsize <- bl %% k
    bref <- c(rep(1:k, size), 1:bsize)
  }
  return(bref)
}

#### - Other Miss functions -####

split_unnamed <- function(x, f) {
  out <- split(x, f)
  unname(out)
}
#------------ default
# Get the indices of the analysis set from the assessment set
default_complement <- function(ind, n) {
  list(
    analysis = setdiff(1:n, ind),
    assessment = ind
  )
}
#---------- rm_out
## This will remove the assessment indices from an rsplit object
rm_out <- function(x) {
  x$out_id <- NA
  x
}

names0 <- function(num, prefix = "x") {
  if (num == 0L) {
    return(character())
  }
  ind <- format(1:num)
  ind <- gsub(" ", "0", ind)
  paste0(prefix, ind)
}
