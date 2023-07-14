#----------------- fun04- spatial Quadgrid sample ------------------#
#' Spatial Quadgrid Sampling
#'
#' @description
#' This function generates a regular rectangular grids and performs spatial
#' sampling based on the provided data and parameters.
#' The result from this function is similar to `Generate Tessellation`
#' tool in ESRI's ArcGIS software but only square or rectangle polygons are
#' possible. The extent of the point coordinates is
#' divided into number of possible grids based on the values of `cellsize`.
#' The `cellsize` is the length and width of polygon to be created.
#' At this point `spatial_quadgrid_sample` and `spatial_hexgrid_sample`are different
#' function. The reason is because rotation of original data and resultant grid
#' is possible for regular rectangular grids. In the future both functions can be merged
#' together
#'
#' @param data The input data. It can be either an 'sf' object or a `SpatialPoints` object.
#' @param cellsize The size of the grid cells, provided as a numeric vector
#' with two values representing the x and y dimensions.
#' @param offset The offset of the grid, provided as a numeric vector with
#' two values representing the x and y offsets.
#' @param show_grid Logical value indicating whether to display the `quadgrid`.
#' Default is TRUE.
#' @param rotation_angle The angle of rotation for the `quadgrid`. Default is NULL.
#' @param fold_selection The method for fold selection. Options include
#' "default", "systematic", or "random". Default is "default".
#' @param k The number of folds for fold selection. Required when
#' `fold_selection` is "systematic" or "random". Default is NULL.
#'
#' @return A list containing the plots (if 'show_grid' is TRUE),
#' the `quadgrid` blocks (sf object), and the split objects.
#'
#' @references
#'
#' Pebesma, E., 2018. Simple Features for R: Standardized Support for Spatial Vector Data. The R
#' Journal 10 (1), 439-446, https://doi.org/10.32614/RJ-2018-009
#'
#' @details
#' The spatial grid sample generates tessellation or fishnet in ESRI's ArcGIS software,
#' Currently `sp`, `sf` and `Vect` data are allowed.
#' The grid shows the number of samples within the grid and grid number
#' e.g. (80,1). The `default` selection  numbers the grids from bottom left
#' and increases row wise.
#' The `random` selection assigns the grid number randomly.
#' The `systematic` grid numbering happens from bottom right and column wise.
#'
#' The `random` selection as its' name applies select grids randomly, and
#' `systematic` selection option
#' allows to select the grids sequentially up to `k`
#' @export
spatial_quadgrid_sample <- function(data,
                                    cellsize = c(10000, 10000),
                                    offset = c(0, 0),
                                    show_grid = TRUE,
                                    rotation_angle = NULL,
                                    fold_selection = "default",
                                    k = NULL) {
  if (show_grid) {
    # Check for availability of ggplot2 and sf
    pkg <- c("ggplot2", "sf")
    pkgna <- names(which(sapply(sapply(pkg, find.package, quiet = TRUE), length) == 0))

    if (length(pkgna) > 0) {
      message(
        "This function requires ",
        print(paste0(pkg, collapse = ", ")),
        " package for plotting.",
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
    if (methods::is(data, "SpatialPoints") ||
        methods::is(data, "SpatialPointsDataFrame") ||
        methods::is(data, "SpatVector") ||
        methods::is(data %in% c("POlygon", "Polygons", "SpatialPoygons"))) {
      data <- sf::st_as_sf(data)
      crs <- sf::st_crs(data)
    } else {
      stop("data should be a `spatial` (sp) or `sf` objects")
    }
  }

  # Handle rlang::is_missing values for correct use of function
  if (rlang::is_missing(data) && rlang::is_missing(cellsize) && rlang::is_missing(offset)) {
    stop("several missing arguments, make sure, input object or cellsize is provided")
  }
  #---------------------------------------------------------------------rotate
  if (!is.null(rotation_angle)) {
    data <- rotate_features(
      input_sf = data,
      rotation_angle = rotation_angle,
      rotation_base = "center"
    )$rotated_geom
    rotation_base <- rotate_features(
      input_sf = data,
      rotation_angle = rotation_angle,
      rotation_base = "center"
    )$rotation_base
  }
  #---------------------------------------------------------------- end rotate
  # Define bb_wrap from sf package
  bb_wrap <- function(bb) {
    stopifnot(is.numeric(bb) && length(bb) == 4)
    structure(as.double(bb), names = c("xmin", "ymin", "xmax", "ymax"), class = "bbox")
  }

  # Calculate size of n
  if (rlang::is_missing(offset)) {
    offset <- sf::st_bbox(data)[c("xmin", "ymin")]
  }

  bb <- if (!rlang::is_missing(cellsize) && !rlang::is_missing(offset)) {
    nx <- ceiling(diff(sf::st_bbox(data)[c(1, 3)] - offset[1]) / cellsize[1])
    ny <- ceiling(diff(sf::st_bbox(data)[c(2, 4)] - offset[2]) / cellsize[2])
    n <- c(nx, ny)
    cellsize <- rep(cellsize, length.out = 2)
    n <- rep(n, length.out = 2)
    bb_wrap(c(sf::st_bbox(data)[c(1, 2)] + offset[1], sf::st_bbox(data)[c(3, 4)] + offset[2] + n * cellsize))
  } else {
    sf::st_bbox(data)
  }

  # Calculate number when cellsize and offset is provided
  if (rlang::is_missing(cellsize)) {
    cellsize <- c(diff(sf::st_bbox(data)[c(1, 3)]), diff(sf::st_bbox(data)[c(2, 4)])) / n
  }

  cellsize_missing <- if (!missing(cellsize)) {
    cellsize <- rep(cellsize, length.out = 2)
    FALSE
  } else {
    TRUE
  }

  # When cellsize is missing
  if (cellsize_missing) {
    xc <- seq(bb[1] + offset[1], bb[3], length.out = nx + 1)
    yc <- seq(bb[2] + offset[2], bb[4], length.out = ny + 1)
  } else {
    xc <- bb[1] + offset[1] + (0:nx) * cellsize[1]
    yc <- bb[2] + offset[2] + (0:ny) * cellsize[2]
  }

  what <- "rectangle"
  if (what == "rectangle") {
    ret <- vector("list", nx * ny)
    square <- function(x1, y1, x2, y2) {
      sf::st_polygon(list(matrix(c(x1, x2, x2, x1, x1, y1, y1, y2, y2, y1), 5)))
    }
    for (i in 1:nx) {
      for (j in 1:ny) {
        ret[[(j - 1) * nx + i]] <- square(xc[i], yc[j], xc[i + 1], yc[j + 1])
      }
    }
  } else {
    stop("could not create square or rectangle grid")
  }

  grd <- sf::st_sfc(ret, crs = sf::st_crs(data)) # Create sfc_polygon

  #------------------------- Rotate Grid -------------------------------------#
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
  #----------------------------------------------------------------------------#
 if(!is.null(rotation_angle)){
  input_grid_geom <- st_geometry(grd)
  rotated_grid <- (input_grid_geom - rotation_base) * rotation_matrix(rotation_angle) + rotation_base
  rot_grid_sf <- sf::st_as_sf(rotated_grid, crs = sf::st_crs(data))
 }else
   rot_grid_sf <- sf::st_as_sf(grd, crs = sf::st_crs(data))

  #----------------------------------------------------------------------------#


  # rot_with_blck <- cbind(rot_grid_sf, st_drop_geometry(input_grid))
  # rot_with_inter <- st_intersects(input_sf,rot_with_blck)
  # eturn(rotated_polygon)

  # rot_with_blck[, "obs"] <- lengths(sf::st_intersects(rot_with_blck, rotated_polygon_sf))

  #---------------------------------------------------------- stop rotate grid
  # Create spatial grids based on length of grids/blocks
  sblock <- sf::st_sf(rot_grid_sf) %>%
    dplyr::mutate(gname = 1:length(lengths(grd)))

  # Intersect gname, with original data to transfer grid id information
  if (k > nrow(sblock) && !fold_selection == "default") {
    stop(
      "'k' is bigger than the number of grids when block selection is not default\n",
      "The number of total grids count is: ", nrow(sblock)
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
    sblock$sgname <- sblock$gname
    warning("grid selection is selected as 'default', input value of k is ignored")
  }

  sblock[, "obs"] <- lengths(sf::st_intersects(sblock, data))

  # Remove any grid with no observations
  sblock <- dplyr::filter(sblock, obs > 0L)

  data <- suppressWarnings(sf::st_intersection(sblock, data))

  if (show_grid) {
    gp2 <- ggplot2::ggplot() +
      ggplot2::geom_sf(
        data = rot_grid_sf, color = "purple",
        fill = "orangered4",
        alpha = 0.04,
        size = 0.7
      ) +
      ggplot2::geom_sf(data = data, ggplot2::aes(color = factor(sgname)), show.legend = FALSE) +
      ggplot2::scale_colour_viridis_d(option = "plasma") +
      # ggplot2::geom_sf_text(ggplot2::aes(label = paste("(", obs, ",", sgname, ")")),
      #                       fun.geometry = sf::st_centroid, data = sblock, color = "blue") +
      ggplot2::labs(x = "", y = "") +
      ggplot2::ggtitle(paste0("Spatial Grid ", ": ", toupper(fold_selection), " fold selection")) +
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
