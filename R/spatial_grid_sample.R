#' Spatial Grid/Block Cross-Validation
#'
#' @description Create the spatial rectangular grids. This function results is similar to `Generate Tessellation`
#' tool in ESRI's ArcGIS software but only square or rectangle polygons are possible. The extent of the point coordinates is #' divided into number of
#' possible grids based on the values of `cellsize`. The cellsize is the length and width of polygon to be created.
#'
#'
#' @param data input data set one of sp, sf
#' @param cellsize size of cell or grid to be created. This could be the range
#'        (distance) obtained from the \code{\link{fit_variogram}}, \code{\link{multiple_variogram}}
#' @param offset (optional) offset distance in projection unit
#' @param show_grid (logical) TRUE for plotting grid and FALSE otherwise
#' @param fold_selection an option to select observations within the grid(cellsize)
#' three options are valid: `default`, `random`, and `systematic`.
#' @param k (integer) the value defining the number of groups of grids out of total grids.
#'
#'
#' @return
#'  result is the list of `plot`, `sf` grid, and `tbl_df`, `tbl`, and `data.frame`
#' @export
#'
#' @references
#'
#' Pebesma, E., 2018. Simple Features for R: Standardized Support for Spatial Vector Data. The R
#' Journal 10 (1), 439-446, https://doi.org/10.32614/RJ-2018-009
#' @examples
#'
#' lcdat <- landcover
#'
#' set.seed(1318)
#' spg <- spatial_grid_sample(data = lcdat, cellsize = c(10000,10000),
#'                      show_grid = TRUE, fold_selection = "random", k = 5)
#'
#' \dontrun{
#' tgrid<- spatial_grid_cv(data = train,cellsize = c(10000, 10000),showGrids = TRUE,
#' fold_selection = "random", k = 6)}
#'
#' @details
#' The spatial grid sample generates tessellation or fishnet in ESRI's ArcGIS software,
#' Currently only sp and sf data are allowed. The grid shows the number of samples within the grid and grid
#'  number
#' e.g. (80,1). The `default` selection  numbers the grids from bottom left and increases row wise.
#' `random` selection assigns the grid number randomly. The `systematic` grid numbering happens from bottom
#'  right and column wise.
#'
#'    The `random` selection as its' name applies select grids randomly, and `systematic` selection option
#'    allows to select the grids sequentially up to `k`
#'
spatial_grid_sample <- function(data,
                                cellsize = c(10000, 10000),
                                offset = c(1000, 1000),
                                show_grid = TRUE, fold_selection = "default", k = NULL) {
  #-------- make coords = c("x", "y)
  #-------- and sp to "sf"
  #------- install ggplot
  if (show_grid) {
    # check for availability of ggplot2
    pkg <- c("ggplot2", "sf", "rlang")
    pkgna <- names(which(sapply(sapply(pkg, find.package, quiet = TRUE), length) == 0))
    #---------------------------#
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
  #--------------- is coordinates are not applied and spatial data is provided
  #--------------- if sp objects convert to sf
  if (!methods::is(data, "sf")) {
    if (methods::is(data, "SpatialPoints") || methods::is(data, "SpatialPointsDataFrame")) {
      data <- sf::st_as_sf(data)
      crs <- sf::st_crs(data)
    } else {
      stop("data should be a `spatial` (sp) or `sf` objects")
    }
  }

  #---------------------------- handle missing values for correct use of function

  if (rlang::is_missing(data) && rlang::is_missing(cellsize) &&
    rlang::is_missing(offset)) {
    # && is_missing(crs)) # create global 10 x 10 degree grid
    # return(st_make_grid(cellsize = c(10,10), offset = c(-180,-90), n = c(36,18),
    #                       crs = st_crs(4326), what = what))
    stop("several missing arguments, make sure, input object or cellsize is provided")
  }

  #--------------- suffle aruments


  #--------- define bb_wrap from sf package
  bb_wrap <- function(bb) {
    stopifnot(is.numeric(bb) && length(bb) == 4)
    structure(as.double(bb), names = c("xmin", "ymin", "xmax", "ymax"), class = "bbox")
  }

  #------------- calculate size of n

  if (rlang::is_missing(offset)) {
    offset <- sf::st_bbox(data)[c("xmin", "ymin")]
  }


  bb <- if (!rlang::is_missing(cellsize) && !rlang::is_missing(offset)) {
    nx <- ceiling(diff(sf::st_bbox(data)[c(1, 3)] - offset[1]) / cellsize[1])
    ny <- ceiling(diff(sf::st_bbox(data)[c(2, 4)] - offset[2]) / cellsize[2])
    n <- c(nx, ny)
    cellsize <- rep(cellsize, length.out = 2)
    n <- rep(n, length.out = 2)
    # bb_wrap(c(offset, offset + n * cellsize))
    bb_wrap(c(sf::st_bbox(data)[c(1, 2)] + offset[1], sf::st_bbox(data)[c(3, 4)] + offset[2] + n * cellsize))
  } else {
    sf::st_bbox(data)
  }


  # calculate number when cellsize and offset is provided
  if (rlang::is_missing(cellsize)) {
    cellsize <- c(diff(sf::st_bbox(data)[c(1, 3)]), diff(sf::st_bbox(data)[c(2, 4)])) / n
  }

  #------------------------------------------------------------------------------
  cellsize_missing <- if (!missing(cellsize)) {
    cellsize <- rep(cellsize, length.out = 2)
    FALSE
  } else {
    TRUE
  }
  #------------------------------------------------------------------------------

  #-------- when cellsize is missing --------------------------------------------

  # # corner points:
  if (cellsize_missing) {
    xc <- seq(bb[1] + offset[1], bb[3], length.out = nx + 1)
    yc <- seq(bb[2] + offset[2], bb[4], length.out = ny + 1)
    # xc = bb[1]+offset[1] + (0:nx) * cellsize[1]
    # yc = bb[2]+offset[2] + (0:ny) * cellsize[2]
  } else {
    xc <- bb[1] + offset[1] + (0:nx) * cellsize[1]
    yc <- bb[2] + offset[2] + (0:ny) * cellsize[2]
    # xc = seq(bb[1]+offset[1], bb[3], length.out = nx + 1)
    # yc = seq(bb[2]+offset[2], bb[4], length.out = ny + 1)
  }

  #------------------------------------------------------------------------------#
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

    # } else if (what == "centers") {
    # 	ret = vector("list", nx * ny)
    # 	cent = function(x1, y1, x2, y2)
    # 		st_point(c( (x1+x2)/2, (y1+y2)/2 ))
    # 	for (i in 1:nx)
    # 		for (j in 1:ny)
    # 			ret[[(j - 1) * nx + i]] = cent(xc[i], yc[j], xc[i+1], yc[j+1])
    # } else if (what == "corners") {
    # 	ret = vector("list", (nx + 1) * (ny + 1))
    # 	for (i in 1:(nx + 1))
    # 		for (j in 1:(ny + 1))
    # 			ret[[(j - 1) * (nx + 1) + i]] = st_point(c(xc[i], yc[j]))
  } else {
    stop("could not create square or rectangle grid")
  }

  # thelist <- list(grd = grd)
  # return(thelist)

  grd <- sf::st_sfc(ret, crs = sf::st_crs(data)) # create sfc_polygon

  # create spatial grids based on length of grids/blocks

  sblock <- sf::st_sf(grd) %>% dplyr::mutate(gname = 1:length(lengths(grd)))

  # Intersect gname, with original data to transfer grid id information
  #--------------------- fold_selectionection----------------------------------

  if (k > nrow(sblock) && !fold_selection == "default") {
    stop(
      "'k' is bigger than the number of grids when block slection is not default\n",
      "The number of total grids count is: ", nrow(sblock)
    )
  } else if (k < 2 && !fold_selection == "default") {
    stop("'k' must be 2 or higher for `systematic` and `random`selection")
  }
  #----------------------------------------------------------------------------#
  if (fold_selection == "systematic") {
    sblock$sgname <- systematic_sequence(object = sblock, k = k)
  } else if (fold_selection == "random") {
    #--------------------------------
    sblock$sgname <- 0L
    num <- floor(nrow(sblock) / k)

    # foldDF <- data.frame(blocks = seq_len(nrow(sblock)), folds = 0)
    # num <- floor(nrow(sblock) / k)
    # create random folds
    sblock$sgname[seq_len(num * k)] <- sample(rep(seq_len(k), num), num * k)
    #
    if (nrow(sblock) %% k != 0) {
      rest <- nrow(sblock) %% k
      unfold <- which(sblock$sgname == 0)
      sblock$sgname[unfold] <- sample(seq_len(k), rest, replace = FALSE)
    }
  } else {
    # (fold_selection == 'default'){
    k <- NULL
    sblock$sgname <- sblock$gname
    warning("grid selection is selected as 'default', input value of k is ignored")
  }


  #---------------------------------------------------------------------------#

  sblock[, "obs"] <- lengths(sf::st_intersects(sblock, data))

  # remove any grid with no observations
  sblock <- dplyr::filter(sblock, obs > 0L)

  data <- sf::st_intersection(sblock, data)

  #--------- use row info to original grid
  if (show_grid == TRUE) {
    gp2 <- ggplot2::ggplot() +
      ggplot2::geom_sf(
        data = grd, color = "purple",
        fill = "orangered4",
        alpha = 0.04,
        size = 0.7
      ) +
      ggplot2::geom_sf(data = data, ggplot2::aes(color = factor(sgname)), show.legend = FALSE) +
      # ggplot2::scale_colour_viridis_d(option = "plasma")+
      ggplot2::scale_colour_viridis_d(option = "plasma") +
      # ggplot2::geom_sf_text(ggplot2::aes_string(label = "obs"),data = sblock,
      #                       color = "blue")+
      ggplot2::geom_sf_text(ggplot2::aes(label = paste("(", obs, ",", sgname, ")")),
        fun.geometry = sf::st_centroid, data = sblock, color = "blue"
      ) +
      # ggplot2::geom_sf_text(ggplot2::aes_string(label = "sgname"),data = sblock,
      #                       color = "red",position =)+
      ggplot2::labs(x = "", y = "") + # set the axes labs to NULL
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

  ## Save some overall information

  cv_att <- list(v = v, repeats = 1)

  rsample::new_rset(
    splits = split_objs$splits,
    ids = split_objs[, grepl("^id", names(split_objs))],
    attrib = cv_att,
    subclass = c("spatial_clustering_cv", "rset")
  )


  theList <- list(
    plots = gp2,
    blocks = sblock,
    splits = split_objs
  )
  return(theList)
  #
  # theList <- list(plots   = gp2,
  #                 splits = split_objs)
  # #class(theList) <- c("SpatialBlock")
  # theList<- list(plot = gp2)
}


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
#-------------------------------------------------------------- regseq

systematic_sequence <- function(object, k = k) {
  #-------- get nrow of an object
  bl <- nrow(object)
  if (bl %% k == 0) {
    size <- bl / k
    # size of data that is divisible
    bref <- rep(1:k, size)
    # bref (block reference) is the regular sequence of size by k
  } else {
    size <- floor(bl / k)
    bsize <- bl %% k
    bref <- c(rep(1:k, size), 1:bsize)
  }
  return(bref)
}

#-------------------------------------------------------------- regseq end

#### - Miss functions -####

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

