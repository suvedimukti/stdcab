#------------------ funs 12 plot multiple variogram ---------------------------#
#' Plotting Multiple Variograms
#'
#' @description
#' This function uses ggplot2 framework to plot semivariogram object (`tibble`)
#' result from \code{\link{fit_multiple_variogram}}.
#'
#' @param object An output from \code{\link{fit_multiple_variogram}} function
#' @param plot_type plot type "bar" or "line". "linePoint" or "pointLine" or
#' "line", or "point" are used to use
#'      point and line format.
#' @param show_range (logical) whether to display range in the plot
#'
#' @return A list with multiple semivariance plot for all variables and a tibble
#' with fitted semivariances for all variables.
#' @export
#'
#' @seealso  \code{\link[automap]{autofitVariogram}}, \code{\link{fit_variogram}},
#' \code{\link{plot_variogram}}
#'
#' @references
#'
#' Pebesma, E.J., 2004. Multivariable geostatistics in S: the gstat package. Computers & Geosciences, 30: 683-691.
#' Benedikt Gräler, Edzer Pebesma and Gerard Heuvelink, 2016. Spatio-Temporal Interpolation using gstat.  The R
#' Journal 8(1), 204-218
#'
#' Hiemstra, P.H., Pebesma, E.J., Twenhofel, C.J.W. and G.B.M. Heuvelink, 2008. Real-time automatic interpolation of ambient gamma dose rates from
#' the Dutch Radioactivity Monitoring Network. Computers & Geosciences.[DOI:](http://dx.doi.org/10.1016/j.cageo.2008.10.011)
#'
#' H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016.
#'
#' @examples
#' ##  Run semivariance on Principal Component Axis1
#' # read data
#' data("landcover")
#'
#' # remove factor variable
#'
#' landcover$class_name <- NULL
#'
#'# fit multiple variograms
#'
#' mvfit  <- fit_variogram(data = landcover, response = "MPC1", coords = NULL)
#'
#' mvplot <- plot_variogram(object = mvfit, length = 99, show_range = TRUE)
#'
#'
#' \dontrun{
#' # load data
#' data("landcover")
#'
#' # remove factor variable
#' landcover$Class_name<- NULL
#'
#' # fit multiple variograms
#' mfit  <- multiple_variogram(data = landcover,coords = NULL, length = 99)
#'
#' mplot <- plot_multiple_variogram (mfit, plot_type = "line", show_range = FALSE)
#'
#' # Display the plot
#'
#' mplot$plot
#'
#' # View tibble
#' mplot$data
#' }
#' @importFrom stats reorder
#'

plot_multiple_variogram <- function(object, plot_type = "line", show_range = TRUE) {
  obj1 <- object %>%
    dplyr::mutate(
      model = dplyr::coalesce(model.x, model.y),
      var = dplyr::coalesce(var.x, var.y)
    ) %>%
    dplyr::select(model, var, dist, line, np, gamma, sill, nugg, range, dir.hor, dir.ver, id)

  obj <- obj1 %>%
    tidyr::pivot_longer(
      cols = c("line", "gamma"),
      names_to = "type",
      values_to = "value",
      values_drop_na = TRUE
    )

  del_var <- c("np", "dir.hor", "dir.ver", "id")

  obj <- obj %>%
    dplyr::select(-dplyr::one_of(del_var))

  pdata <- obj %>%
    dplyr::filter(type == "gamma")

  ldata <- obj %>%
    dplyr::filter(type == "line")

  range_mean <- round(mean(pdata$range, na.rm = TRUE), 3)
  range_med <- round(median(pdata$range, na.rm = TRUE), 3)

  if (plot_type %in% c("linePoint", "pointLine", "line", "point", NULL)) {
    semivar <- ggplot2::ggplot(data = pdata,ggplot2::aes(x = dist, y = value,
                                                         color = factor(var))) +
      ggplot2::geom_point(show.legend = FALSE) +
      ggplot2::geom_line(data = ldata, ggplot2::aes(color = factor(var)),
                         size = 1, show.legend = FALSE) +
      ggplot2::scale_y_continuous(limits = c(min(pdata$value, ldata$value),
                                    max(pdata$value, ldata$value))) +
      ggplot2::scale_x_continuous(
        limits = c(0, max(pdata$dist)),
        breaks = seq(from = 0, to = max(pdata$dist), length.out = 5)
      ) +
      ggplot2::xlab("Distance (m)") +
      ggplot2::ylab("Semivariance") +
     ggplot2::theme_bw(12)
  } else if (plot_type == "bar") {
    pdata <- pdata %>%
      dplyr::arrange(dplyr::desc(range)) %>%
      dplyr::group_by(var) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(var = factor(var))

    semivar <- pdata %>%
      ggplot2::ggplot(ggplot2::aes(x = stats::reorder(factor(var), range),
                                   y = range, fill = var)) +
      ggplot2::stat_summary(geom = "bar", fun = "mean", position = "dodge",
                   width = 0.99, show.legend = FALSE) +
      ggplot2::scale_fill_viridis_d() +
      ggplot2::ylab("Distance (m)") +
      ggplot2::xlab("Predictor Variables") +
      ggplot2::theme_classic(12) +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "grey99",
                                                 colour = "black", size = .25),
        panel.border = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(colour = "white", size = NA),
        panel.grid.minor = ggplot2::element_line(colour = "white", size = 0.25),
        strip.background = ggplot2::element_rect(fill = "grey95", colour = NA)
      )
  } else {
    semivar <- ggplot2::ggplot(data = pdata, ggplot2::aes(x = dist, y = value,
                                                      color = factor(var))) +
      ggplot2::geom_point(show.legend = FALSE) +
      ggplot2::geom_line(data = ldata, ggplot2::aes(color = factor(var)),
                         lty = 1, show.legend = FALSE) +
      ggplot2::scale_y_continuous(limits = c(min(pdata$value, ldata$value),
                                    max(pdata$value, ldata$value))) +
      ggplot2::scale_x_continuous(
        limits = c(0, max(pdata$dist)),
        breaks = seq(from = 0, to = max(pdata$dist), length.out = 5)
      ) +
      ggplot2::xlab("Distance (m)") +
      ggplot2:: ylab("Semivariance)") +
      ggplot2::theme_bw(12)
  }

  if (show_range) {
    if (plot_type != "bar") {
      semivar <- semivar +
        ggplot2::geom_vline(xintercept = range_med, linetype = "dashed",
                            show.legend = FALSE) +
        ggplot2::geom_text(aes(range_med, range_med,
                      label = paste0("Median = ", range_med), hjust = -0.5),
                      show.legend = FALSE)
    } else {
      semivar <- semivar +
        ggplot2::geom_hline(ggplot2::aes(yintercept = range_mean),
                            linetype = "dashed", color = "blue", size = 1.5) +
        ggplot2::geom_text(ggplot2::aes(1.5, range_mean, label = paste0("Mean = ", range_mean), vjust = -1)) +
        ggplot2::geom_hline(yintercept = range_med, linetype = "dashed", color = "purple", size = 1.5) +
        ggplot2::geom_text(ggplot2::aes(1.5, range_med, label = paste0("Median = ", range_med), vjust = -1))
    }
  } else {
    semivar
  }

  ret_list <- list(
    plot = semivar,
    data = obj
  )
  return(ret_list)
}
