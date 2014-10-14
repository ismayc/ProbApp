library("ggplot2")

#' Draw Normal Distribution Density with an area shaded in.
#'
#' @param lb Lower bound of the shaded area. Use \code{-Inf} for a left tail.
#' @param ub Upper bound of the shaded area. Use \code{Inf} for a right tail.
#' @param mean Mean of the normal distribution
#' @param sd Standard deviation of the normal distribution
#' @param limits Lower and upper bounds on the x-axis of the area displayed.
#' @return ggplot object.
#' @examples
#' # Standard normal with upper 2.5% tail shaded
#' normal_prob_area_plot(2, Inf)
#' # Standard normal with lower 2.5% tail shaded
#' normal_prob_area_plot(-Inf, 2)
#' # standard normal with middle 68% shaded.
#' normal_prob_area_plot(-1, 1)
normal_prob_area_plot <- function(lb, ub, mean = 0, sd = 1, limits = c(mean - 3 * sd, mean + 3 * sd)) {
  x <- seq(limits[1], limits[2], length.out = 100)
  xmin <- max(lb, limits[1])
  xmax <- min(ub, limits[2])
  areax <- seq(xmin, xmax, length.out = 100)
  area <- data.frame(x = areax, ymin = 0, ymax = dnorm(areax, mean = mean, sd = sd))
  (ggplot()
   + geom_line(data.frame(x = x, y = dnorm(x, mean = mean, sd = sd)),
               mapping = aes(x = x, y = y))
   + geom_ribbon(data = area, mapping = aes(x = x, ymin = ymin, ymax = ymax))
   + scale_x_continuous(limits = limits))
}