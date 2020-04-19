#' Generate domain colorings of a given function of a complex variable
#'
#' @param f single-variable pre-defined function of complex variable z
#' @param domain the symmetric interval half-radius for both x and y grids
#' @param n number of points to use in grids
#' @param a luminosity
#' @param filename filename to save
#' @param saveJPEG boolean whether to save plot or not
#'
#' @description {A basic function to compute domain colorings of functions of a single complex variable}
#' @details {Uses HSV conversion scheme to color a complex mapping based off its argument and modulus.}
#' @return ggplot plot
#' @export domain_coloring
domain_coloring <- function(f, domain = 1.5, n = 200, a = 0.01, filename = NULL, saveJPEG = FALSE)
{
  # x grid boundaries
  a.x <- -domain
  b.x <- domain
  # y grid boundaries
  a.y <- -domain
  b.y <- domain
  # Create grids
  grid.x <- seq(a.x, b.x, length.out = n)
  grid.y <- seq(a.y, b.y, length.out = n)
  z <- as.vector(outer(grid.x, 1i*grid.y, "+"))
  # HSL colors
  h <- (Arg(f(z)) < 0)*1 + Arg(f(z))/(2*pi)
  l <- (1-a^Mod(z))
  s <-  1
  # # Convert to HSV
  v <- l+s*pmin(l, 1-l)
  # s <- ifelse(v == 0, 0, 2-2*l/v)
  # s <- 1 # We can easily just use 100% saturation too...
  output <- data.frame(x = Re(z), y = Im(z), h = h, v = v, s = s)

  # Options for plot
  opt <- ggplot2::theme(legend.position = "none",
               panel.background = ggplot2::element_blank(),
               panel.grid = ggplot2::element_blank(),
               axis.ticks = ggplot2::element_blank(),
               axis.title = ggplot2::element_blank(),
               axis.text  = ggplot2::element_blank()
  )
  # The plot
  p <- ggplot2::ggplot(data = output, ggplot2::aes_string(x = "x", y = "y")) +
    ggplot2::geom_tile(fill = grDevices::hsv(output$h, output$s, output$v)) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    opt
  print(p)
  if(saveJPEG)
  {
    if(!is.null(filename))
    {
      ggplot2::ggsave(filename, plot = p, device = "jpeg", limitsize = FALSE,
             width = 95, height = 95, units = "in", dpi = 72)
    } else
    {
      stop("Must enter filename to save")
    }

  }
  return(p)

}
