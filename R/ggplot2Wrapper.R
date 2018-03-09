#' Wrapper function for ggplot for plotting Date against home value, structure cost, or land value depending on the margin
#'@param x <- vector
#'

plotMyData<-function(x)
{
  x%>% ggplot2::ggplot()+ggplot2::aes(x=x, y=p)+ggplot2::geom_point()
}
