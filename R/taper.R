#' taper
#'
#' @param net
#' @param beta
#' @param tolerance
#'
#' @return
#' @export
#'
#' @examples
taper <- function(net, beta, tolerance = 0.05){

  threshold <- -log(tolerance) * beta
  message(paste0('Removing edges longer than ', round(threshold), 'km'))

  tidygraph::activate(net, 'edges') |>
    dplyr::filter(distance < threshold) # should make distance generic
}

# The larger your domain grows the more potential connections there will be, which can
# increase computational costs significantly. We can fix this by filtering out edges defined
# at a certain threshold.
#If we're using the exponential beta parameterization, we can truncate using the following command,
#where the term in the log is the minimum distance effect we want to capture.
#We'll set it to .005, which if we assume food is 200 gives us 1.
#So at a maximum parameterization of beta = 20,
#truncating the network at this distance serves to eliminate flows that will be less than .5% or 1 food.
# its basically half life decay
# but this only works for expoential and not log?
