#' Title
#'
#' @param net
#' @param mode
#'
#' @return
#' @export
#'
#' @examples
nystuen_dacey <- function(net, mode = 'trade'){

  if (mode == 'trade') {
    net %E>%
      group_by(from) %>%
      filter(trade_flow == max(trade_flow)) %>%
      ungroup() %>%
      filter(., trade_flow < .N()$harvest[to]) %N>%
      mutate(terminal = node_is_sink())
  } else {
    net %E>%
      group_by(from) %>%
      filter(migrant_flow == max(migrant_flow)) %>%
      ungroup() %>%
      filter(., migrant_flow < .N()$immigrants[to]) %N>%
      mutate(terminal = node_is_sink())
  }
}
