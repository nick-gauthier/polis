#' Trade
#'
#' @param net
#' @param alpha
#' @param beta
#' @param phi
#'
#' @return
#' @export
#'
#' @examples
trade <- function(net, alpha, beta){
  net %E>%
    mutate(trade_utility = .N()$population[to] ^ alpha * exp(-distance / beta))  %N>%
    mutate(trade_balance = centrality_degree(weights = trade_utility, mode = 'out', loops = TRUE),
           trade_production = food) %E>%
    mutate(trade_flow = .N()$trade_production[from] * trade_utility / .N()$trade_balance[from]) %N>%
    mutate(harvest = centrality_degree(weights = trade_flow, mode = 'in', loops = TRUE))
}
