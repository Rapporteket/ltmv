#' Lag figur med aldersfordeling
#'
#' @description
#'
#' @return
#'
#' @export
#'
#' @examples
lag_fig_aldersfordeling = function(d) {
  d_aldersfordeling = filter(d, !is.na(alder))

  rapwhale::lag_fig_histogram(d_aldersfordeling,
    x = alder,
    binwidth = 10,
    breaks_width = 20
  ) +
    xlab("Alder") +
    ylab(NULL) +
    ggtitle("Aldersfordeling ved start")
}
