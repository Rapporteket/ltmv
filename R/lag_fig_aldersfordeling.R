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
  soylebreidde = if_else(max(d_aldersfordeling$alder) > 18, 10, 1)

  rapwhale::lag_fig_histogram(d_aldersfordeling,
    x = alder,
    binwidth = soylebreidde,
    breaks_width = soylebreidde * 2
  ) +
    xlab("Alder") +
    ylab(NULL) +
    ggtitle("Aldersfordeling ved start")
}
