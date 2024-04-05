#' Lag figur med diagnosefordeling
#'
#' @description
#'
#'
#' @return
#'
#' @export
#'
#' @examples
lag_fig_diagnosefordeling = function(d) {
  d_diagnosegrupper = d %>%
    filter(!is.na(diag_gruppe_navn)) %>%
    count(diag_gruppe, diag_gruppe_navn) %>%
    arrange(desc(diag_gruppe)) %>%
    mutate(
      diag_gruppe_navn = forcats::fct_inorder(diag_gruppe_navn),
      n_tot = sum(n),
      prop = n / n_tot
    )

  rapwhale::lag_fig_soyle_prosent(d_diagnosegrupper, diag_gruppe_navn, prop) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab("Andel") +
    ggplot2::ggtitle("Diagnosefordeling")
}