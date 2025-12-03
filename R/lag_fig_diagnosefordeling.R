#' Lag figur med diagnosefordeling
#'
#' @description
#' Tek inn ei dataramme som minst inneheld variablane `diag_gruppe` og
#' `diag_gruppe_navn`,
#' og lagar ein figur med oversikt over andel pasientar i dei ulike
#' diagnosegruppene.
#'
#' @param d
#' Dataramme som må innehalda variablane `diag_gruppe` og `diag_gruppe_navn`.
#'
#' @return
#' Stolpediagram med andel pasientar i ulike diagnosegrupper.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' hent_skjema("ventreg") |>
#'   legg_til_overordnet_diag() |>
#'   lag_fig_diagnosefordeling()
#' }
lag_fig_diagnosefordeling = function(d) {
  d_diagnosegrupper = d |>
    filter(!is.na(diag_gruppe_barn_navn)) |>
    count(diag_gruppe_barn, diag_gruppe_barn_navn) |>
    arrange(desc(diag_gruppe_barn)) |>
    mutate(
      diag_gruppe_barn_navn = forcats::fct_inorder(diag_gruppe_barn_navn),
      n_tot = sum(n),
      prop = n / n_tot
    )

  rapwhale::lag_fig_soyle_prosent(d_diagnosegrupper, diag_gruppe_barn_navn, prop) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab("Andel") +
    ggplot2::ggtitle("Diagnosefordeling")
}
