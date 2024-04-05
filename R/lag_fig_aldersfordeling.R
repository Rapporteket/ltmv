#' Lag figur med aldersfordeling
#'
#' @description
#' Tek inn ei dataramme som minst inneheld variabelen `alder`,
#' og lagar ein figur med aldersfordeling.
#'
#' @param d
#' Dataramme som må innehalda variabelen `alder`.
#'
#' @return
#' Histogram med aldersfordeling.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' hent_skjema("ventreg") |>
#'   legg_til_pasientid(mceid) |>
#'   legg_til_pasientinfo(patient_id) |>
#'   legg_til_alder_og_kategori(birth_date, start_date) |>
#'   lag_fig_aldersfordeling()
#' }
lag_fig_aldersfordeling = function(d) {
  d_aldersfordeling = filter(d, !is.na(alder))
  soylebreidde = if_else(max(d_aldersfordeling$alder) > 18, 10, 1)

  rapwhale::lag_fig_histogram(d_aldersfordeling,
    x = alder,
    binwidth = soylebreidde,
    breaks_width = soylebreidde * 2
  ) +
    ggplot2::xlab("Alder") +
    ggplot2::ylab(NULL) +
    ggplot2::ggtitle("Aldersfordeling ved start")
}
