#' Hent skjema
#'
#' @description
#' Hentar all data frå eitt skjema.
#'
#' @param skjemanamn
#' Tekststreng med namn på skjemaet som skal hentast.
#' @param registernamn
#' Tekststreng med namn på register. Standardverdi er "ltmv".
#' Argumentet er med for å kunna køyra testar på testdatabase.
#'
#' @details
#' Fuksjonen brukar [rapbase::loadRegData()]
#' til å henta all data frå skjemaet med namn `skjemanamn`:
#' `SELECT * FROM skjemanamn`
#'
#' I databasen er det brukt store bokstavar i variabelnamna.
#' I utdata frå denne funksjonen er dei gjort om til små bokstavar med
#' [stringr::str_to_lower()].
#'
#' @return
#' Tibble med all data frå skjemaet med namn `skjemanamn`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' hent_skjema("ventreg")
#' }
hent_skjema = function(skjemanamn, registernamn = "data") {
  sporring = paste0("SELECT * FROM ", skjemanamn)
  rapbase::loadRegData(registernamn, sporring) |>
    as_tibble() |>
    rename_with(str_to_lower)
}
