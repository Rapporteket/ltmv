#' Lag dashboard-datasett
#'
#' @description
#' Funksjonen les inn skjemaet ventreg, legg til diverse infomasjon
#' til kvar rad, og gjer datasettet klart til bruk i dashboardet.
#'
#' @param fra
#' Dato. Skjema frå og med denne datoen skal inkluderast.
#' @param til
#' Dato. Skjema til og med denne datoen skal inkluderast.
#' @param alderkategori
#' Tekstvektor for alderkategori ved start. Éin eller fleire av verdiane "barn", "voksen" og NA_character.
#' @param alderkategori_naa
#' Tekstvektor for alderkategori nå. Éin eller fleire av verdiane "barn", "voksen" og NA_character.
#' @param kjonn
#' Numerisk vektor. Éin eller fleire av verdiane 1, 2, NA_real_.
#' 1 er mann, 2 er kvinne.
#' @param inkluder_missing
#' Logisk variabel. `TRUE` eller `FALSE` alt etter om skjema med manglande
#' startdato skal inkluderast.
#' @param resh_id
#' Tekstvektor med RESH-ID datasettet eventuelt skal filtrerast på.
#' @param user_role
#' Tekstvektor med brukarrolle som avgjer om datasettet skal filtrerast eller
#' ikkje.
#'
#' @return
#' Skjemaet ventreg med ekstra informasjon per rad,
#' til bruk i dashboardvisinga.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' lag_datasett_dashboard(
#'   fra = "2020-01-01",
#'   til = "2023-01-01",
#'   alderkategori = "barn"
#'   alderkategori_naa = "voksen",
#'   kjonn = c(1, 2, NA_real_),
#'   inkluder_missing = TRUE,
#'   resh_id = 99999,
#'   user_role = "SC"
#' )
#' }
lag_datasett_dashboard = function(fra,
                                  til,
                                  alderkategori,
                                  alderkategori_naa,
                                  kjonn,
                                  inkluder_missing,
                                  resh_id,
                                  user_role) {
  d_dashboard = hent_skjema("ventreg") |>
    legg_til_pasientid(mceid) |>
    legg_til_pasientinfo(patient_id) |>
    legg_til_alder_og_kategori(birth_date, start_date) |>
    legg_til_stoppinfo(mceid) |>
    legg_til_aktiv_behandling() |>
    legg_til_hf_rhf_navn() |>
    legg_til_overordnet_diag() |>
    mutate(
      alder_no = lubridate::time_length(
        x = lubridate::interval(
          start = birth_date,
          end = Sys.Date()
        ),
        unit = "years"
      ),
    ) |>
    filter(
      start_date >= !!fra | (is.na(start_date) & inkluder_missing),
      start_date <= !!til | (is.na(start_date) & inkluder_missing),
      alderkat %in% !!alderkategori &
        alderkat_no %in% !!alderkategori_naa |
        (is.na(alderkat) & "" %in% !!alderkategori),
      gender %in% !!kjonn | (is.na(gender) & "" %in% !!kjonn)
    alderkat_no = case_when(
      alder_no >= 18 ~ "voksen",
      alder_no < 18 ~ "barn",
      TRUE ~ "NA"
    )

  if (user_role != "SC") {
    d_dashboard = filter(d_dashboard, centreid == !!resh_id)
  }

  d_dashboard
}
