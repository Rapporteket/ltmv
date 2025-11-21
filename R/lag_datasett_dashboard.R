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
#' @param enhetstype
#' Tekstvektor med enhetstypene ("RHF", "HF", eller "Sykehus") data kan filtreres
#' ut mot. Hentes fra `shiny::radioButtons()` med `id = enhet_type` i app_server.R.
#' @param per_rhf
#' Teksvektor med de ulike RHF-ene data kan filtreres ut mot.  Hentes fra
#' `shiny::checkboxGroupInput()` med `id = rhf_utvalg_dashboard` i app_server.R.
#' @param per_hf
#' Teksvektor med de ulike HF-ene data kan filtreres ut mot.  Hentes fra
#' `shiny::selectInput()` med `id = hf_utvalg_dashboard` i app_server.R.
#' @param per_sykehus
#' Teksvektor med de ulike sykehusene data kan filtreres ut mot.  Hentes fra
#' `shiny::selectInput()` med `id = sykehus_utvalg_dashboard` i app_server.R.
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
#'   alderkategori = "barn",
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
                                  user_role,
                                  enhetstype,
                                  per_rhf,
                                  per_hf,
                                  per_sykehus) {
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
      alderkat_no = case_when(
        alder_no >= 18 ~ "voksen",
        alder_no < 18 ~ "barn",
        TRUE ~ "NA"
      )
    ) |>
    filter(
      start_date >= !!fra | (is.na(start_date) & inkluder_missing),
      start_date <= !!til | (is.na(start_date) & inkluder_missing),
      alderkat %in% !!alderkategori |
        (is.na(alderkat) & "" %in% !!alderkategori),
      alderkat_no %in% !!alderkategori_naa |
        (is.na(alderkat_no) & "" %in% !!alderkategori_naa),
      gender %in% !!kjonn | (is.na(gender) & "" %in% !!kjonn)
    )

  if (user_role != "SC") {
    d_dashboard = filter(d_dashboard, centreid == !!resh_id)
  }

  d_centretype = hent_skjema("centretype") |>
    select(id, name)

  d_centre = hent_skjema("centre") |>
    select(id, typeid) |>
    mutate(id = as.integer(id))

  d_belongsto_fylt = hent_skjema("centre") |>
    select(id, centrename, belongsto) |>
    mutate(belongsto = if_else(is.na(belongsto), id, belongsto))

  d_belongsto_hf = d_belongsto_fylt |>
    select(-belongsto) |>
    rename(hf = centrename)

  d_centre_hf = d_belongsto_fylt |>
    left_join(
      d_belongsto_hf,
      by = join_by(belongsto == id),
      relationship = "many-to-one"
    )


  d_id_sykehus_hf_rhf = d_centre_hf |>
    mutate(id = as.numeric(id)) |>
    left_join(d_centre,
              by = join_by(id == id)
    ) |>
    left_join(d_centretype,
              by = join_by(typeid == id)
    ) |>
    rename(rhf = name, sykehusnavn = centrename) |>
    select(id, sykehusnavn, hf, rhf) |>
    distinct()
  d_dashboard
}
