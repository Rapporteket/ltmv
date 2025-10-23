#' Lag HTML-tabell med oversikt over ferdige og uferdige skjema på sjukehusnivå
#'
#' @description
#' Funksjonen tek inn datoar `fra` og `til`,
#' tekstvektorar `alderkategori`, `resh_id` og `user_role`,
#' og ein logisk vektor `aktiv_behandling`,
#' og gjev ut ein HTML-tabell med talet på ferdige og uferdige skjema på
#' sjukehusnivå for registrerings-, avslutnings- og ulike oppfylgjingsskjema.
#'
#' @param fra
#' Dato, eller eventuelt tekst på formatet "yyyy-mm-dd".
#' Skjema oppretta frå og med denne datoen vert inkludert.
#' @param til
#' Dato, eller eventuelt tekst på formatet "yyyy-mm-dd".
#' Skjema oppretta til og med denne datoen vert inkludert.
#' @param alderkategori
#' Tekstvektor med éin eller fleire av verdiane "barn", "voksen" og "".
#' @param aktiv_behandling
#' Logisk vektor med éin eller begge verdiane `TRUE` og `FALSE`.
#' @param resh_id
#' Tekststreng med RESH-ID til eininga til innlogga brukar.
#' @param user_role
#' Tekststreng med rolla til innlogga brukar.
#' Til dømes "SC" (system coordinator) eller "LU" (local user).
#'
#' @details
#' Funksjonen tek inn datoar `fra` og `til`,
#' tekstvektorar `alderkategori`, `resh_id` og `user_role`,
#' og ein logisk vektor `aktiv_behandling`,
#' og gjev ut ein HTML-tabell med talet på ferdige og uferdige skjema på
#' sjukehusnivå for registrerings-, avslutnings- og ulike oppfylgjingsskjema.
#'
#' Viss `user_role` ikkje er lik "SC" (system coordinator),
#' vert berre data frå eininga med RESH-ID `resh_id` vist.
#'
#' @return
#' HTML-tabell med talet på ferdige og uferdige skjema på sjukehusnivå
#' for registrerings-, avslutnings- og ulike oppfylgjingsskjema.
#' @export
#'
#' @examples
#' \dontrun{
#' lag_antall_skjema_tabell(
#'   fra = Sys.Date() - 365,
#'   til = Sys.Date(),
#'   alderkategori = "voksen",
#'   aktiv_behandling = TRUE,
#'   resh_id = 99999,
#'   user_role = "SC"
#' )
#' }
lag_antall_skjema_tabell = function(fra, til, alderkategori, aktiv_behandling, resh_id, user_role) {
  d_skjemaoversikt = hent_skjema("skjemaoversikt") |>
    mutate(skjema_id = as.integer(forlopsid)) |>
    legg_til_pasientid(skjema_id) |>
    legg_til_pasientinfo(patient_id) |>
    legg_til_alder_og_kategori(birth_date, hoveddato) |>
    legg_til_stoppinfo(skjema_id) |>
    legg_til_aktiv_behandling() |>
    mutate(move_to_centre = avdresh) |>
    legg_til_hf_rhf_navn() |>
    filter(
      lubridate::date(opprettetdato) >= !!fra,
      lubridate::date(opprettetdato) <= !!til,
      alderkat %in% !!alderkategori | (is.na(alderkat) & "" %in% !!alderkategori),
      aktiv_behandling %in% !!aktiv_behandling
    )

  d_centretype <<- hent_skjema("centretype") |>
    select(id, name)

  d_centre = hent_skjema("centre") |>
    select(id, typeid) |>
    mutate(id = as.integer(id))


  d_sykehus_rhf = hent_skjema("skjemaoversikt") |>
    mutate(avdresh = as.integer(avdresh)) |>
    left_join(d_centre,
      by = c("avdresh" = "id")
    ) |>
    left_join(d_centretype,
      by = c("typeid" = "id"),
      relationship = "many-to-one"
    ) |>
    rename(rhf = name) |>
    select(sykehusnavn, rhf) |>
    distinct()

  d_aggregert = aggreger_antall_skjema_tabell(d_skjemaoversikt,
    user_role = user_role,
    resh_id = resh_id
  ) |>
    left_join(d_sykehus_rhf,
      by = "sykehusnavn"
    )

  d_totalt = d_aggregert |>
    filter(sykehusnavn == "Totalt") |>
    select(-rhf)

  d_hmn = d_aggregert |>
    filter(rhf == (d_centretype |> filter(id == 3) |> dplyr::pull(name))) |>
    janitor::adorn_totals(where = "row", name = (d_centretype |> filter(id == 3) |> dplyr::pull(name))) |>
    mutate(prioritet = if_else(sykehusnavn == (d_centretype |> filter(id == 3) |> dplyr::pull(name)), 0, 1)) |>
    arrange(prioritet, sykehusnavn) |>
    select(-prioritet)


  d_hn = d_aggregert |>
    filter(rhf == (d_centretype |> filter(id == 4) |> dplyr::pull(name))) |>
    janitor::adorn_totals(where = "row", name = (d_centretype |> filter(id == 4) |> dplyr::pull(name))) |>
    mutate(prioritet = if_else(sykehusnavn == (d_centretype |> filter(id == 4) |> dplyr::pull(name)), 0, 1)) |>
    arrange(prioritet, sykehusnavn) |>
    select(-prioritet)

  d_hso = d_aggregert |>
    filter(rhf == (d_centretype |> filter(id == 1) |> dplyr::pull(name))) |>
    janitor::adorn_totals(where = "row", name = (d_centretype |> filter(id == 1) |> dplyr::pull(name))) |>
    mutate(prioritet = if_else(sykehusnavn == (d_centretype |> filter(id == 1) |> dplyr::pull(name)), 0, 1)) |>
    arrange(prioritet, sykehusnavn) |>
    select(-prioritet)

  d_hv = d_aggregert |>
    filter(rhf == (d_centretype |> filter(id == 2) |> dplyr::pull(name))) |>
    janitor::adorn_totals(where = "row", name = (d_centretype |> filter(id == 2) |> dplyr::pull(name))) |>
    mutate(prioritet = if_else(sykehusnavn == (d_centretype |> filter(id == 2) |> dplyr::pull(name)), 0, 1)) |>
    arrange(prioritet, sykehusnavn) |>
    select(-prioritet)

  d_privat = d_aggregert |>
    filter(rhf == (d_centretype |> filter(id == 7) |> dplyr::pull(name))) |>
    janitor::adorn_totals(where = "row", name = (d_centretype |> filter(id == 7) |> dplyr::pull(name))) |>
    mutate(prioritet = if_else(sykehusnavn == (d_centretype |> filter(id == 7) |> dplyr::pull(name)), 0, 1)) |>
    arrange(prioritet, sykehusnavn) |>
    select(-prioritet)

  d_antall_skjema = d_hmn |>
    bind_rows(d_hn) |>
    bind_rows(d_hso) |>
    bind_rows(d_hv) |>
    bind_rows(d_privat) |>
    select(-rhf) |>
    bind_rows(d_totalt)

  if (user_role != "SC") {
    d_antall_skjema = d_antall_skjema |>
      filter(!sykehusnavn %in% (d_centretype |> dplyr::pull(name)))
  }

  if (nrow(d_antall_skjema) != 0) {
    formater_antall_skjema_tabell(d_antall_skjema)
  } else {
    htmltools::HTML("Ingen skjema for valgt datointervall.")
  }
}

#' Lag dataramme med oversikt over ferdige og uferdige skjema på sjukehusnivå
#'
#' @description
#' Funksjonen tek inn ei dataramme `d_skjemaoversikt`,
#' og gjev ut ei dataramme med talet på ferdige og uferdige skjema på
#' sjukehusnivå for registrerings-, avslutnings- og ulike oppfylgjingsskjema.
#'
#' @param d_skjemaoversikt
#' Skjemaet «skjemaoversikt» frå LTMV-databasen, som ei dataramme.
#' Eventuelt filtrert på t.d. sjukehus.
#' @param user_role
#' Tekststreng med rolla til innlogga brukar.
#' Til dømes "SC" (system coordinator) eller "LU" (local user).
#' @param resh_id
#' Tekststreng med RESH-ID til eininga til innlogga brukar.
#'
#' @return
#' Dataramme med talet på ferdige og uferdige skjema på sjukehusnivå
#' for registrerings-, avslutnings- og ulike oppfylgjingsskjema.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' d_skjemaoversikt = hent_skjema("skjemaoversikt")
#' ltmv:::aggreger_antall_skjema_tabell(d_skjemaoversikt)
#' }
aggreger_antall_skjema_tabell = function(d_skjemaoversikt, user_role, resh_id) {
  if (user_role != "SC") {
    d_skjemaoversikt = filter(d_skjemaoversikt, avdresh == !!resh_id)
  }

  d_antall_skjema = d_skjemaoversikt |>
    grupper_skjemaoversikt() |>
    count(hf_gr, skjema_gruppe, sykehusnavn) |>
    select(-hf_gr) |>
    pivot_wider(
      names_from = skjema_gruppe,
      values_from = n,
      values_fill = 0,
      names_expand = TRUE
    ) |>
    janitor::adorn_totals(name = "Totalt") |>
    as_tibble() |>
    rowwise() |>
    mutate(
      Totalt = sum(c_across(!sykehusnavn & !contains("(uferdig)"))),
      `Totalt (uferdig)` = sum(c_across(!sykehusnavn & contains("(uferdig)")))
    ) |>
    ungroup()

  # Fjern attributt lagt til av janitor::adorn_totals()
  attr(d_antall_skjema, "totals") = NULL
  attr(d_antall_skjema, "tabyl_type") = NULL
  attr(d_antall_skjema, "core") = NULL

  # Fjern totalrad viss ikkje brukarrolle er systemkoordinator
  if (user_role != "SC") {
    d_antall_skjema = filter(d_antall_skjema, sykehusnavn != "Totalt")
  }

  d_antall_skjema
}

#' Formater antall skjema-tabell
#'
#' @description
#' Tek inn ei dataramme med tal på ferdige og uferdige skjema,
#' frå [aggreger_antall_skjema_tabell()],
#' og gjev den ut som HTML-tabell klar til å visast i Rapporteket.
#'
#' @param d_antall_skjema
#' Dataramme frå [aggreger_antall_skjema_tabell()].
#'
#' @return
#' `d_antall_skjema` formatert som HTML-tabell,
#' med samlande overskrifter, hover-effekt og så vidare.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' d_skjemaoversikt = hent_skjema("skjemaoversikt")
#' d_antall_skjema = ltmv:::aggreger_antall_skjema_tabell(d_skjemaoversikt)
#' ltmv:::formater_antall_skjema_tabell(d_antall_skjema)
#' }
formater_antall_skjema_tabell = function(d_antall_skjema) {


  d_antall_skjema |>
    knitr::kable("html", col.names = NULL, format.args = list(big.mark = " ")) |>
    kableExtra::add_header_above(
      header = c("", rep(c("Ferdig", "Uferdig"), 6)),
      color = "grey",
      font_size = 12,
      align = "r"
    ) |>
    kableExtra::add_header_above(
      header = c(
        "RHF/Sykehus",
        `Registrering år 0` = 2,
        `Oppfølging år 1` = 2, `Oppfølging år 3` = 2,
        `Videre oppfølging\n(år 5+ og AdHoc)` = 2, Avslutning = 2,
        Totalt = 2
      )
    ) |>
    kableExtra::column_spec(seq(3, 13, 2), color = "red") |>
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover")) |>
    kableExtra::row_spec(c(nrow(d_antall_skjema), which(d_antall_skjema$sykehusnavn %in% (d_centretype |> dplyr::pull(name)))),
      bold = TRUE,
      background = "#D5E0E9"
    ) |>
    kableExtra::column_spec(12:13, bold = TRUE)
}

#' Grupper skjemaoversikt
#'
#' @description
#' Grupperer skjemaoversikt i kategoriar alt etter om skjemaa er
#' ferdige eller uferdige,
#' og slår saman ad hoc oppfylgjing og oppfylgjingskjema frå og med år 5
#' til kategorien «Videre oppfølging».
#'
#' @param d_skjemaoversikt
#' Skjemaet «skjemaoversikt» frå LTMV-databasen, som ei dataramme.
#'
#' @details
#' Tak inn skjemaet «skjemaoversikt» frå LTMV-databasen,
#' som kan hentast med `hent_skjema("skjemaoversikt")`,
#' og legg til to nye kolonner, `skjema_gruppe_nr` og `skjema_gruppe`.
#'
#' `skjema_gruppe` er lik `skjemanavn` for alle skjema utanom oppfylgjing
#' frå og med år 5, og ad hoc oppfylgjing.
#' Desse vert slått saman og får verdien `Videre oppfølging (år 5+ og AdHoc)`
#' i den nye kolonnen `skjema_gruppe`.
#' I tillegg vil det for uferdige skjema leggjast på "(uferdig)" på slutten
#' av verdiane i `skjema_gruppe` for uferdige skjema.
#'
#' `skjema_gruppe_nr` tek utgangspunkt i kolonnen `skjemarekkeflg`,
#' som er eit unikt tal for kvar skjematype,
#' der verdien svarar til oppfylgjingsår.
#' Ad hoc oppfylgjing har i `skjemarekkeflg` verdiar på 500 og noko,
#' og avslutningsskjema verdien 999.
#' `skjema_gruppe_nr` nyttar same verdiane som `skjemarekkeflg`,
#' utanom oppfylgjing frå og med år 5 og ad hoc oppfylgjing,
#' som no får verdien 99.
#' I tillegg skiljer `skjema_gruppe_nr` på ferdige og uferdige skjema,
#' i motsetnad til `skjemarekkeflg`,
#' ved å leggja til 0.5 til verdien.
#' I `skjema_gruppe_nr` tek altså ferdige skjema heiltalsverdiar,
#' medan uferdige skjema har tilsvarande verdiar,
#' men med .5 lagt til.
#'
#' Dette er ein intern funksjon som i hovudsak nyttast til å laga
#' tabelloversikt over ferdige og uferdige skjema.
#'
#' @return
#' Dataramma `d_skjemaoversikt` med to ekstra kolonner,
#' skjema_gruppe_nr og skjema_gruppe.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' d_skjemaoversikt_gruppert = hent_skjema("skjemaoversikt") |>
#'   ltmv:::grupper_skjemaoversikt()
#'
#' d_skjemaoversikt_gruppert |>
#'   head(20) |>
#'   select(skjemanavn, skjemastatus, skjema_gruppe_nr, skjema_gruppe)
#' }
grupper_skjemaoversikt = function(d_skjemaoversikt) {
  d_skjema_grupper = tibble(
    skjema_gruppe_nr = c(0, 0.5, 1, 1.5, 3, 3.5, 99, 99.5, 999, 999.5),
    skjema_gruppe = fct_inorder(c(
      "Registrering år 0", "Registrering år 0 (uferdig)",
      "Oppfølging år 1", "Oppfølging år 1 (uferdig)",
      "Oppfølging år 3", "Oppfølging år 3 (uferdig)",
      "Videre oppfølging (år 5+ og AdHoc)",
      "Videre oppfølging (år 5+ og AdHoc) (uferdig)",
      "Avslutning", "Avslutning (uferdig)"
    ))
  )

  d_skjemaoversikt |>
    mutate(
      # Sett skjema_gruppe_nr til 99 for "Videre oppfølging (år 5+ og AdHoc)"
      skjema_gruppe_nr = if_else(skjemarekkeflg %in% c(0, 1, 3, 999),
        true = skjemarekkeflg,
        false = 99
      ),
      # Legg til 0.5 til skjema_gruppe_nr for uferdige skjema
      skjema_gruppe_nr = if_else(skjemastatus != 1,
        true = skjema_gruppe_nr + 0.5,
        false = skjema_gruppe_nr
      )
    ) |>
    left_join(d_skjema_grupper, by = "skjema_gruppe_nr")
}
