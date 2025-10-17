## Se navn til alle tabellene i ltmv database
# rapbase::describeRegistryDb("data") |> names()
# [1] "centre"             "centreattribute"    "centretype"         "chainview"          "conclude"           "followups"
# [7] "followupsnum"       "friendly_vars"      "fylke"              "kommune"            "lastfollowup"       "listboxtextrow"
# [13] "mce"                "patient"            "patientlist"        "postnr"             "postnr_kat"         "reg_questionnaires"
# [19] "skjemaoversikt"     "text"               "user"               "user_centre_list"   "usergroup"          "varattribute"
# [25] "varcontainer"       "varinterval"        "varservice"         "varvar"             "ventfol"            "ventreg"


# Henter inn data på fylkeskoder og helseregion tilhørighet.
fylke_rhf_adresse = system.file("extdata", "fylke-rhf.xlsx", package = "ltmv")
d_fylke_til_rhf = readxl::read_excel(fylke_rhf_adresse)

d_centretype = hent_skjema("centretype") %>% select (id,name)
d_ventreg_utvalg = hent_skjema("ventreg") %>% select(centreid, move_to_rhf) %>% distinct()
d_centreid_rhf = d_ventreg_utvalg %>%
  left_join(d_centretype, by = c("move_to_rhf" = "id"), relationship = "many-to-one") %>%
  select(-move_to_rhf)

d_skjemaoversikt = hent_skjema("skjemaoversikt") |>
  mutate(skjema_id = as.integer(forlopsid)) |>
  legg_til_pasientid(skjema_id) |>
  legg_til_pasientinfo(patient_id) |>
  legg_til_alder_og_kategori(birth_date, hoveddato) |>
  legg_til_stoppinfo(skjema_id) |>
  legg_til_aktiv_behandling() |>
  mutate(move_to_centre = avdresh) |>
  legg_til_hf_rhf_navn() %>%
  left_join(d_centreid_rhf, by = c("avdresh" = "centreid"), relationship = "many-to-one") %>%
  rename(rhf = name) %>%
  relocate(rhf, .after = avdresh)




d_test = d_skjemaoversikt %>% left_join(d_ventreg_utvalg,
                               by = c("avdresh" = "centreid"),
                               relationship = "many-to-many")


  # filter(
  #   lubridate::date(opprettetdato) >= !!"2014-01-01",
  #   lubridate::date(opprettetdato) <= !!"2024-01-01",
  #   alderkat %in% !!c("barn", "voksen") | (is.na(c("barn", "voksen")) & "" %in% !!c("barn", "voksen")),
  #   aktiv_behandling %in% !!aktiv_behandling
  # )

  d_antall_skjema = aggreger_antall_skjema_tabell(d_skjemaoversikt,
                                                  user_role = user_role,
                                                  resh_id = resh_id
  )

  if (nrow(d_antall_skjema) != 0) {
    formater_antall_skjema_tabell(d_antall_skjema)
  } else {
    htmltools::HTML("Ingen skjema for valgt datointervall.")
  }



lag_antall_skjema_tabell(
  fra = "2014-01-01",
  til = "2024-01-01",
  alderkat = c("barn", "voksen"),
  aktiv_behandling = c(TRUE, FALSE),
  resh_id = user$unit(),
  user_role = "SC"
)

