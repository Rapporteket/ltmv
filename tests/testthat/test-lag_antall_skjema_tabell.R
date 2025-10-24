# Eksempeldata for skjemaoversikt
# Slik kan data frå hent_skjema("skjemaoversikt") sjå ut
skjemanavn = c(
  "Registrering år 0", "Avslutning", "Oppfølging år 1", "Oppfølging år 3",
  "Oppfølging år 5", "Oppfølging år 35", "Oppfølging ad-hoc"
)
skjemarekkeflg = c(0, 999, 1, 3, 5, 35, 500.253)
skjemadato = seq(as.POSIXct("2020-01-01 00:00:00"), as.POSIXct("2020-01-14 00:00:00"), by = 60 * 60 * 24)

d_skjemaoversikt_eksempel = tibble(
  skjemanavn = rep(!!skjemanavn, each = 2),
  skjemastatus = rep(1:0, length(!!skjemanavn)),
  forlopsid = 1:(2 * length(!!skjemanavn)),
  opprettetav = "test",
  opprettetdato = !!skjemadato,
  sistlagretav = "test",
  sistlagretdato = opprettetdato,
  hoveddato = as.Date(opprettetdato),
  sykehusnavn = rep(c("OSLO UNIVERSITETSSYKEHUS HF", "HELSE BERGEN HF"), each = 7),
  avdresh = rep(c("4001031", "100082"), each = 7),
  hf_gr = rep(c(14, 6), each = 7),
  skjemarekkeflg = rep(!!skjemarekkeflg, each = 2)
)

# Testar for aggreger_antall_skjema_tabell()
d_antall_skjema = aggreger_antall_skjema_tabell(d_skjemaoversikt_eksempel,
  user_role = "SC"
)

d_antall_skjema_fasit = tibble(
  sykehusnavn = c("HELSE BERGEN HF", "OSLO UNIVERSITETSSYKEHUS HF", "Totalt"),
  `Registrering år 0` = c(0L, 1L, 1L),
  `Registrering år 0 (uferdig)` = c(0L, 1L, 1L),
  `Oppfølging år 1` = c(0L, 1L, 1L),
  `Oppfølging år 1 (uferdig)` = c(0L, 1L, 1L),
  `Oppfølging år 3` = c(0L, 1L, 1L),
  `Oppfølging år 3 (uferdig)` = c(1L, 0L, 1L),
  `Videre oppfølging (år 5+ og AdHoc)` = c(3L, 0L, 3L),
  `Videre oppfølging (år 5+ og AdHoc) (uferdig)` = c(3L, 0L, 3L),
  Avslutning = c(0L, 1L, 1L),
  `Avslutning (uferdig)` = c(0L, 1L, 1L),
  Totalt = c(3L, 4L, 7L),
  `Totalt (uferdig)` = c(4L, 3L, 7L)
)

test_that("aggreger_antall_skjema_tabell() gjev ut forventa tal på kolonner", {
  # 11 kolonner: sjukehusnamn + ferdig/uferdig for 5 skjemagrupper
  expect_identical(ncol(d_antall_skjema), 13L)
})

test_that("aggreger_antall_skjema_tabell() gjev ut rette kolonner i rett rekkjefylgje", {
  kolonner_finst = names(d_antall_skjema)
  kolonner_skal_finnast = c(
    "sykehusnavn",
    "Registrering år 0", "Registrering år 0 (uferdig)",
    "Oppfølging år 1", "Oppfølging år 1 (uferdig)",
    "Oppfølging år 3", "Oppfølging år 3 (uferdig)",
    "Videre oppfølging (år 5+ og AdHoc)",
    "Videre oppfølging (år 5+ og AdHoc) (uferdig)",
    "Avslutning", "Avslutning (uferdig)",
    "Totalt", "Totalt (uferdig)"
  )
  expect_identical(kolonner_finst, kolonner_skal_finnast)
})

test_that("aggreger_antall_skjema_tabell() gjev ut forventa resultat", {
  expect_identical(d_antall_skjema, d_antall_skjema_fasit)
})

d_antall_skjema_lokal = aggreger_antall_skjema_tabell(d_skjemaoversikt_eksempel,
  user_role = "LU",
  resh_id = "100082"
)

d_antall_skjema_fasit_lokal = tibble(
  sykehusnavn = "HELSE BERGEN HF",
  `Registrering år 0` = 0L,
  `Registrering år 0 (uferdig)` = 0L,
  `Oppfølging år 1` = 0L,
  `Oppfølging år 1 (uferdig)` = 0L,
  `Oppfølging år 3` = 0L,
  `Oppfølging år 3 (uferdig)` = 1L,
  `Videre oppfølging (år 5+ og AdHoc)` = 3L,
  `Videre oppfølging (år 5+ og AdHoc) (uferdig)` = 3L,
  Avslutning = 0L,
  `Avslutning (uferdig)` = 0L,
  Totalt = 3L,
  `Totalt (uferdig)` = 4L
)

test_that("aggreger_antall_skjema_tabell() gjev ut forventa resultat med user_role = LU", {
  expect_identical(d_antall_skjema_lokal, d_antall_skjema_fasit_lokal)
})

# Testar for formater_antall_skjema_tabell()
test_that("formater_antall_skjema_tabell() gjev ut ein «kable»", {
  v_rhf = c(
    "HELSE SØR-ØST RHF",
    "HELSE VEST RHF",
    "HELSE MIDT-NORGE RHF",
    "HELSE NORD RHF",
    "PRIVAT/IDEELL ORGANISASJON"
  )

  tab_antall_skjema = formater_antall_skjema_tabell(d_antall_skjema, v_rhf)
  expect_s3_class(tab_antall_skjema, "knitr_kable")
})

# Testar for grupper_skjemaoversikt()

test_that("grupper_skjemaoversikt() gjev ut forventa resultat", {
  d_skjemaoversikt_gruppert = d_skjemaoversikt_eksempel |>
    mutate(
      skjema_gruppe_nr = c(
        0, 0.5, 999, 999.5, 1, 1.5, 3, 3.5, 99, 99.5, 99, 99.5, 99, 99.5
      ),
      skjema_gruppe = factor(
        x = c(
          "Registrering år 0", "Registrering år 0 (uferdig)",
          "Avslutning", "Avslutning (uferdig)",
          "Oppfølging år 1", "Oppfølging år 1 (uferdig)",
          "Oppfølging år 3", "Oppfølging år 3 (uferdig)",
          "Videre oppfølging (år 5+ og AdHoc)",
          "Videre oppfølging (år 5+ og AdHoc) (uferdig)",
          "Videre oppfølging (år 5+ og AdHoc)",
          "Videre oppfølging (år 5+ og AdHoc) (uferdig)",
          "Videre oppfølging (år 5+ og AdHoc)",
          "Videre oppfølging (år 5+ og AdHoc) (uferdig)"
        ),
        levels = c(
          "Registrering år 0", "Registrering år 0 (uferdig)",
          "Oppfølging år 1", "Oppfølging år 1 (uferdig)",
          "Oppfølging år 3", "Oppfølging år 3 (uferdig)",
          "Videre oppfølging (år 5+ og AdHoc)",
          "Videre oppfølging (år 5+ og AdHoc) (uferdig)",
          "Avslutning", "Avslutning (uferdig)"
        )
      )
    )

  expect_identical(grupper_skjemaoversikt(d_skjemaoversikt_eksempel),
    expected = d_skjemaoversikt_gruppert
  )
})
