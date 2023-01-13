test_that("grupper_skjemaoversikt() gjev ut forventa resultat", {
  skjemanavn = c(
    "Registrering år 0", "Avslutning", "Oppfølging år 1", "Oppfølging år 3",
    "Oppfølging år 5", "Oppfølging år 35", "Oppfølging ad-hoc"
  )
  skjemarekkeflg = c(0, 999, 1, 3, 5, 35, 500.253)

  d_skjemaoversikt_eksempel = tibble::tibble(
    skjemanavn = rep(!!skjemanavn, each = 2),
    skjemastatus = rep(1:0, length(!!skjemanavn)),
    forlopsid = 1:(2*length(!!skjemanavn)),
    opprettetdato = as.POSIXct("2020-01-01 00:00:00"),
    skjemarekkeflg = rep(!!skjemarekkeflg, each = 2)
  )

  d_skjemaoversikt_gruppert = d_skjemaoversikt_eksempel %>%
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
