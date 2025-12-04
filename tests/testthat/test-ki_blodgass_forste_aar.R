# Tester at ki_blodgass_forste_aar() beregner `ki_krit_teller` og
# `ki_krit_nevner` riktig. Tester linje for linje i beregning av
# nevner, og videre beregning av teller.

# Testdata
d_superbredt_test = tibble(
  r_start_date = as.Date(c("2020-01-01", "2001-06-15", "2023-06-01", "2022-01-01", "2020-01-01", "2020-01-01", "2020-01-01", "2020-01-01", "2020-01-01", "2020-01-01")),
  r_ventilation_method = c(3, 1, 2, 2, 2, 2, 2, 2, 2, 2),
  f1_pco2_air = c(5.2, 5.2, NA, NA, NA, NA, NA, NA, NA, NA),
  f1_po2_air = c(NA, NA, 8.1, 8.1, 8.1, NA, NA, NA, 8.1, NA),
  f1_capillarypo2_air = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  f1_capillarypco2_air = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  f1_be = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  f1_arterialpco2_air = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  f1_transcutaneous_co2_air = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  fah_followup_date = as.Date(c("2021-01-01", "2002-06-15", "2024-06-01", "2023-01-01", "2021-01-01", "2023-01-01", "2020-01-01", "2021-01-01", "2021-01-01", "2021-01-01")),
  fah_pco2_air = c(6.1, 6.1, NA, NA, NA, 6.1, 6.1, NA, NA, 6.1),
  fah_po2_air = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  fah_capillarypo2_air = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  fah_capillarypco2_air = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  fah_be = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  fah_arterialpco2_air = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  fah_transcutaneous_co2_air = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  c_stop_date = as.Date(c(NA, NA, NA, "2022-06-01", "2023-02-01", NA, NA, NA, NA, NA)),
  c_deceased_date = as.Date(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
)

# Fasit for beregning av teller og nevner for
# d_superbredt_test, dato_data = as.Date("2024-12-31"))
d_ki_fasit = tibble(
  r_start_date = as.Date(c("2020-01-01", "2001-06-15", "2023-06-01", "2022-01-01", "2020-01-01", "2020-01-01", "2020-01-01", "2020-01-01", "2020-01-01", "2020-01-01")),
  r_ventilation_method = c(3, 1, 2, 2, 2, 2, 2, 2, 2, 2),
  f1_pco2_air = c(5.2, 5.2, NA, NA, NA, NA, NA, NA, NA, NA),
  f1_po2_air = c(NA, NA, 8.1, 8.1, 8.1, NA, NA, NA, 8.1, NA),
  f1_capillarypo2_air = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  f1_capillarypco2_air = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  f1_be = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  f1_arterialpco2_air = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  f1_transcutaneous_co2_air = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  fah_followup_date = as.Date(c("2021-01-01", "2002-06-15", "2024-06-01", "2023-01-01", "2021-01-01", "2023-01-01", "2020-01-01", "2021-01-01", "2021-01-01", "2021-01-01")),
  fah_pco2_air = c(6.1, 6.1, NA, NA, NA, 6.1, 6.1, NA, NA, 6.1),
  fah_po2_air = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  fah_capillarypo2_air = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  fah_capillarypco2_air = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  fah_be = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  fah_arterialpco2_air = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  fah_transcutaneous_co2_air = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  c_stop_date = as.Date(c(NA, NA, NA, "2022-06-01", "2023-02-01", NA, NA, NA, NA, NA)),
  c_deceased_date = as.Date(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)),
  ki_krit_nevner = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE),
  ki_krit_teller = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE)
)

test_that("ki_blodgass_forste_aar() beregner teller og nevner riktig", {
  expect_identical(ki_blodgass_forste_aar(d_superbredt_test, dato_data = as.Date("2024-12-31")), d_ki_fasit)
})
