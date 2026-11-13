# Tester for hjelpefunksjoner i ltmv-pakken

######## Lager testdata for kontrollere_patientlist_kolnavn ###################
d_test_patientlist_rapp = tibble(
  id = c(1, 2),
  registered_date = NA,
  birth_date = NA,
  gender = NA,
  deceased = NA,
  deceased_date = NA,
  zipcode = NA,
  town = NA,
  county = NA,
  municipality_name = NA
)

d_test_patientlist_dd = tibble(
  pasientid = c(1, 2),
  dato_reg = NA,
  fodselsdato = NA,
  kjonn = NA,
  avdod = NA,
  dodsdato = NA,
  postnummer = NA,
  poststed = NA,
  kommune = NA,
  fylke = NA
)

d_fasit_patientlist_ut = c(
  "pasientid",
  "RegistreringsDato",
  "Fodselsdato",
  "Kjonn",
  "Avdod",
  "Dodsdato",
  "Postnummer",
  "Poststed",
  "Kommune",
  "Fylke"
)

############### Tester for kontrollere_patientlist_kolnavn ###################

# Tester at funksjonen gir feilmelding dersom et kolonnenavn er feil
test_that("kontrollere_patientlist_kolnavn gir feilmelding ved feil navn", {
  d_patientlist_feil_navn = d_test_patientlist_dd |>
    rename(registerdato = dato_reg)

  expect_error(kontrollere_patientlist_kolnavn(d_patientlist_feil_navn))
})

# Tester at funksjonen gir feilmelding dersom et kolonnenavn er mangler
test_that("kontrollere_patientlist_kolnavn gir feilmelding ved feil navn", {
  d_patientlist_navn_mangler = d_test_patientlist_dd |>
    select(-kjonn)

  expect_error(kontrollere_patientlist_kolnavn(d_patientlist_navn_mangler))
})

# Tester at funksjonen gir ut riktige kolonnenavn dersom kolonnenavn fra
# inndata er riktig
test_that("kontrollere_patientlist_kolnavn gir ut riktig kolonnenavn", {
  expect_identical(
    names(kontrollere_patientlist_kolnavn(d_test_patientlist_rapp)),
    d_fasit_patientlist_ut
  )
  expect_identical(
    names(kontrollere_patientlist_kolnavn(d_test_patientlist_dd)),
    d_fasit_patientlist_ut
  )
})
