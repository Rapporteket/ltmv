# Tester for superbreitt_format()

################## Lager testdata til å teste funksjonen ######################

# Pasientliste (med 33 kolonner for å komme gjennom
# "if (length(names(d_full_patientlist)) == 33)")
d_test_patientlist = tibble(
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
# Legger til 32 kolonner for å bli totalt 33 kolonner
for (i in 1:23) {
  d_test_patientlist[[paste0("kol_", i)]] = NA
}
# MCE
d_test_mce = tibble(
  mceid = c(11, 12, 13, 14, 15, 16, 17),
  patient_id = c(1, 2, 1, 2, 1, 2, 2),
  parent_mce = c(11, 11, 11, 12, 11, 12, 12)
)
# Ventreg
d_test_ventreg = tibble(
  mceid = c(11, 12)
)
# Ventfol
d_test_ventfol = tibble(
  mceid = c(13, 14, 15, 16),
  year = c(1, 1, 3, -1),
  followup_date = as.Date(c(
    "2020-01-01", "2020-01-05", "2022-01-01", "2024-06-01"
  ))
)
# Conclude
d_test_conclude = tibble(
  mceid = 17
)
# Slår sammen datasettene med funksjonen superbreitt_format()
d_test_superbredt = superbreitt_format(
  d_full_patientlist = d_test_patientlist,
  d_full_mce = d_test_mce,
  d_full_ventreg = d_test_ventreg,
  d_full_ventfol = d_test_ventfol,
  d_full_conclude = d_test_conclude
)
###############################################################################
################# Tester for data på superbredt format ########################
###############################################################################

# Antall rader skal matche antall registreringer i ventreg
test_that("superbreitt_format har riktig antall rader", {
  expect_equal(nrow(d_test_superbredt), nrow(d_test_ventreg))
})

test_that("superbreitt_format inneholder prefiksene fra alle skjemaene", {
  forventede_prefiks = c("p_", "r_", "f1_", "f3_", "fah_", "lf_", "c_")

  expect_true(
    all(vapply(
      forventede_prefiks,
      \(prefix) any(startsWith(names(d_test_superbredt), prefix)),
      logical(1)
    ))
  )
})

test_that("superbreitt_format gir warning dersom d_full_patientlist mangler
          kolonnene som skal bytte navn", {
  expect_error(superbreitt_format(
    d_full_patientlist = d_test_patientlist |> select(-"town"),
    d_full_mce = d_test_mce,
    d_full_ventreg = d_test_ventreg,
    d_full_ventfol = d_test_ventfol,
    d_full_conclude = d_test_conclude
  ))
})
