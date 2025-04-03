superbreitt_format= function(d_full_patientlist,
                  d_full_mce,
                  d_full_ventreg,
                  d_full_ventfol,
                  d_full_conclude){
  # Pasientdata
  if (length(names(d_full_patientlist))==33){
    d_full_patientlist=d_full_patientlist %>% dplyr::select("pasientid"="id",
                                         "RegistreringsDato"="registered_date",
                                         "Fodselsdato"="birth_date",
                                         "Kjonn"= "gender",
                                         "Avdod"= "deceased",
                                         "Dodsdato"= "deceased_date",
                                         "Postnummer"="zipcode",
                                         "Poststed"= "town",
                                         "Kommune"= "county",
                                         "Fylke"= "municipality_name")
  }

  d_p = d_full_patientlist %>%
    rename_with(~ paste0("p_", .x))

  # Kopling skjemaid til forelderid og pasientid
  d_mce = select(d_full_mce, mceid, patient_id, parent_mce)

  # Førstegangsregistrering
  d_r = d_full_ventreg %>%
    rename_with(~ paste0("r_", .x)) %>%
    left_join(d_mce, by = join_by(r_mceid == mceid))

  # Ventfol med pasientid
  d_full_ventfol_ekstra = left_join(
    x = d_full_ventfol,
    y = d_mce,
    by = join_by(mceid == mceid)
  )

  # Oppfylgjing år 1
  d_f1 = d_full_ventfol_ekstra %>%
    filter(year == 1) %>%
    rename_with(~ paste0("f1_", .x), .cols = !c(patient_id, parent_mce))

  # Oppfylgjing år 3
  d_f3 = d_full_ventfol_ekstra %>%
    filter(year == 3) %>%
    rename_with(~ paste0("f3_", .x), .cols = !c(patient_id, parent_mce))

  # Fyrste ad hoc oppfylgjing
  d_fah = d_full_ventfol_ekstra %>%
    filter(year == -1) %>%
    rename_with(~ paste0("fah_", .x), .cols = !c(patient_id, parent_mce)) %>%
    arrange(fah_followup_date) %>%
    # Øvste skjema når det finst fleire med same dato
    distinct(parent_mce, .keep_all = TRUE)

  # Siste oppfylgjing
  d_lf = d_full_ventfol_ekstra %>%
    rename_with(~ paste0("lf_", .x), .cols = !c(patient_id, parent_mce)) %>%
    group_by(parent_mce) %>%
    slice_max(order_by = lf_followup_date) %>%
    # Høgste oppfylgingsår når det finst fleire med same dato
    slice_max(order_by = lf_year) %>%
    # Nedste skjema når det finst fleire med same dato og oppfylgingsår
    arrange(-row_number()) %>%
    distinct(parent_mce, .keep_all = TRUE)

  # Avslutning
  d_c = d_full_conclude %>%
    rename_with(~ paste0("c_", .x)) %>%
    left_join(d_mce, by = join_by(c_mceid == mceid))


  # Gjer om til superbreitt format

  d_superbrei = d_r %>%
    left_join(d_p,
              by = join_by(patient_id == p_pasientid)
              #relationship = "many-to-one"
    ) %>%
    left_join(d_f1,
              by = join_by(r_mceid == parent_mce, patient_id)
              #relationship = "one-to-one"
    ) %>%
    left_join(d_f3,
              by = join_by(r_mceid == parent_mce, patient_id)
              #relationship = "one-to-one"
    ) %>%
    left_join(d_fah,
              by = join_by(r_mceid == parent_mce, patient_id)
              #relationship = "one-to-one"
    ) %>%
    left_join(d_lf,
              by = join_by(r_mceid == parent_mce, patient_id)
              #relationship = "one-to-one"
    ) %>%
    left_join(d_c,
              by = join_by(r_mceid == parent_mce, patient_id)
              #relationship = "one-to-one"
    ) %>%
    select(-parent_mce) %>%
    relocate(patient_id, starts_with("p_"))

}
