lag_datasett_superbreitt_dashboard = function(fra,
                                              til,
                                              alderkategori,
                                              kjonn,
                                              inkluder_missing,
                                              resh_id,
                                              user_role) {
  d_superbreitt_dashboard = superbreitt_format(
    d_full_patientlist = hent_skjema("patient"),
    d_full_mce = hent_skjema("mce"),
    d_full_ventreg = hent_skjema("ventreg"),
    d_full_ventfol = hent_skjema("ventfol"),
    d_full_conclude = hent_skjema("conclude")
  ) |>
    legg_til_alder_og_kategori(p_Fodselsdato, r_start_date) |>
    filter(
      r_start_date >= !!fra | (is.na(r_start_date) & inkluder_missing),
      r_start_date <= !!til | (is.na(r_start_date) & inkluder_missing),
      alderkat %in% !!alderkategori |
        (is.na(alderkat) & "" %in% !!alderkategori),
      p_Kjonn %in% !!kjonn | (is.na(p_Kjonn) & "" %in% !!kjonn)
    )

  if (user_role != "SC") {
    d_superbreitt_dashboard = filter(d_superbreitt_dashboard, r_centreid == !!resh_id) # FIXME Vert ikkje heilt rett med r_centreid her
  }

  d_superbreitt_dashboard
}

superbreitt_format = function(d_full_patientlist,
                              d_full_mce,
                              d_full_ventreg,
                              d_full_ventfol,
                              d_full_conclude) {
  # Pasientdata
  if (length(names(d_full_patientlist)) == 33) {
    d_full_patientlist = d_full_patientlist |>
      dplyr::select(
        "pasientid" = "id",
        "RegistreringsDato" = "registered_date",
        "Fodselsdato" = "birth_date",
        "Kjonn" = "gender",
        "Avdod" = "deceased",
        "Dodsdato" = "deceased_date",
        "Postnummer" = "zipcode",
        "Poststed" = "town",
        "Kommune" = "county",
        "Fylke" = "municipality_name"
      )
  }

  d_p = rename_with(d_full_patientlist, ~ paste0("p_", .x))

  # Kopling skjemaid til forelderid og pasientid
  d_mce = select(d_full_mce, mceid, patient_id, parent_mce)

  # Førstegangsregistrering
  d_r = d_full_ventreg |>
    rename_with(~ paste0("r_", .x)) |>
    left_join(d_mce, by = join_by(r_mceid == mceid))

  # Ventfol med pasientid
  d_full_ventfol_ekstra = left_join(
    x = d_full_ventfol,
    y = d_mce,
    by = join_by(mceid == mceid)
  )

  # Oppfylgjing år 1
  d_f1 = d_full_ventfol_ekstra |>
    filter(year == 1) |>
    rename_with(~ paste0("f1_", .x), .cols = !c(patient_id, parent_mce)) |>
    sjekk_duplikat("1")

  # Oppfylgjing år 3
  d_f3 = d_full_ventfol_ekstra |>
    filter(year == 3) |>
    rename_with(~ paste0("f3_", .x), .cols = !c(patient_id, parent_mce)) |>
    sjekk_duplikat("3")

  # Fyrste ad hoc oppfylgjing
  d_fah = d_full_ventfol_ekstra |>
    filter(year == -1) |>
    rename_with(~ paste0("fah_", .x), .cols = !c(patient_id, parent_mce)) |>
    arrange(fah_followup_date) |>
    # Øvste skjema når det finst fleire med same dato
    distinct(parent_mce, .keep_all = TRUE)

  # Siste oppfylgjing
  d_lf = d_full_ventfol_ekstra |>
    rename_with(~ paste0("lf_", .x), .cols = !c(patient_id, parent_mce)) |>
    group_by(parent_mce) |>
    slice_max(order_by = lf_followup_date) |>
    # Høgste oppfylgingsår når det finst fleire med same dato
    slice_max(order_by = lf_year) |>
    # Nedste skjema når det finst fleire med same dato og oppfylgingsår
    arrange(-row_number()) |>
    distinct(parent_mce, .keep_all = TRUE)

  # Avslutning
  d_c = d_full_conclude |>
    rename_with(~ paste0("c_", .x)) |>
    left_join(d_mce, by = join_by(c_mceid == mceid))


  # Gjer om til superbreitt format

  d_superbrei = d_r |>
    left_join(d_p,
      by = join_by(patient_id == p_pasientid),
      relationship = "many-to-one"
    ) |>
    left_join(d_f1,
      by = join_by(r_mceid == parent_mce, patient_id),
      relationship = "one-to-one"
    ) |>
    left_join(d_f3,
      by = join_by(r_mceid == parent_mce, patient_id),
      relationship = "one-to-one"
    ) |>
    left_join(d_fah,
      by = join_by(r_mceid == parent_mce, patient_id),
      relationship = "one-to-one"
    ) |>
    left_join(d_lf,
      by = join_by(r_mceid == parent_mce, patient_id),
      relationship = "one-to-one"
    ) |>
    left_join(d_c,
      by = join_by(r_mceid == parent_mce, patient_id),
      relationship = "one-to-one"
    ) |>
    select(-parent_mce) |>
    relocate(patient_id, starts_with("p_"))
}

sjekk_duplikat = function(d, aar) {
  if (n_distinct(d$parent_mce) < nrow(d)) {
    warning("Det finst registreringsskjema med fleire ", aar, "-års-oppfølgingsskjema")
    # Øvste skjema når det finst fleire
    distinct(d, parent_mce, .keep_all = TRUE)
  } else {
    d
  }
}
