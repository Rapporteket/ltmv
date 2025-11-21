#' Server logic for the ltmv app
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return A shiny app server object
#' @export
app_server = function(input, output, session) {
  rapbase::logShinyInputChanges(input)
  rapwhale::aktiver_kvalregtema()

  rapbase::appLogger(session = session, msg = "Starting ltmv application :-)")

  registry_name = "ltmv"
  hospital_name = "Udefinert avdeling/sykehus"
  # user_full_name = rapbase::getUserFullName(session)
  # user_role = rapbase::getUserRole(session)
  # user_resh_id = rapbase::getUserReshId(session)

  map_orgname = ltmv::hent_skjema("centre") |>
    select(UnitId = id, orgname = centrename)
  user = rapbase::navbarWidgetServer2("ltmv-navbar-widget", "ltmv", caller = "ltmv", map_orgname = map_orgname)

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

  v_rhf = pull(d_centretype, name)

  d_dashboard = shiny::reactive({
    lag_datasett_dashboard(
      fra = input$dato_dashboard[1],
      til = input$dato_dashboard[2],
      alderkategori = input$alderkat_dashboard,
      alderkategori_naa = input$alderkat_naa_dashboard,
      kjonn = input$kjonn,
      inkluder_missing = input$inkluder_missing,
      resh_id = user$unit(),
      user_role = user$role(),
      enhetstype = input$enhet_type,
      per_rhf = input$rhf_utvalg_dashboard,
      per_hf = input$hf_utvalg_dashboard,
      per_sykehus = input$sykehus_utvalg_dashboard
    )
  })

  output$aktivitetsoversikt = shiny::reactive({
    lag_aktivitetsoversikt(d_dashboard())
  })

  output$diagnosefordeling = renderPlot(
    {
      lag_fig_diagnosefordeling(d_dashboard())
    },
    res = 150
  )

  output$aldersfordeling = renderPlot(
    {
      lag_fig_aldersfordeling(d_dashboard())
    },
    res = 150
  )

  d_ki = shiny::reactive({
    d_dashboard() |>
      filter(lubridate::year(start_date) >= lubridate::year(Sys.Date()) - 5)
  })

  output$blodgass_for = renderPlot({
    lag_spcfigur_ki_blodgass(d_ki())
  })

  output$download_report = shiny::downloadHandler(
    filename = function() {
      basename(tempfile(
        pattern = "ltmv-sample_report",
        fileext = paste0(".", input$format_report)
      ))
    },
    content = function(file) {
      fn = rapbase::renderRmd(
        system.file("sample_report.Rmd", package = "ltmv"),
        outputType = input$format_report,
        params = list(
          author = user$fullName(),
          hospital_name = hospital_name,
          table_format = input$format_report,
          resh_id = user$unit(),
          registry_name = registry_name,
          user_full_name = user$fullName(),
          user_role = user$role()
        )
      )
      file.rename(fn, file)
    }
  )

  dagens_dato = lubridate::today()

  observeEvent(input$vise_alt, {
    updateDateRangeInput(
      session = session,
      inputId = "dato_dashboard",
      start = "1961-01-01",
      end = dagens_dato
    )
  })

  observeEvent(input$to_aar_knapp, {
    updateDateRangeInput(
      session = session,
      inputId = "dato_dashboard",
      start = dagens_dato - lubridate::years(2),
      end = dagens_dato
    )
  })

  observeEvent(input$fem_aar_knapp, {
    updateDateRangeInput(
      session = session,
      inputId = "dato_dashboard",
      start = dagens_dato - lubridate::years(5),
      end = dagens_dato
    )
  })

  observeEvent(input$alle_datoer_knapp, {
    updateDateRangeInput(
      session = session,
      inputId = "dato_antall_skjema",
      start = "2014-01-01",
      end = dagens_dato
    )
  })

  observeEvent(input$eldre_enn_ett_aar, {
    updateDateRangeInput(
      session = session,
      inputId = "dato_antall_skjema",
      start = "2014-01-01",
      end = dagens_dato - lubridate::years(1)
    )
  })

  observeEvent(input$innevarende_aar_knapp, {
    updateDateRangeInput(
      session = session,
      inputId = "dato_antall_skjema",
      start = paste0(lubridate::year(dagens_dato), "-01-01"),
      end = dagens_dato
    )
  })

  observeEvent(input$tretti_dager_knapp, {
    updateDateRangeInput(
      session = session,
      inputId = "dato_antall_skjema",
      start = dagens_dato - 29,
      end = dagens_dato
    )
  })

  observeEvent(input$syv_dager_knapp, {
    updateDateRangeInput(
      session = session,
      inputId = "dato_antall_skjema",
      start = dagens_dato - 6,
      end = dagens_dato
    )
  })

  output$antall_skjema = reactive({
    lag_antall_skjema_tabell(
      fra = input$dato_antall_skjema[1],
      til = input$dato_antall_skjema[2],
      alderkat = input$alderkat,
      aktiv_behandling = input$aktiv_behandling,
      resh_id = user$unit(),
      user_role = user$role(),
      vis_hf = input$kun_hf,
      vis_rhf = input$kun_rhf,
      rhf_utvalg = input$rhf_utvalgt
    )
  })

  output$utvalgte_rhf = renderUI({
    if (user$role() == "SC") {
      shiny::checkboxGroupInput("rhf_utvalgt",
        label = "Utvalgte RHF:",
        choices = v_rhf,
        selected = v_rhf
      )
    } else {
      NULL
    }
  })

  output$rhf = renderUI({
    if (user$role() == "SC") {
      shiny::checkboxInput("kun_rhf",
        label = tags$strong("Vis kun RHF"),
        value = FALSE
      )
    } else {
      NULL
    }
  })

  output$hf = renderUI({
    if (user$role() == "SC") {
      shiny::checkboxInput("kun_hf",
        label = tags$strong("Vis per HF"),
        value = FALSE
      )
    } else {
      NULL
    }
  })

  observeEvent(input$kun_rhf, {
    if (isTRUE(input$kun_rhf)) {
      updateCheckboxInput(session, "kun_hf", value = FALSE)
    }
  })

  observeEvent(input$kun_hf, {
    if (isTRUE(input$kun_hf)) {
      updateCheckboxInput(session, "kun_rhf", value = FALSE)
    }
  })


  # dummy report and orgs to subscribe and dispatch
  orgs = list(
    TestOrg = 999999
  )
  report = list()

  # subscribe
  rapbase::autoReportServer(
    "ltmv-subscription",
    registryName = registry_name, type = "subscription",
    reports = report, orgs = orgs,
    user = user
  )

  # dispatch
  org = rapbase::autoReportOrgServer("ltmv-dispatch-org", orgs)
  file_format = rapbase::autoReportFormatServer("ltmv-dispatch-format")
  param_names = shiny::reactive("output_format")
  param_values = shiny::reactive(c(file_format()))
  rapbase::autoReportServer(
    "ltmv-dispatch",
    registryName = registry_name, type = "dispatchment",
    org = org$value,
    paramNames = param_names, paramValues = param_values, reports = report,
    orgs = orgs,
    user = user
  )

  # use stats
  rapbase::statsGuideServer("ltmv-stats", registryName = registry_name)
  rapbase::statsServer("ltmv-stats", registryName = registry_name, app_id = Sys.getenv("FALK_APP_ID"))

  # export
  rapbase::exportGuideServer("ltmv-export", registry_name)
  rapbase::exportUCServer("ltmv-export", dbName = "data", teamName = "ltmv")
}
