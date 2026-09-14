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
      resh_id = user$org(),
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

  output$diagnosefordeling = shiny::renderPlot(
    {
      lag_fig_diagnosefordeling(d_dashboard())
    },
    res = 150
  )

  output$aldersfordeling = shiny::renderPlot(
    {
      lag_fig_aldersfordeling(d_dashboard())
    },
    res = 150
  )

  d_ki = shiny::reactive({
    d_dashboard() |>
      filter(lubridate::year(start_date) >= lubridate::year(Sys.Date()) - 5)
  })

  output$blodgass_for = shiny::renderPlot({
    lag_spcfigur_ki_blodgass(d_ki())
  })

  d_ki_superbreitt = shiny::reactive({
    lag_datasett_superbreitt_dashboard(
      fra = input$dato_dashboard[1],
      til = input$dato_dashboard[2],
      alderkat = input$alderkat_dashboard,
      kjonn = input$kjonn,
      inkluder_missing = input$inkluder_missing,
      resh_id = user$org(),
      user_role = user$role(),
      enhetstype = input$enhet_type,
      per_rhf = input$rhf_utvalg_dashboard,
      per_hf = input$hf_utvalg_dashboard,
      per_sykehus = input$sykehus_utvalg_dashboard
    ) |>
      filter(lubridate::year(r_start_date) >= lubridate::year(Sys.Date()) - 5)
  })

  output$blodgass_forste_aar = shiny::renderPlot({
    lag_spcfigur_ki_blodgass_forste_aar(d_ki_superbreitt())
  })

  dagens_dato = lubridate::today()

  shiny::observeEvent(input$vise_alt, {
    shiny::updateDateRangeInput(
      session = session,
      inputId = "dato_dashboard",
      start = "1961-01-01",
      end = dagens_dato
    )
  })

  shiny::observeEvent(input$to_aar_knapp, {
    shiny::updateDateRangeInput(
      session = session,
      inputId = "dato_dashboard",
      start = dagens_dato - lubridate::years(2),
      end = dagens_dato
    )
  })

  shiny::observeEvent(input$fem_aar_knapp, {
    shiny::updateDateRangeInput(
      session = session,
      inputId = "dato_dashboard",
      start = dagens_dato - lubridate::years(5),
      end = dagens_dato
    )
  })

  output$enhet_dashboard = shiny::renderUI(
    if (user$role() == "SC") {
      shiny::radioButtons("enhet_type",
        label = "Enhetstype:",
        choices = c("RHF", "HF", "Sykehus"),
        selected = "RHF",
        inline = TRUE
      )
    } else {
      NULL
    }
  )

  output$rhf_dashboard = shiny::renderUI(
    if (user$role() == "SC" && input$enhet_type == "RHF") {
      shiny::checkboxGroupInput("rhf_utvalg_dashboard",
        label = NULL,
        choices = v_rhf,
        selected = v_rhf
      )
    } else {
      NULL
    }
  )

  output$hf_dashboard = shiny::renderUI(
    if (user$role() == "SC" && input$enhet_type == "HF") {
      shiny::selectizeInput("hf_utvalg_dashboard",
        label = NULL,
        choices = sort(d_id_sykehus_hf_rhf$hf),
        multiple = TRUE,
        options = list(
          placeholder = "Trykk her for å velge HF"
        )
      )
    } else {
      NULL
    }
  )

  output$sykehus_dashboard = shiny::renderUI(
    if (user$role() == "SC" && input$enhet_type == "Sykehus") {
      shiny::selectizeInput("sykehus_utvalg_dashboard",
        label = NULL,
        choices = sort(
          d_id_sykehus_hf_rhf$sykehusnavn[!grepl(
            "HF|IKT|AS|LANDSFORENINGEN FOR HJERTE OG LUNGESYKE",
            d_id_sykehus_hf_rhf$sykehusnavn
          )]
        ),
        multiple = TRUE,
        options = list(
          placeholder = "Trykk her for å velge sykehus"
        )
      )
    } else {
      NULL
    }
  )

  shiny::observeEvent(input$alle_datoer_knapp, {
    shiny::updateDateRangeInput(
      session = session,
      inputId = "dato_antall_skjema",
      start = "2014-01-01",
      end = dagens_dato
    )
  })

  shiny::observeEvent(input$eldre_enn_ett_aar, {
    shiny::updateDateRangeInput(
      session = session,
      inputId = "dato_antall_skjema",
      start = "2014-01-01",
      end = dagens_dato - lubridate::years(1)
    )
  })

  shiny::observeEvent(input$innevarende_aar_knapp, {
    shiny::updateDateRangeInput(
      session = session,
      inputId = "dato_antall_skjema",
      start = paste0(lubridate::year(dagens_dato), "-01-01"),
      end = dagens_dato
    )
  })

  shiny::observeEvent(input$tretti_dager_knapp, {
    shiny::updateDateRangeInput(
      session = session,
      inputId = "dato_antall_skjema",
      start = dagens_dato - 29,
      end = dagens_dato
    )
  })

  shiny::observeEvent(input$syv_dager_knapp, {
    shiny::updateDateRangeInput(
      session = session,
      inputId = "dato_antall_skjema",
      start = dagens_dato - 6,
      end = dagens_dato
    )
  })

  output$antall_skjema = shiny::reactive({
    lag_antall_skjema_tabell(
      fra = input$dato_antall_skjema[1],
      til = input$dato_antall_skjema[2],
      alderkat = input$alderkat,
      aktiv_behandling = input$aktiv_behandling,
      resh_id = user$org(),
      user_role = user$role(),
      vis_hf = input$kun_hf,
      vis_rhf = input$kun_rhf,
      rhf_utvalg = input$rhf_utvalgt
    )
  })

  output$utvalgte_rhf = shiny::renderUI({
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

  output$rhf = shiny::renderUI({
    if (user$role() == "SC") {
      shiny::checkboxInput("kun_rhf",
        label = tags$strong("Vis kun RHF"),
        value = FALSE
      )
    } else {
      NULL
    }
  })

  output$hf = shiny::renderUI({
    if (user$role() == "SC") {
      shiny::checkboxInput("kun_hf",
        label = tags$strong("Vis per HF"),
        value = FALSE
      )
    } else {
      NULL
    }
  })

  shiny::observeEvent(input$kun_rhf, {
    if (isTRUE(input$kun_rhf)) {
      shiny::updateCheckboxInput(session, "kun_hf", value = FALSE)
    }
  })

  shiny::observeEvent(input$kun_hf, {
    if (isTRUE(input$kun_hf)) {
      shiny::updateCheckboxInput(session, "kun_rhf", value = FALSE)
    }
  })

  # Lager oversikt over HF til HF-rapport
  d_full_ventreg = hent_skjema("ventreg") |>
    legg_til_hf_rhf_navn()

  d_rapport_hf_resh = d_full_ventreg |>
    group_by(hf_resh) |>
    count() |>
    filter_out(n < 5) |> # Fjerner sykehus som ligger inne med færre enn 5 pasienter
    pull(hf_resh)

  hf_valg_rapport = d_full_ventreg |>
    filter_out(hf_resh == 106635) |> # Fjerner Lovisenberg
    filter(hf_resh %in% !!d_rapport_hf_resh) |>
    distinct(hf_tekst) |>
    pull(hf_tekst)

  shiny::observe({
    # Viser bare fanen "Rapporter" dersom rolle er "SC"
    if (user$role() == "SC") {
      shiny::showTab(inputId = "tabs", target = "tab_rapport")

      shiny::updateSelectInput(
        session,
        "HF_valg",
        choices = hf_valg_rapport,
        selected = "Helse Bergen"
      )
    } else {
      shiny::hideTab(inputId = "tabs", target = "tab_rapport")
    }
  })

  # Egen mappe per session som bare inneholder den ferdige rapporten.
  # session$token sikrer at samtidige brukere ikke overskriver
  # hverandre sin ressurssti, og at ingen midlertidige filer fra
  # knitr/LaTeX blir eksponerte over HTTP.
  rapport_mappe = file.path(tempdir(), paste0("hf-rapport-", session$token))
  dir.create(rapport_mappe, showWarnings = FALSE, recursive = TRUE)

  rapport_prefiks = paste0("rapport-", session$token)
  shiny::addResourcePath(prefix = rapport_prefiks, directoryPath = rapport_mappe)

  session$onSessionEnded(function() {
    shiny::removeResourcePath(rapport_prefiks)
    unlink(rapport_mappe, recursive = TRUE)
  })

  filsti_generert_rapport = shiny::reactiveVal(NULL)
  web_src = shiny::reactiveVal(NULL)

  # Nullstiller rapporten når valgene i sidemenyen endres,
  # slik at nedlastingsknappen aldri kan levere en rapport for et annet HF,
  # år eller filformat enn det som står i menyen (og filnavnet).
  shiny::observeEvent(
    list(input$HF_valg, input$aar_valg, input$format_report),
    {
      filsti_generert_rapport(NULL)
      web_src(NULL)
    },
    ignoreInit = TRUE
  )

  # Når man trykker på "Generer Rapport":
  shiny::observeEvent(input$generer, {
    id = shiny::showNotification(
      "Genererer ny rapport...",
      duration = 10,
      type = "message"
    )
    on.exit(shiny::removeNotification(id), add = TRUE)

    # Forsøker å kjøre rapbase::renderRmd og gir ut feilmelding om det krasjer
    tryCatch(
      {
        fn = rapbase::renderRmd(
          system.file("HF_rapport.Rmd", package = "ltmv"),
          outputType = input$format_report,
          # Liste med parametere til rapporten, som velges i sidemeny
          params = list(
            HF_navn = input$HF_valg,
            rapporteringsaar = input$aar_valg
          ),
          template = NULL,
          quiet = FALSE
        )
        output$rapport_visning = shiny::renderUI({
          shiny::tags$iframe(
            src = web_src,
            style = "width:100%; height: calc(100vh - 155px); border: none;"
          )
        })
        # renderRmd gir filen eit tilfeldig namn.
        # Kopien som blir servert får et fast, enkelt navn,
        # slik at URL-en slipper mellomrom og æøå fra HF-navnet.
        # Det pene filnamnet blir satt i downloadHandler under.
        servert_namn = paste0("rapport.", input$format_report)

        # Bare den nyeste rapporten skal ligge i den eksponerte mappen
        unlink(list.files(rapport_mappe, full.names = TRUE), recursive = TRUE)
        file.copy(from = fn, to = file.path(rapport_mappe, servert_namn))
        unlink(fn)

        filsti_generert_rapport(file.path(rapport_mappe, servert_namn))
        web_src(file.path(rapport_prefiks, servert_namn))
      },
      error = function(e) {
        shiny::showNotification(
          paste("Feil ved generering:", e$message),
          type = "error"
        )
      }
    )
  })

  output$last_ned_knapp = shiny::renderUI({
    if (rapport_ferdig()) {
      shiny::downloadButton("download_report", "Last ned rapport")
    }
  })

  output$download_report = shiny::downloadHandler(
    filename = function() {
      paste0(
        "HF-rapport-", input$HF_valg, "-", Sys.Date(), ".", input$format_report
      )
    },
    content = function(file) {
      generert_rapport = filsti_generert_rapport()
      file.rename(generert_rapport, file)
    }
  )

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
