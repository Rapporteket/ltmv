# Grunnmappe for datafiler (pakkeintern variabel)
grunnmappe_ablanor = "***FJERNA-ADRESSE***"

#' Les inn AblaNor-data
#'
#' @description
#' Les inn AblaNor-datadumpar og lagra dei som objekt, eventuelt med automatisk validering.
#'
#' @param mappe_dd Grunnmappe som datadumpar skal hentast frå. Må ha undermapper
#'     med datonamn (format: `ÅÅÅÅ-MM-DD`). Viss `NULL` (standard), vert standardmappa
#'     med PROD-data brukt.
#' @param dato Datoen for den aktuelle datadumpen (anten tekst på formatet `ÅÅÅÅ-MM-DD`
#'     eller eit [base::Date]-objekt). Vert brukt til å velja rett undermappe
#'     til `mappe_dd`. Viss `NULL` (standard), vert nyaste dato brukt.
#' @param status Type oppføringar som skal lesast. Sjå [rapwhale::les_dd_oqr()]
#'     for informasjon om moglege verdiar. Som standard vert berre ferdigstilte oppføringar lesne.
#' @param valider Skal dataa validerast, dvs. sjekkast for feilverdiar før innlesing? (Standard: ja.)
#'     Omfattar både testar ved bruk av kodeboka og registerspesifikke testar.
#' @param omgjevnad Omgjevnad som dataobjekta skal lagrast til. Standard er den globale omgjevnaden.
#'
#' @details
#' Alle standard datafiler til registeret vert lesne inn, eventuelt validerte,
#' filtrerte (sjå nedanfor) og så lagra som R-objekt. Dei får namna `d_skjemanamn`, der `skjemanamn`
#' er namnet på det aktuelle skjemaet (for eksempel `d_basereg` for basisskjemaet).
#' Som standard vert dei lagra i den globale omgjevnaden, og dei vil overskriva eventuelle
#' eksisterande objekt med same namn.
#'
#' I AblaNor skal berre skjema som høyrer til forløp som har resultert i ein prosedyre
#' (eventuelt ein avbroten ein) analyserast. I objekta nemnde ovanfor er derfor oppføringar
#' for andre forløp filtrerte vekk. Viss ein person for eksempel berre har eit basisskjema
#' men ikkje (enno) eit prosedyreskjema, vil personen også vera filtrert vekk frå
#' basisskjema-datsettet (og forløpsdatasettet, pasientdatasettet og andre datasett).
#' Men dei *ufiltrerte* datasetta vert også lagra, då med namn på forma `d_full_skjemanamn`,
#' for eksempel `d_full_basereg` for basisdatasettet. Merk at desse «fullversjonane»
#' vanlegvis *ikkje* skal brukast til analysar. Men dei kan vera nyttige når ein skal
#' sjå på ferdigstillings&#173;statistikk (gjerne i kombinasjon med `status =  NULL`).
#'
#' @export
#' @examples
#' \dontrun{
#' # Les inn ferdigstilte skjema (nyaste datadumpar)
#' les_data_ablanor()
#' }
les_data_ablanor = function(mappe_dd = NULL, dato = NULL, status = 1,
                            valider = TRUE, omgjevnad = .GlobalEnv) {
  if (is.null(mappe_dd)) {
    mappe_dd = grunnmappe_ablanor
  }

  # ID brukt i datadump-filnamn
  register_id = "AblaNor"

  kb = rapwhale::les_kb_oqr(mappe_dd, reg_id = register_id, dato = dato, valider_kb = valider)
  les_og_lagra = function(skjema) {
    d = rapwhale::les_dd_oqr(mappe_dd,
      reg_id = register_id, skjema_id = skjema,
      status = status, dato = dato, kodebok = kb,
      valider_kb = FALSE, valider_dd = valider
    )
    objektnamn = paste0("d_full_", skjema)
    assign(objektnamn, d, envir = omgjevnad)
  }

  # fixme: Legg til validering

  # Les inn fullstendige datasett (utan filtrering på forløp)
  les_og_lagra("basereg")
  les_og_lagra("pros")
  les_og_lagra("gkv")
  les_og_lagra("rand12")

  # I AblaNor skal me berre sjå på forløp som har resultert
  # i prosedyrar (inkl. avbrotne prosedyrar). Filtrerer derfor
  # ut eventuelle andre forløp.
  mceid_akt = omgjevnad$d_full_pros$mceid
  filtrer_og_lagra = function(skjema) {
    objektnamn_full = paste0("d_full_", skjema)
    objektnamn_filtrert = paste0("d_", skjema)
    d_full = get(objektnamn_full, envir = omgjevnad)
    d_filtrert = dplyr::filter(d_full, mceid %in% !!mceid_akt)
    assign(objektnamn_filtrert, d_filtrert, envir = omgjevnad)
  }
  filtrer_og_lagra("basereg")
  filtrer_og_lagra("pros")
  filtrer_og_lagra("gkv")
  filtrer_og_lagra("rand12")
}
