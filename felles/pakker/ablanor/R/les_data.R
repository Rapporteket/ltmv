# Grunnmappe for datafiler (pakkeintern variabel)
grunnmappe_ablanor = "***FJERNA-ADRESSE***"

#' Les inn AblaNor-data
#'
#' @description
#' Les inn AblaNor-data og lagra dei som objekt, eventuelt med automatisk validering.
#'
#' @param mappe_dd Grunnmappe som datadumpar skal hentast frå. Må ha undermapper
#'     med datonamn (format «ÅÅÅÅ-MM-DD»). Viss `NULL` (standard), vert standardmappa
#'     med PROD-data brukt.
#' @param dato Datoen for den aktuelle datadumpen (tekst på formatet «ÅÅÅÅ-MM-DD»
#'     eller eit [base::Date]-objekt). Vert brukt til å velja rett undermappe
#'     til `grunnmappe`. Viss `NULL` (standard), vert nyaste dato brukt.
#' @param status Type oppføringar som skal lesast. Sjå [rapwhale::les_dd_oqr()]
#'     for informasjon om moglege verdiar. Som standard vert berre ferdigstilte oppføringar lesne.
#' @param valider Skal dataa validerast, dvs. sjekkast for feilverdiar før innlesing? (Standard: ja.)
#'     Omfattar både testar ved bruk av kodeboka og registerspesifikke testar.
#' @param omgjevnad Omgjevnad som dataobjekta skal lagrast til. Standard er den globale omgjevnaden.
#'
#' @details
#' Alle standard datafiler til registeret vert lesne inn, eventuelt validerte,
#' og så lagra som R-objekt. Dei får namna `d_skjemanamn`, der `skjemanamn`
#' er namnet på det aktuelle skjemaet (for eksempel `d_basereg` for basisskjemaet).
#' Som standard vert dei lagra i den globale omgjevnaden, og dei vil overskriva eventuelle
#' eksisterande objekt med same namn.
#'
#' @export
#' @examples
#' \dontrun{
#' # Les inn ferdigstilte oppføringar frå nyaste datadump
#' les_data_ablanor()
#' }
les_data_ablanor = function(mappe_dd = NULL, dato = NULL, status = 1,
                            valider = TRUE, omgjevnad = .GlobalEnv) {
  if (is.null(mappe_dd)) {
    mappe_dd = grunnmappe_ablanor
  }

  # ID brukt i datadump-filnamn
  register_id = "AblaNor"

  kb = rapwhale::les_kb_oqr(mappe_dd, reg_id = register_id)
  les_og_lagra = function(skjema) {
    d = rapwhale::les_dd_oqr(mappe_dd, reg_id = register_id, skjema_id = skjema, status = status, dato = dato, kodebok = kb)
    objektnamn = paste0("d_", skjema)
    assign(objektnamn, d, envir = omgjevnad)
  }

  # fixme: Legg til validering
  les_og_lagra("basereg")
  les_og_lagra("pros")
  les_og_lagra("gkv")
  les_og_lagra("rand12")
}
