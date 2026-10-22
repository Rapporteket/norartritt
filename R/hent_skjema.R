#' Hent skjema
#'
#' @description
#' Henter alle data fra et skjema.
#'
#' @param skjemanavn
#' Tekststreng med navn på skjemaet som skal hentes.
#' @param registernavn
#' Tekststreng med navn på register. Standardverdi er "data".
#' Argumentet er med for å kunne kjøre tester på testdatabase.
#'
#' @details
#' Fuksjonen bruker [rapbase::loadRegData()]
#' til å hente alle data fra skjemaet med navn `skjemanavn`:
#' `SELECT * FROM skjemanavn`. Funksjonen er avhengig av
#' at database er koblet til for å fungere.
#'
#' @return
#' Tibble med alle data fra skjemaet med navn `skjemanavn`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' hent_skjema("Inklusjon")
#' }
hent_skjema = function(skjemanavn, registernavn = "data") {
  sporring = paste0("SELECT * FROM ", skjemanavn)
  rapbase::loadRegData(registernavn, sporring) |>
    as_tibble()
}



#' hent og vask norartritt data
#'
#' @description
#' Datainnhenting for NorArtritt-rapporteket.
#'
#' Leser inn angitte skjema basert på navn i `skjema`.
#' Kjører standard vasking av disse og genererer objekter som legges i globalt
#' miljø.
#' Se `norartritt::vask_data_norartritt()` for mer utfyllende info om hvilke
#' objekter som returneres.
#'
#' @param report Rapporterinig av fremgang
#'
#' @returns
#' Returnerer objektene `d_diag`, `d_diag_med`, `d_diag_pers`, `d_dodsdato`,
#' `d_inkl`, `d_inkl_oppf`, `d_med`, `d_med_vasket` og  `d_oppf`til det globale miljøet.
#' Se `norartritt::vask_data_norartritt()` for mer informasjon om disse objektene.
#'
#' @export
#'
#' @examples
#' hent_og_vask_data_norartritt_rapportek()
hent_og_vask_data_norartritt_rapportek = function(report = function(message) invisible(NULL)) {

  skjema = c("inklusjonskjema_1", "oppfoelgingskjema_2",
             "diagnoseskjema_9", "medisineringskjema_3")

  l_alle = list()

  for(skjemanavn in skjema) {
    l_alle[[skjemanavn]] = hent_skjema(skjemanavn, "norartritt")
  }

  # Vaske data
  vask_data_norartritt(d_inkl = l_alle[[1]],
                       d_oppf = l_alle[[2]],
                       d_diag = l_alle[[3]],
                       d_med  = l_alle[[4]],
                       datadump_dato = Sys.Date(),
                       report = report)

}
