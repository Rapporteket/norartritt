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
