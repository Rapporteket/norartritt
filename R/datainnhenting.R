
#' les data NorArtritt
#'
#' @description
#' Leser inn data fra NorArtritt og lagrer de som objekt.
#'
#'
#' @param mappe_dd Grunnmappe som datadumper skal hentes fra. Må ha undermapper
#'     med datonavn (format: `ÅÅÅÅ-MM-DD`). Hvis `NULL` (standard), blir
#'     grunnmappe_norartritt brukt.
#' @param dato Datoen for den aktuelle datadumpen (enten tekst på formatet `ÅÅÅÅ-MM-DD`
#'     eller et [base::Date]-objekt). Brukes for å velge riktig undermappe i
#'      `mappe_dd`. Hvis `NULL` (standard), brukes siste dato i `mappe_dd`.
#' @param versjon Versjon av data som skal lastes ned. Standard er `MRS-PROD`.
#' Kan endres om det skal lastes ned data fra test eller QA-versjon av registeret.
#' Versjon er gjenspeilet i datafilnavn så det må stemme overens med navn på
#' filene som er lastet ned.
#' @param omgjevnad Miljøet som dataobjektene skal lagres til.
#' Standard er det globale miljøet.
#'
#' @details
#' Leser inn standard datafiler for NorArtritt og lagrer som dataobjekt.
#' Foreløpig leses det kun inn hele datafiler, men validering skal legges til
#' når funksjonene for det er klare. Datafilene er ufiltrerte, så de inneholder
#' blant annet enkelte diagnoser som ikke er relevante for analyser.
#' Disse vil filtreres bort når valideringsfunksjonene er klare, og det vil
#' også returneres filtrerte og validerte datasett i denne funksjonen.
#' Det skal også legges inn støtte for de resterende datafilene som ikke brukes
#' per dags dato, men som kan tenkes å være nyttige i fremtiden.
#'
#' @export
#' @examples
#' \dontrun{
#' # Les inn data (nyeste datadumper)
#' les_data_norartritt()
#' }
les_data_norartritt = function(mappe_dd, dato = NULL, versjon = NULL, omgjevnad = .GlobalEnv) {
  if (is.null(mappe_dd)) {
    mappe_dd = grunnmappe_norartritt
  }

  # lagre dato for datadump
  if (is.null(dato)) {
    dato = dir(mappe_dd, pattern = "^[0-9]{4}-[0-1][0-9]-[0-9]{2}$", full.names = FALSE) |>
      sort() |>
      dplyr::last()
  }
  dato = lubridate::as_date(dato)

  if (is.null(versjon)) {
    versjon = "MRS-PROD"
  }

  if (file.exists(paste0(mappe_dd, dato, "\\datadump.RData"))) {
    load(paste0(mappe_dd, dato, "\\datadump.RData"),
      envir = omgjevnad
    )
  } else {
    assign("datadump_dato", dato, envir = omgjevnad)
    # les inn kodebok
    kb = rapwhale::les_kb_mrs(mappe_dd, dato = dato)

    # les inn data
    les_inn_data = function(skjema_id,
                            kb = kb,
                            dato = parent.frame()$dato,
                            versjon = parent.frame()$versjon) {
      # skjekk at skjema finnes i datadump-mappe, hvis ikke hopper vi over den
      if (any(stringr::str_detect(
        string = list.files(paste0(mappe_dd, "\\", dato, "\\")),
        pattern = skjema_id
      ))) {
        d = rapwhale::les_dd_mrs(mappe_dd,
          dato = parent.frame()$dato,
          versjon = parent.frame()$versjon,
          skjema_id = skjema_id,
          kodebok = kb
        )

        # if(skjema_id == "Medisineringskjema") {
        #   valider_legemiddeltype(mappe_dd)
        # }

        # returnerer dataene
        objektnamn = paste0("d_full_", skjema_id)
        assign(objektnamn, d, envir = omgjevnad)
      } else {
        print(paste0(skjema_id, " finnes ikke i mappe med datadumper. "))
      }
    }

    les_inn_data(skjema_id = "Inklusjonskjema", kb = kb, versjon = versjon)
    les_inn_data(skjema_id = "Diagnoseskjema", kb = kb, versjon = versjon)
    les_inn_data(skjema_id = "Oppfølgingskjema", kb = kb, versjon = versjon)
    les_inn_data(skjema_id = "Oppfølgingskjema2", kb = kb, versjon = versjon)
    les_inn_data(skjema_id = "Medisineringskjema", kb = kb, versjon = versjon)
    les_inn_data(skjema_id = "Medisineringskjema2", kb = kb, versjon = versjon)
    les_inn_data(skjema_id = "Billeddiagnostikkskjema", kb = kb, versjon = versjon)
    les_inn_data(skjema_id = "Bivirkningskjema", kb = kb, versjon = versjon)
    les_inn_data(skjema_id = "Historiskdose", kb = kb, versjon = versjon)
    les_inn_data(skjema_id = "Infusjonslogg", kb = kb, versjon = versjon)
    les_inn_data(skjema_id = "Injeksjonskjema", kb = kb, versjon = versjon)
    les_inn_data(skjema_id = "Komorbidtilstandskjema", kb = kb, versjon = versjon)
    les_inn_data(skjema_id = "Promsinklusjonsskjema", kb = kb, versjon = versjon)

    save(kb,
      datadump_dato,
      d_full_Inklusjonskjema,
      d_full_Diagnoseskjema,
      d_full_Oppfølgingskjema,
      d_full_Oppfølgingskjema2,
      d_full_Medisineringskjema,
      d_full_Medisineringskjema2,
      d_full_Billeddiagnostikkskjema,
      d_full_Bivirkningskjema,
      d_full_Historiskdose,
      d_full_Infusjonslogg,
      d_full_Injeksjonskjema,
      d_full_Komorbidtilstandskjema,
      d_full_Promsinklusjonsskjema,
      file = paste0(mappe_dd, dato, "\\datadump.RData"),
      precheck = FALSE
    )
  }
}
