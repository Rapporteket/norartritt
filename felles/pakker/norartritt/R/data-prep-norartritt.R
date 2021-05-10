# Preprossesering av data for NorArtritt

#' @importFrom magrittr %>%
#' @importFrom dplyr add_count filter group_by arrange distinct select left_join mutate
NULL
#'
# Lage en funksjon for å forberede alle datasett i NorArtritt for analyse.
# Hovedfokus er for bruk i årsrapport, men jeg antar objektene vil være
# å foretrekke i øvrige analyser også.

# TODO
# Liste opp alt som skal forbedres i alle de ulike objektene
# Generelle ting som skal fikses i flere at objektene
# Funksjonen skal implementeres i les_data_norartritt() slik at en får ut
# både fullstendige datasett og ferdig prosseserte datasett.

# Fjerne skjema - eksempel - fjerne diagnose hvis inklusjon mangler.
# Filtrere bort pasienter som ikke har aktuelle diagnoser.
# Har de kun uaktuelle diagnoser skal de ikke være med i registeret i det hele tatt

# FIXME - Flytte hjelpefunksjoner til dette skriptet.

# Kjernefunksjon som kaller på alle underfunksjoner
vask_data_norartritt = function(d_inkl, d_oppf, d_diag, d_med) {

  # Legger sammen inklusjon og oppfølging
  d_inkl_oppf = d_inkl %>%
    bind_rows(d_oppf)

  # Rett inklusjonsdato
  d_inkl_oppf = velg_tidligste_inklusjondato(d_inkl_oppf = d_inkl_oppf)

  # Rett skjematype
  d_inkl_oppf = konverter_skjematype(inkl = d_inkl, oppf = d_oppf)

  # Pakk ut inklusjons og oppfølgingsskjema.
  d_inkl = d_inkl_oppf %>%
    filter(FormTypeId == 1)
  d_oppf = d_inkl_oppf %>%
    filter(FormTypeId == 2)

  # Filtrer bort ugyldige skjema
  fjern_ugyldige_skjema(inkl = d_inkl, oppf = d_oppf, diag = d_diag, med = d_med) # Returnerer filtrerte objekter

  # Legg til datovariabler
  skjema = legg_til_datovariabler(
    d_inkl = inkl_filtrert, d_oppf = oppf_filtrert,
    d_diag = diag_filtrert, d_med = med_filtrert
  )

  # Trekker ut de ulike skjematypene fra skjema objekt
  d_inkl = skjema[[1]]
  d_oppf = skjema[[2]]
  d_diag = skjema[[3]]
  d_med = skjema[[4]]

  # strukturer variabler som mangler kodebok

  # Lag utvalgte ferdigfiltrerte objekter
  lag_filtrerte_objekter(
    d_inkl = d_inkl,
    d_diag = d_diag,
    d_med = d_med
  )
}


#' Lag filtrerte objekter
#'
#' Lager filtrerte objekter som er nyttige for bruk i analyse til for eksempel
#' årsrapport.
#'
#' Objektene som returneres er:
#' * \strong{d_med_vasket} Inneholder alle medisinskjema for alle pasienter i
#' registeret som har en aktuell diagnose. Skjema uten startdato er fjernet, og
#' dupliserte skjema er tatt bort. For duplikater er skjema som har en
#' sluttdato beholdt.
#' * \strong{d_diag_pers} Inneholder siste diagnose for hver pasient.
#' * \strong{d_diag_med} Inneholder siste diagnose for hver pasient, i tillegg
#' til hele medisinhistorikken for pasientene. Her legges det også til
#' "Ingen medisin" for pasienter som ikke får medisinsk behandling.
#'
#' @param d_inkl Inklusjonsdatasett fra NorArtritt
#' @param d_diag Diagnosedatasett fra NorArtritt
#' @param d_med Medisindatasett fra NorArtritt
#'
#' @return
#' Returnerer objektene d_med_vasket, d_diag_pers og d_diag_med til det
#' globale miljøet.
#' @export
lag_filtrerte_objekter = function(d_inkl, d_diag, d_med) {

  # medisinforløp for hver pasient hvor duplikater er fjernet
  d_med_vasket = d_med %>%
    filter(!is.na(StartDato))

  skjemaid_duplikat = d_med_vasket %>%
    add_count(PasientGUID, StartDato, LegemiddelType) %>%
    filter(n > 1) %>%
    group_by(PasientGUID) %>%
    arrange(SluttDato) %>%
    distinct(PasientGUID, .keep_all = TRUE) %>%
    select(-n)

  d_med_vasket = setdiff(d_med_vasket, skjemaid_duplikat)

  # Siste diagnose for hver pasient
  d_diag_pers = d_diag %>%
    arrange(desc(dato_diag)) %>%
    distinct(PasientGUID, .keep_all = TRUE)

  # d_diag_med: Siste diagnose og hele medisinhistorikken, inkludert Ingen medisin
  # for de som ikke har medisinsk behandling
  d_diag_med = d_diag_pers %>%
    select(
      PasientGUID, UnitId, FormDate, PatientAge, PatientGender, Kode, Navn, Dato, DiagnoseDato,
      aar, dato_diag, diag_stilt_aar
    ) %>%
    left_join(d_med_vasket %>%
      select(
        PasientGUID, Enhet, Mengde, LegemiddelType, Legemiddel, Intervall,
        EndringArsak, StartDato, SluttDato, EndringsDato,
        startaar, sluttaar
      ), by = "PasientGUID") %>%
    legg_til_medisinnavn() %>% # FIXME - Flytte denne til tidligere objekt.
    legg_til_sykehusnavn() %>% # FIXME - Flytte til tidligere objekt
    mutate(
      legemiddel_navn = replace(
        as.character(legemiddel_navn),
        is.na(legemiddel_navn), "Ingen medisin"
      ),
      legemiddel_navn_kode = replace(
        as.integer(legemiddel_navn_kode),
        is.na(legemiddel_navn_kode), 99L
      ),
      biokat = replace(biokat, is.na(biokat), 99L),
      csdmard = ifelse(is.na(LegemiddelType), yes = 0L,
        no = csdmard
      ),
      dmard = ifelse(is.na(LegemiddelType), yes = 0L,
        no = dmard
      ),
      tsdmard = ifelse(is.na(LegemiddelType), yes = 0L,
        no = tsdmard
      ),
      bio_og_tsdmard = ifelse(is.na(LegemiddelType), yes = 0L,
        no = bio_og_tsdmard
      ),
      legemiddel_gruppert = replace(
        as.integer(legemiddel_gruppert),
        is.na(legemiddel_gruppert), 99L
      ),
      legemiddel_gruppert_navn = replace(
        as.character(legemiddel_gruppert_navn),
        is.na(legemiddel_gruppert_navn), "Ingen medisin"
      ),
      Virkestoff = replace(
        as.character(Virkestoff),
        is.na(Virkestoff), "Ingen medisin"
      ),
      diagnose_aar = lubridate::year(dato_diag)
    )

  assign("d_med_vasket", d_med_vasket, envir = .GlobalEnv)
  assign("d_diag_pers", d_diag_pers, envir = .GlobalEnv)
  assign("d_diag_med", d_diag_med, envir = .GlobalEnv)
}



strukturer_variabler_uten_kodebok = function(d_inkl, d_oppf, d_diag, d_med) {
}
