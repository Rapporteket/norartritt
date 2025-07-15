# Preprossesering av data for NorArtritt

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

#' vask data norartritt
#'
#' Funksjon som vasker data fra NorArtritt. Datadumper som lastes ned fra
#' registeret trenger preprossesering før det kan brukes i analyse.
#' Denne funksjonen gjør alt det nødvendige arbeidet med preprossesering
#' og lager nye objekter som brukes mye i analyse. Funksjonen vil normalt
#' sett ikke kalles direkte, men det kan være nyttig hvis en prøver å lese
#' inn et identifiserbart datasett med fødselsnummer, og ikke PasientGUID.
#'
#'
#' @param d_inkl Inklusjonsdatasett, vanligvis d_full_Inklusjonsskjema.
#' @param d_oppf Oppfølgingsdatasett, vanligvis d_full_Oppfølgingsskjema.
#' @param d_diag Diagnosedatasett, vanligvis d_full_Diagnoseskjema.
#' @param d_med Medisindatasett, vanligvis d_full_Medisineringskjema.
#'
#' @return
#' Returnerer filtrerte og *vaskede* datasett for ulike skjema i NorArtritt.
#'
#' Basisskjema:
#' * \strong{d_diag, d_med, d_inkl, d_oppf, d_inkl_oppf} \cr
#' Tilsvarer de fullstendige datasettene men med grunnleggende filtrering, slik
#' at blant annet ugyldige skjema er filtrert ut og datovariabler er kontrollert.
#'
#' Koblede skjema:
#' * \strong{d_med_vasket} \cr
#' Inneholder alle medisinskjema for alle pasienter i
#' registeret som har en aktuell diagnose. Skjema uten startdato er fjernet, og
#' dupliserte skjema er tatt bort. For duplikater er skjema som har en
#' sluttdato beholdt.
#' * \strong{d_diag_pers} \cr
#' Inneholder siste diagnose for hver pasient.
#' * \strong{d_diag_med} \cr
#' Inneholder siste diagnose for hver pasient, i tillegg
#' til hele medisinhistorikken for pasientene. Her legges det også til
#' "Ingen medisin" for pasienter som ikke får medisinsk behandling.
#'
#' * \strong{d_dodsdato} \cr
#' Inneholder oversikt over dødsdato, hvor tilgjengelig.
#'
#' @export
vask_data_norartritt = function(d_inkl, d_oppf, d_diag, d_med) {
  # Legger sammen inklusjon og oppfølging
  d_inkl_oppf = bind_rows(d_inkl, d_oppf)

  # Rett inklusjonsdato
  d_inkl_oppf = velg_tidligste_inklusjondato(d_inkl_oppf = d_inkl_oppf)

  # Pakk ut inklusjons og oppfølgingsskjema.
  d_inkl = filter(d_inkl_oppf, FormTypeId == 1)
  d_oppf = filter(d_inkl_oppf, FormTypeId == 2)

  # Rett skjematype
  d_inkl_oppf = konverter_skjematype(inkl = d_inkl, oppf = d_oppf)

  # Pakk ut inklusjons og oppfølgingsskjema.
  d_inkl = filter(d_inkl_oppf, FormTypeId == 1)
  d_oppf = filter(d_inkl_oppf, FormTypeId == 2)

  # Konsolidere skjemaGUIDs (For pasienter som har inklusjon/kontroll ved flere
  # sykehus vil det være ulike HovedskjemaGUIDs). Dette sikrer at alle oppfølginger
  # for den enkelte pasient følger med.
  skjemaguid_kobling = d_inkl |>
    group_by(PasientGUID) |>
    arrange(FormDate) |>
    distinct(PasientGUID, .keep_all = TRUE) |>
    select(PasientGUID, SkjemaGUID = SkjemaGUID)

  d_inkl = d_inkl |>
    select(-SkjemaGUID) |>
    left_join(skjemaguid_kobling, by = "PasientGUID")

  d_oppf = d_oppf |>
    select(-HovedskjemaGUID) |>
    left_join(rename(skjemaguid_kobling, HovedskjemaGUID = SkjemaGUID),
      by = "PasientGUID",
      relationship = "many-to-one"
    )

  d_diag = d_diag |>
    select(-HovedskjemaGUID) |>
    left_join(rename(skjemaguid_kobling, HovedskjemaGUID = SkjemaGUID),
      by = "PasientGUID",
      relationship = "many-to-one"
    )

  d_med = d_med |>
    select(-HovedskjemaGUID) |>
    left_join(rename(skjemaguid_kobling, HovedskjemaGUID = SkjemaGUID),
      by = "PasientGUID",
      relationship = "many-to-one"
    )

  # Filtrer bort ugyldige skjema
  filtrerte_skjema = fjern_ugyldige_skjema(
    inkl = d_inkl,
    oppf = d_oppf,
    diag = d_diag,
    med = d_med
  )
  inkl_filtrert = filtrerte_skjema[[1]]
  oppf_filtrert = filtrerte_skjema[[2]]
  diag_filtrert = filtrerte_skjema[[3]]
  med_filtrert = filtrerte_skjema[[4]]

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
    d_oppf = d_oppf,
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
lag_filtrerte_objekter = function(d_inkl, d_diag, d_med, d_oppf) {
  # Lager objekt med dødsdato for alle pasienter som har dødd
  d_dodsdato = d_inkl |>
    select(PasientGUID, DeathDate) |>
    filter(!is.na(DeathDate)) |>
    distinct(PasientGUID, .keep_all = TRUE) |>
    mutate(DeathDate = as_date(DeathDate))

  # Baseobjekter for alle skjema
  d_inkl = d_inkl |>
    konverter_missing_til_na() |>
    legg_til_sykehusnavn()

  d_oppf = d_oppf |>
    konverter_missing_til_na() |>
    legg_til_sykehusnavn()

  d_diag = d_diag |>
    legg_til_diagnosegrupper() |>
    left_join(d_dodsdato, by = "PasientGUID") |>
    filter(is.na(DeathDate) | DeathDate >= dato_diag)

  d_diag = legg_til_kriteriedatoer(
    d_inkl = d_inkl,
    d_diag = d_diag
  )

  d_med = d_med |>
    filter(!(LegemiddelType == 999 & is.na(Legemiddel))) |>
    # FIXME - Fjerner her medisinforløp med LegemiddelType 999 som kommer fra St. Olav etter innføring
    # av helseplattformen. Disse er på et format som gjør at vi må vedlikeholde et separat sett
    # med interne kodebøker for å ta imot data fra HP, og det er ikke aktuelt.
    # Tar de derfor bort fra analyse.
    # Fjern filtrering når data er korrigert og import er fikset i MRS/HP.
    # FIXME - Har også lagt Ålesund til listen etter de fikk innført helseplattformen der
    filter(
      !(LegemiddelType == 999 & Hospital == "St. Olav" & lubridate::date(CreationDate) > "2023-05-29"),
      !(LegemiddelType == 999 & Hospital == "Ålesund" & lubridate::date(CreationDate) > "2024-06-05")
    ) |>
    legg_til_medisinnavn() |>
    left_join(d_dodsdato, by = "PasientGUID") |>
    mutate(
      SluttDato = case_when(
        is.na(SluttDato) & !is.na(DeathDate) ~ DeathDate,
        SluttDato > DeathDate ~ DeathDate,
        DeathDate > SluttDato ~ SluttDato,
        TRUE ~ SluttDato
      ),
      startaar = lubridate::year(StartDato),
      sluttaar = lubridate::year(SluttDato)
    ) |>
    filter(
      StartDato < DeathDate | is.na(DeathDate),
      SluttDato <= datadump_dato | is.na(SluttDato),
      StartDato <= SluttDato | is.na(SluttDato)
    )

  # medisinforløp for hver pasient hvor duplikater er fjernet
  if (d_med |> filter(legemiddel_navn_kode == 999) |> nrow() > 0) {
    stop(error = "Det finnes legemiddel med LegemiddelType 999 som ikke er definert i kodebok")
  }

  # Fjerner duplikate medisinforløp og medisinforløp som mangler startdato
  d_med_vasket_uten_na = d_med |>
    filter(!is.na(StartDato))

  d_med_vasket = d_med_vasket_uten_na |>
    group_by(PasientGUID, StartDato, LegemiddelType) |>
    arrange(SluttDato) |>
    distinct(PasientGUID, StartDato, LegemiddelType, .keep_all = TRUE) |>
    ungroup()

  # Siste diagnose for hver pasient
  d_diag_pers = d_diag |>
    arrange(desc(dato_diag)) |>
    distinct(PasientGUID, .keep_all = TRUE)

  # d_diag_med: Siste diagnose og hele medisinhistorikken, inkludert Ingen medisin
  # for de som ikke har medisinsk behandling
  # FIXME - Forbedre select. Se hva som ekskluderes.
  d_diag_med = d_diag_pers |>
    select(
      PasientGUID, UnitId, FormDate, PatientAge, PatientGender, Kode, Navn, Dato, DiagnoseDato,
      dato_diag, diag_stilt_aar, dager_diag_til_datadump, diaggrupper_med, diaggrupper_med_tekst,
      diaggrupper_rem, diaggrupper_rem_tekst, rem_maal, diaggrupper_hoved,
      diaggrupper_hoved_tekst, perifer_aksial_diaggruppe,
      perifer_aksial_diaggruppe_tekst
    ) |>
    left_join(select(d_med_vasket,
      PasientGUID, Enhet, Mengde, LegemiddelType, Intervall,
      EndringArsak, StartDato, SluttDato, EndringsDato, DeathDate,
      startaar, sluttaar, legemiddel_navn, legemiddel_navn_kode,
      biokat, csdmard, dmard, tsdmard, bio_og_tsdmard,
      legemiddel_gruppert, legemiddel_gruppert_navn,
      Virkestoff
    ), by = "PasientGUID") |>
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

  d_inkl_oppf = bind_rows(d_inkl, d_oppf)


  assign("d_med_vasket", d_med_vasket, envir = .GlobalEnv)
  assign("d_diag_pers", d_diag_pers, envir = .GlobalEnv)
  assign("d_diag_med", d_diag_med, envir = .GlobalEnv)
  assign("d_inkl_oppf", d_inkl_oppf, envir = .GlobalEnv)
  assign("d_inkl", d_inkl, envir = .GlobalEnv)
  assign("d_oppf", d_oppf, envir = .GlobalEnv)
  assign("d_diag", d_diag, envir = .GlobalEnv)
  assign("d_med", d_med, envir = .GlobalEnv)
  assign("d_dodsdato", d_dodsdato, envir = .GlobalEnv)
}



strukturer_variabler_uten_kodebok = function(d_inkl, d_oppf, d_diag, d_med) {
}




# FIXME - utvide til alle skjematyper

# TODO - Skal lage et nytt objekt som inneholder alle pasientforløp.
# - Pasienter kan være registrert ved flere sykehus og dermed få dupliserte
# inklusjonsskjema og diagnoseskjema osv.
# Målet er å redusere dette i den grad det er mulig å prøve å få litt bedre
# organiserte data.
#
# lag_sykdomsforlop = function(d_full_Inklusjonskjema, d_full_Oppfølgingskjema, d_full_Diagnoseskjema, d_full_Medisineringskjema){
#
#   PID = unique(c(d_full_Inklusjonskjema$PasientGUID, d_full_Oppfølgingskjema$PasientGUID, d_full_Diagnoseskjema$PasientGUID, d_full_Medisineringskjema$PasientGUID))
#
#   all(PID %in% unique(d_full_Inklusjonskjema$PasientGUID))
#
#   hovedskjemaGUID = d_full_Inklusjonskjema |> pull(SkjemaGUID)
#
#   nest
# }


#' lag base-data for ra-indikatorer
#'
#' Lager et base-uttrekk for pasienter som skal inkluderes i indikatorer for
#' RA-pasienter.
#' Ytterligere skjema må kobles på for den enkelte indikator.
#'
#' @param d_diag Diagnosedatasett som skal brukes.
#' @param d_inkl Inklusjonsdatasett som skal brukes.
#'
#' @return
#' En tibble med aktuelle variabler fra diagnoseskjema og inklusjonsskjema
#' filtrert for å kun inneholde pasienter som skal inkluderes i RA-indikatorer.
#' @export
#'
#' @examples
lag_ra_indikator_base = function(d_diag, d_inkl) {
  d_ra_base = d_diag |>
    arrange(dato_diag) |>
    distinct(PasientGUID,
      .keep_all = TRUE
    ) |>
    filter(
      diaggrupper_med == 1,
      diag_stilt_aar >= 2014,
    ) |>
    select(PasientGUID, Kode, Navn, dato_diag, diag_stilt_aar, dager_diag_til_datadump) |>
    left_join(select(d_inkl, PasientGUID, UnitId, InklusjonDato, DeathDate),
      by = "PasientGUID",
      relationship = "one-to-one"
    ) |>
    mutate(dager_diag_til_inklusjon = difftime(InklusjonDato, dato_diag, units = "days")) |>
    filter(
      dager_diag_til_inklusjon >= 0,
      dager_diag_til_inklusjon <= 90
    ) |>
    relocate(UnitId, .after = PasientGUID)

  d_ra_base
}
