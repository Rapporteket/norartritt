# Hjelpefunksjoner som brukes i norartritt

#' @importFrom magrittr %>%
#' @importFrom dplyr mutate filter select left_join right_join case_when group_by summarise ungroup arrange slice pull bind_rows
#' @importFrom lubridate as_date year
NULL

#' Legg til medisinnavn
#'
#' Kobler på legemiddelnavn og ekstra informasjon om medisiner registrert
#' i NorArtritt til medisindata.
#'
#' @param d_medisin medisindata fra NorArtritt.
#' Må inneholde kolonnen LegemiddelType.
#'
#' @details
#' På grunn av måten data overføres fra GoTreatIt til registeret er det
#' enkelte medisiner som mangler navn og kode ved innlesning. Disse blir
#' registrert med LegemiddelType 999. Denne funksjonen løser opp disse
#' medisinene og fordeler dem til egne koder for hver medisin.
#'
#' Det finnes to csv-filer som leses inn fra NorArtritt sin kvalitetsserver.
#' En kodebok for alle legemiddel og en for legemiddel kodet
#' som LegemiddelType 999. Ved endringer i hvilke medisiner som registreres
#' er det her vi må oppdatere for å få det inn i analysene.
#'
#' @return
#' Returnerer opprinnelig datasett i tillegg til ekstra informasjon om
#' de ulike medisinene som registreres. Nye variabler som legges til er:
#' * \strong{legemiddel_navn}	Navn på formatet `legemiddelnavn (virkestoff)`.
#' * \strong{legemiddel_navn_kode}	Heltallskode for gruppering av legemiddel_navn.
#' * \strong{biokat}	Indikator for om legemiddel er biologisk. Tar verdien 0 eller 1.
#' * \strong{dmard}	Indikator for om legemiddel er dmard
#' (disease modifying antirheumatic drugs). Tar verdien 0 eller 1.
#' * \strong{csdmard}	Indikator for om legemiddel er csdmard
#' (conventional synthetic disease modifying antirheumatic drugs).
#' Tar verdien 0 eller 1.
#' * \strong{tsdmard}	Indikator for om legemiddel er tsdmard (targeted synthetic disease modifying antirheumatic drugs). Tar verdien 0 eller 1.
#' * \strong{bio_og_tsdmard}	Indikator for om legemiddel er enten biologisk eller tsdmard.
#' Tar verdien 0 eller 1.
#' * \strong{legemiddel_gruppert} Heltallskode for gruppering av enkelte legemidler.
#' * \strong{legemiddel_gruppert_navn}	Navn for legemiddel som brukes i de tilfeller
#' hvor legemiddel er gruppert. For eksempel slås alle de ulike medisinene med
#' virkestoff infliximab sammen til en gruppe.
#' * \strong{Virkestoffnavn} Navn på virkestoff alene.
#'
#' @export
#' @examples
#' # d_medisin er medisindata fra NorArtritt.
#'
#' d_medisin_med_navn = legg_til_medisinnavn(d_medisin)
legg_til_medisinnavn = function(d_medisin) {

  # Leser inn kodebøker for medisiner
  mappe = paste0(***FJERNET ADRESSE***)
  medisin_grupper = "medisin-grupper.csv"
  legemiddel_999 = "legemiddel-kodebok.csv"

  medisin_fil = readr::read_delim(paste0(mappe, medisin_grupper),
    delim = ";",
    trim_ws = TRUE,
    col_types = c("ici__iiiiiiccc"),
    locale = readr::locale(encoding = "windows-1252")
  )

  medisin_kode_999 = readr::read_delim(paste0(mappe, legemiddel_999),
    delim = ";",
    trim_ws = TRUE,
    col_types = c("ic"),
    locale = readr::locale(encoding = "windows-1252")
  )

  # Sjekk at alle LegemiddelTyper i datasettet finnes i medisinkodebok
  na_legemiddel_navn = d_medisin %>%
    filter(!LegemiddelType %in% medisin_fil$LegemiddelType) %>%
    dplyr::pull(LegemiddelType)

  if (length(na_legemiddel_navn) > 0) {
    stop(paste0(
      "LegemiddelType ",
      stringr::str_c(na_legemiddel_navn, collapse = ", "),
      " er ikke definert i medisinkodeboken"
    ))
  }

  # Henter ut navn og riktig kode for medisiner med LegemiddelType 999
  d_medisin = d_medisin %>%
    left_join(medisin_fil %>%
      select(
        LegemiddelType, legemiddel_navn,
        legemiddel_navn_kode
      ),
    by = "LegemiddelType"
    ) %>%
    mutate(medisin = dplyr::coalesce(Legemiddel, legemiddel_navn)) %>%
    left_join(medisin_kode_999, by = c("medisin" = "legemiddel_kodebok")) %>%
    mutate(
      legemiddel_navn_kode =
        case_when(
          !is.na(legemiddel_kodebok_kode) ~ legemiddel_kodebok_kode,
          TRUE ~ legemiddel_navn_kode
        )
    )

  # Legger til ekstra informasjon om hvert legemiddel
  d_medisin = d_medisin %>%
    select(-Legemiddel, - LegemiddelType, -legemiddel_navn,
           -medisin, -legemiddel_kodebok_kode) %>%
    left_join(distinct(medisin_fil, legemiddel_navn_kode, .keep_all = TRUE),
      by = "legemiddel_navn_kode"
    ) %>%
    select(-legemiddelnavn_i_kodebok)

  d_medisin
}

#' Legg til sykehusnavn
#'
#' Kobler på sykehusnavn for bruk i analyser og rapporter.
#'
#' @param d Datasett fra NorArtritt. Må inneholde
#' kolonnen UnitId som tilsvarer RESH-id for de ulike avdelingene.
#'
#' @details
#' NorArtritt har allerede variabler for sykehusnavn og sykehuskortnavn i
#' registeret, men disse variablene er mangelfulle. På grunn av det har vi
#' laget en egen tabell på kvalitetsserver hvor vi har oppdaterte og riktige
#' koblinger mellom UnitId og sykehusnavn. Det gjør det også enklere for oss
#' å gruppere sykehus etter ulike kriterier. Ved endringer i inkluderte sykehus
#' eller RESH-id'er er det denne tabellen som må oppdateres for å få riktige
#' navn i rapporter og analyser.
#'
#' @return
#' Returnerer opprinnelig datasett i tillegg til ekstra informasjon hentet fra
#' sykehustabell på kvalitetsserver. Nye variabler er:
#' * \strong{sykehusnavn} Fullt sykehusnavn.
#' Eksempel Haukeland universitetssykehus.
#' * \strong{sykehus_kortnavn} Forkortet sykehusnavn. Eksempelvis er
#' Haukeland universitetssykehus forkortet til HUS.
#' * \strong{sykehus_gruppe} Heltallskode for gruppering av sykehus. Primært
#' for å samle Avtalespesialister i en gruppe.
#' * \strong{sykehus_gruppe_navn} Navn tilknyttet heltallskode
#' i `sykehus_gruppe`.
#'
#' @export
#' @examples
#' # d er datasett fra NorArtritt som inneholder variabelen UnitId
#'
#' d_med_sykehusnavn = legg_til_sykehusnavn(d)
legg_til_sykehusnavn = function(d) {
  adresse = ***FJERNET ADRESSE***
  sykehus_navnefil = readr::read_delim(adresse,
    delim = ";",
    col_types = c("iccic_"),
    locale = readr::locale(encoding = "windows-1252")
  )

  d = d %>%
    left_join(sykehus_navnefil, by = c("UnitId" = "resh_id"))

  if (any(is.na(d$sykehusnavn))) {
    stop(paste0(
      "Det mangler kobling for UnitId: ",
      stringr::str_c(d %>%
        filter(is.na(sykehusnavn)) %>%
        distinct(UnitId) %>%
        pull(UnitId), collapse = ", ")
    ))
  }
  d
}


#' Legg til diagnosegrupper
#'
#' Legger til ekstra kolonner for diagnoser til diagnosedata fra NorArtritt.
#'
#' @param d Diagnosedatasett fra NorArtritt. Må inneholde variablene Kode og Navn.
#'
#' @details
#' Funksjonen leser inn en diagnosefil fra NorArtritts kvalitetsserver.
#' Denne filen inneholder koblinger for alle diagnosene som er brukt i analyser
#' for NorArtritt, i tillegg til variabler for å gruppere diagnoser
#' etter ulike kriterier. For eksempel er kodene M058, M059 og M060 samlet til
#' en gruppe 'Revmatoid Artritt'.
#' 'Leddsykdom' legges også i riktig grupper
#' selv om diagnosen ikke har en egen kode i registeret.
#' Ved endringer i hvilke diagnoser som inkluderes i
#' analyser er det i denne filen vi må oppdatere koblingene.
#'
#' @return
#' Returnerer inndata men med ekstra variabler med utbedret diagnoseinformasjon.
#' Nye variabler er:
#' * \strong{diaggrupper_med} Heltallskode for de ulike diagnosegruppene. Det er
#' vanligvis denne inndelingen som brukes i NorArtritts analyser og rapporter.
#' Delt inn i 8 diagnosegrupper.
#' * \strong{diaggrupper_med_tekst} Diagnosenavn tilhørende de ulike
#' heltallskodene i `diaggrupper_med`.
#' * \strong{diaggrupper_rem} Heltallskode for inndeling i diagnosegrupper brukt
#' i sammenheng med beregning av remisjon. Disse måles på ulike måter, angitt i
#' variabelen `rem_maal`.
#' * \strong{diaggrupper_rem_tekst} Diagnosenavn tilknyttet heltallskodene
#' angitt i `diaggrupper_rem`.
#' * \strong{rem_maal} Navn for remisjonsmål knyttet heltallskoden i
#' `diaggrupper_rem`. Er enten DAS28-CRP eller ASDAS-CRP avhengig av diagnosen.
#' * \strong{diaggrupper_hoved} Heltallskode for en alternativ diagnoseinndeling,
#' hvor diagnosene er delt i 5 grupper.
#' * \strong{diaggrupper_hoved_tekst} Diagnosenavn tilknyttet kodene angitt i
#' `diaggrupper_hoved`.
#' * \strong{perifer_aksial_diaggruppe} Heltallskode for en diagnoseinndeling
#' for perifer aksial artritt. Delt inn i 3 grupper.
#' * \strong{perifer_aksial_diaggruppe_tekst} Gruppenavn for diagnosene delt etter
#' heltallskoden i `perifer_aksial_diaggruppe`.
#'
#' @export
#' @examples
#' # d_diagnose er diagnosedata fra NorArtritt
#' d_med_diagnosedata = legg_til_diagnosegrupper(d_diagnose)
legg_til_diagnosegrupper = function(d) {
  adresse = ***FJERNET ADRESSE***
  diagnosegrupper = readr::read_delim(adresse,
    delim = ";",
    col_types = c("ciciccicic"),
    locale = readr::locale(encoding = "windows-1252")
  )

  ukjent_kode = setdiff(d$Kode, diagnosegrupper$Kode)
  ukjent_kode = ukjent_kode[!is.na(ukjent_kode)]
  if (length(ukjent_kode) > 0) {
    stop(paste0("Kode: ", stringr::str_c(ukjent_kode, collapse = ", "), " finnes ikke i diagnosekodebok"))
  }

  d = d %>%
    left_join(diagnosegrupper, by = "Kode")

  # Leddsykdom har ikke kode i registeret, så den må håndteres manuelt.
  d = d %>%
    mutate(
      diaggrupper_med =
        replace(
          diaggrupper_med,
          Navn == "Leddsykdom", 8L
        ),
      diaggrupper_med_tekst =
        replace(
          diaggrupper_med_tekst,
          Navn == "Leddsykdom",
          "Andre kroniske artritter"
        ),
      diaggrupper_hoved =
        replace(
          diaggrupper_hoved,
          Navn == "Leddsykdom", 5L
        ),
      diaggrupper_hoved_tekst =
        replace(
          diaggrupper_hoved_tekst,
          Navn == "Leddsykdom",
          "Andre kroniske artritter"
        ),
      perifer_aksial_diaggruppe =
        replace(
          perifer_aksial_diaggruppe,
          Navn == "Leddsykdom", 3L
        ),
      perifer_aksial_diaggruppe_tekst =
        replace(
          perifer_aksial_diaggruppe_tekst,
          Navn == "Leddsykdom",
          "Udifferensiert"
        )
    )

  d
}



#' Velg tidligste inklusjondato
#'
#' Velger den tidligste inklusjonsdatoen for hver pasient oppgitt på
#' inklusjons- og oppfølgingsskjema i NorArtritt.
#'
#' @details
#' Inklusjons- og oppfølgingsskjema i NorArtritt har en variabel kalt
#' `InklusjonDato` som angir hvilken dato pasienten først ble inkludert i
#' registeret. Denne datoen skulle i realiteten vært identisk for alle
#' oppføringer for en pasient, men det er ikke tilfellet. Denne funksjonen
#' velger ut den tidligste registrerte `InklusjonDato` for hver pasient og
#' skriver over alle senere/manglende datoer i andre skjema.
#' Hvis en pasient aldri har fått registrert inklusjonsdato returneres NA for
#' denne pasienten.
#'
#' @param pas_id Variabel som brukes som pasientidentifikator. Standard verdi
#' er `PasientGUID`, men for personidentifiserbare datadumper kan
#' `Fødselsnummer` brukes.
#' @param d_inkl_oppf Sammenkoblet datasett som inneholder alle aktuelle
#' inklusjons- og oppfølgingsskjema for pasientgruppen.
#'
#' @return
#' Returnerer inndata med oppdatert `InklusjonDato` for alle pasienter.
#' @export
#'
#' @examples
#' # d_inkl_oppf er sammenslått datasett med inklusjons- og oppfølgingsskjema
#' d_inkl_oppf_dato = velg_tidligste_inklusjondato(d_inkl_oppf)
velg_tidligste_inklusjondato = function(pas_id = PasientGUID, d_inkl_oppf) {
  min_na = function(x) {
    if (all(is.na(x))) {
      x[NA]
    } else {
      min(x, na.rm = TRUE)
    }
  }

  d = d_inkl_oppf %>%
    group_by({{ pas_id }}) %>%
    mutate(InklusjonDato = as.Date(min_na(InklusjonDato))) %>%
    filter(!is.na(InklusjonDato)) %>%
    ungroup()

  d
}

#' Valider legemiddeltype
#'
#' Validerer legemiddeltype i medisinfil mot kodebok. Hvis HEMIT gjør endringer i
#' hvordan legemiddeltyper er kodet, eller legger til nye legemiddel i hovedlisten
#' fra de som per nå er kodet som LegemiddelType = 999 vil denne funksjonen
#' stanse innlesningen av medisinskjema og gi returnere en feilmelding.
#'
#' @param mappe_dd Filsti til hvor kodebok er lagret.
#'
#' @return
#' Returnerer en feilmelding hvis legemiddeltyper i kodebok er endret.
#'
#' @export
#'
#' @examples
#' # mappe_dd er plassering for kodebok som skal sjekkes.
#'
#' valider_legemiddeltype(mappe_dd)
valider_legemiddeltype = function(mappe_dd) {
  kb = les_kb_mrs(mappe_dd)

  kb_legemiddeltype = kb %>%
    filter(
      skjema_id == "Medisineringskjema",
      variabel_id == "LegemiddelType"
    ) %>%
    select(verdi, verditekst)

  mappe = paste0(***FJERNET ADRESSE***)
  medisin_grupper = "medisin-grupper.csv"

  medisinfil = readr::read_delim(paste0(mappe, medisin_grupper),
    delim = ";",
    trim_ws = TRUE,
    col_types = c("ici__iiiiiiccc"),
    locale = readr::locale(encoding = "windows-1252")
  )

  medisin_uttrekk = medisinfil %>%
    select("verdi" = LegemiddelType, "verditekst" = legemiddelnavn_i_kodebok) %>%
    mutate(verdi = as.character(verdi))

  legemiddel_feil = setdiff(kb_legemiddel_type, medisin_uttrekk)

  if (nrow(legemiddel_feil) > 0) {
    stop(stringr::str_c("Det er ikke samsvar mellom legemiddelnavn i kodebok og vår medisinfil
         for legemiddeltype: ", legemiddel_feil %>% pull(verdi), collapse = ", "))
  }
}


#' Konverter skjematyper
#'
#' `konverter_skjematype` håndterer manglende eller duplikate skjema for
#' pasienter i NorArtritt.
#' Alle pasienter *skal* i teorien ha ett inklusjonsskjema, og et ubegrenset
#' antall oppfølgingsskjema. I virkeligheten er det dessverre ikke slik, og det
#' finnes pasienter med flere inklusjonsskjema og pasienter uten inklusjonsskjema.
#'
#' Overflødige inklusjonsskjema konverteres til oppfølgingsskjema
#' (det tidligste inklusjonsskjema beholdes som inklusjonsskjema),
#' og hvis en pasient mangler inklusjonsskjema vil det tidligste oppfølgingsskjemaet
#' konverteres til inklusjonsskjema.
#'
#' @param inkl datasett med alle inklusjonsskjema for NorArtritt. Må minimum
#' inneholde variablene PasientGUID, Skjematype, FormTypeId og SkjemaGUID.
#' @param oppf datasett med alle oppfølgingskskjema for NorArtritt. Må minimum
#' inneholde variablene PasientGUID, Skjematype, FormTypeId og SkjemaGUID.
#'
#' @return
#' Returnerer en tibble med alle inklusjons- og oppfølgingsskjema samlet.
#' Dette vil videre filtreres og utbedres i påfølgende funksjoner før det til
#' slutt returneres som opprinnelige datasett hvor inklusjon og oppfølging er
#' separert.
#'
#' @export
#'
#' @examples
#' # Leser inn data for NorArtritt
#' norartritt::les_data_norartritt()
#' d_inkl_full_rettet = konverter_skjematype(
#'   inkl = d_full_Inklusjonsskjema,
#'   oppf = d_full_Oppfølgingsskjema
#' )
konverter_skjematype = function(inkl, oppf) {

  # ID for pasienter med skjema som skal konverteres
  id_oppf_uten_inkl = setdiff(oppf$PasientGUID, inkl$PasientGUID)
  id_flere_inklusjon = inkl$PasientGUID[which(duplicated(inkl$PasientGUID))]

  # Slå sammen inklusjon og oppfølgingsskjema
  d_inkl_oppf = inkl %>%
    bind_rows(oppf)

  # Trekke ut skjemaGUID for skjema som skal konverteres
  inkl_til_oppf_skjemaGUID = d_inkl_oppf %>%
    filter(
      PasientGUID %in% id_flere_inklusjon,
      FormTypeId == 1L
    ) %>%
    group_by(PasientGUID) %>%
    arrange(FormDate) %>%
    slice(-1) %>%
    pull(SkjemaGUID)

  oppf_til_inkl_skjemaGUID = d_inkl_oppf %>%
    filter(
      PasientGUID %in% id_oppf_uten_inkl,
      FormTypeId == 2L
    ) %>%
    group_by(PasientGUID) %>%
    arrange(FormDate) %>%
    slice(1) %>%
    pull(SkjemaGUID)

  # Konvertere til riktig skjematype
  d_inkl_oppf = d_inkl_oppf %>%
    mutate(
      Skjematype = case_when(
        SkjemaGUID %in% inkl_til_oppf_skjemaGUID ~ "Oppfølgingskjema",
        SkjemaGUID %in% oppf_til_inkl_skjemaGUID ~ "Inklusjonskjema",
        TRUE ~ Skjematype
      ),
      FormTypeId = case_when(
        SkjemaGUID %in% inkl_til_oppf_skjemaGUID ~ 2,
        SkjemaGUID %in% oppf_til_inkl_skjemaGUID ~ 1,
        TRUE ~ FormTypeId
      )
    ) %>%
    arrange(FormTypeId)

  d_inkl_oppf
}

#' Fjern ugyldige skjema
#'
#' Fjerner skjema i de ulike datasettene hvis pasienten mangler en nødvendig
#' registrering av et annet skjema.
#' For å inkluderes i analysene *må* pasienten ha både inklusjons- og
#' diagnoseskjema.
#' Denne funksjonen fjerner skjema i følgende rekkefølge:
#' Diagnoseskjema for pasienter som mangler inklusjonsskjema.
#' Inklusjonsskjema for pasienter som mangler diagnoseskjema.
#' Oppfølgingsskjema for pasienter som mangler diagnoseskjema.
#' Medisineringsskjema for pasienter som mangler diagnoseskjema.
#'
#' @param inkl Datasett med inklusjonsskjema for NorArtritt.
#' @param oppf Datasett med oppfølgingsskjema for NorArtritt.
#' @param med Datasett med medisinskjema for NorArtritt.
#' @param diag Datasett med diagnoseskjema for NorArtritt.
#'
#' @return
#' Returnerer filtrerte objekter for inndata med "_filtrert" som suffix.
#'
#' @export
#'
#' @examples
#' # leser inn data for NorArtritt
#' library(norartritt)
#' les_data_norartritt()
#' fjern_ugyldige_skjema(
#'   inkl = d_full_Inklusjonskjema,
#'   oppf = d_full_Oppfølgingskjema,
#'   med = d_full_Medisineringskjema,
#'   diag = d_full_Diagnoseskjema
#' )
fjern_ugyldige_skjema = function(inkl, oppf, med, diag) {
  diag = fjern_uaktuelle_diagnoser(diag)

  diag_filtrert = fjerne_skjema_hjelpefunksjon(d_hoved = inkl, d_motpart = diag)
  inkl_filtrert = fjerne_skjema_hjelpefunksjon(d_hoved = diag, d_motpart = inkl)
  oppf_filtrert = fjerne_skjema_hjelpefunksjon(d_hoved = diag, d_motpart = oppf)
  med_filtrert = fjerne_skjema_hjelpefunksjon(d_hoved = diag, d_motpart = med)

  # Returnere alle objekter
  (list(inkl_filtrert, oppf_filtrert, diag_filtrert, med_filtrert))
}

#' Fjern uaktuelle diagnoser
#'
#' Fjerner et utvalg uaktuelle diagnoser som er registrert i NorArtritt.
#' Disse er ikke av interesse for analyser, og hvis pasientene kun har en av
#' disse diagnosene oppfyller de ikke inklusjonskriteriene for registeret.
#'
#' @param diag Diagnosedatasett fra NorArtritt.
#'
#' @return
#' Returnerer diagnosedatasett hvor diagnosene
#' Artrose, Juvenil Idiopatisk Artritt (JIA), Kondrokalsinose,
#' Krystallartritter, Pyogen Artritt, Urinsyregikt, Septisk Artritt
#' og Polyartritt er filtrert bort.
#'
#' @export
#'
#' @examples
#' # Leser inn data fra NorArtritt
#' library(norartritt)
#' les_data_norartritt()
#' diagnosedata_filtrert = fjern_uaktuelle_diagnoser(d_full_Diagnoseskjema)
fjern_uaktuelle_diagnoser = function(diag) {
  # uaktuelle diagnoser----------
  uakt_diag = c(
    "Artrose", "Juvenil Idiopatisk Artritt (JIA)", "Kondrokalsinose",
    "Krystallartritter", "Pyogen Artritt", "Urinsyregikt", "Septisk Artritt"
  )
  uakt_koder = c("M130", "M131", "M138", "M139") # Polyartritt

  # Fjerner diagnoseskjema for pasienter som mangler andre diagnoser enn de overnevnte
  diag = diag %>%
    filter(!Navn %in% !!uakt_diag, !Kode %in% !!uakt_koder)

  diag
}

#' Hjelpefunksjon for fjerning av skjema
#'
#' Hjelpefunksjon for fjerning av ugyldige skjema. Sjekker om det finnes
#' Pasient-ID i d_motpart som ikke finnes i d_hoved.
#' Brukes for eksempel for å fjerne inklusjonsskjema for pasienter som ikke har
#' diagnoseskjema.
#'
#' @param d_hoved datasett med skjema som inneholder variabelen PasientGUID
#' d_motpart skal kontrolleres mot.
#' @param d_motpart datasett med skjema som eventuelt skal filtreres bort.
#' Må inneholde variabelen PasientGUID.
#'
#' @return
#' Returnerer filtrert versjon av d_motpart, hvor ID'er som ikke finnes i
#' d_hoved er filtrert bort.
fjerne_skjema_hjelpefunksjon = function(d_hoved, d_motpart) {
  id_mangler_i_hoved = setdiff(d_motpart$PasientGUID, d_hoved$PasientGUID)
  d = d_motpart %>%
    filter(!PasientGUID %in% id_mangler_i_hoved)
  d
}


#' Legger til ekstra datovariabler
#'
#' Legger til ekstra datovariabler til skjema i NorArtritt for å forenkle
#' analyser.
#' For inklusjon- og oppfølgingsskjema legges det til:
#' * \strong{aar_ktrl} Årstall for inklusjon i registeret.
#' * \strong{dato_ktrl} Dato for inklusjon i registeret.
#' For medisinskjema legges det til:
#' * \strong{startaar} År for oppstart medisin.
#' * \strong{sluttaar} År for avslutning av medisin.
#' * \strong{StartDato} Dato for oppstart medisin.
#' * \strong{SluttDato} Dato for avslutning av medisin.
#'
#' @param d_inkl inklusjonsskjema fra NorArtritt.
#' @param d_oppf Oppfølgingsskjema fra NorArtritt.
#' @param d_med Medisinskjema fra NorArtritt.
#' @param d_diag Diagnoseskjema fra NorArtritt.
#'
#' @return
#' Returnerer en liste med de fire skjematypene i NorArtritt hvor ekstra
#' datovariabler er lagt til.
#'
#' @export
#'
#' @examples
#' # leser inn data for norartritt:
#' library(norartritt)
#' les_data_norartritt()
#' legg_til_datovariabler(
#'   d_inkl = d_full_Inklusjonsskjema,
#'   d_oppf = d_full_Oppfølgingsskjema,
#'   d_med = d_full_Medisinskjema,
#'   d_diag = d_full_Diagnoseskjema
#' )
legg_til_datovariabler = function(d_inkl, d_oppf, d_med, d_diag) {
  d_inkl = d_inkl %>%
    mutate(
      aar_ktrl = year(InklusjonDato),
      dato_ktrl = as_date(InklusjonDato)
    )

  d_oppf = d_oppf %>%
    mutate(
      aar_ktrl = year(FormDate),
      dato_ktrl = as_date(FormDate)
    )

  d_med = d_med %>%
    mutate(
      startaar = year(StartDato),
      sluttaar = year(SluttDato),
      StartDato = as_date(StartDato),
      SluttDato = as_date(SluttDato)
    )

  d_diag = d_diag %>%
    mutate(
      dato_diag = as_date(FormDate),
      diag_stilt_aar = year(FormDate),
      dager_diag_til_datadump = as.integer(!!datadump_dato - dato_diag)
    )

  # Returnere alle objekter
  (list(d_inkl, d_oppf, d_diag, d_med))
}


# legg_til_dager_diag_til_datadump = function() {

# }

#' Konverter missing til na
#'
#' @description
#' Konverterer manglende besvarelser til NA for variablene
#' `BASDAI`, `Das28`, `Das283`, `Das28Crp`, `Das28Crp3`,
#' `Cdai`, `Sdai`, `Asdas`, `DAPSA` og `Total`. Disse variablene er numeriske
#' verdier og manglende besvarelser er kodet som henholdsvis 0 for `BASDAI` og
#' -1 ellers i datasettet. Disse konverteres her til `NA_real_`.
#' `vask_data_norartritt()` kaller på denne funksjonen når
#' `lag_filtrerte_objekter()` kjøres.
#'
#' @param d datasett med disse variablene. `inklusjonsskjema` eller
#' `oppfølgingsskjema`.
#'
#' @return
#' Returnerer opprinnelig datasett hvor manglende besvarelser er erstattet med
#' `NA_real_`
#' @export
#'
#' @examples
#' d_inkl = konverter_missing_til_na(d_inkl)
konverter_missing_til_na = function(d) {

  vars_fra_0_real = c("BASDAI")
  vars_fra_1_real = c("Das28", "Das283", "Das28Crp", "Das28Crp3", "Cdai",
                 "Sdai", "Asdas", "DAPSA", "Total")
  vars_fra_1_int = c("KlePaaSelv", "OppISengen", "LofteKopp",
                 "Utendors", "Vaske", "Boye", "Skru", "KommeInn")

  d = d |>
    mutate(across(all_of(vars_fra_0_real),
                  \(x) replace(x, x == 0, NA_real_)),
           across(all_of(vars_fra_1_real),
                  \(x) replace(x, x == -1, NA_real_)),
           across(all_of(vars_fra_1_int),
                  \(x) replace(x, x == -1, NA_integer_))
           )

}

