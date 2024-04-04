# Kvalitetsindikatorer for NorArtritt

#' @importFrom magrittr %>%
#' @importFrom dplyr filter group_by ungroup arrange desc distinct n_distinct select left_join mutate pull
#' @importFrom lubridate as_date ymd days
NULL
#'
#' Kvalitetsindikator for tidlig oppstart av behandling
#'
#' @description
#' Lager KI-datasett for om en pasient fikk tidlig oppstart av behandling.
#'
#' @details
#' Indikatoren viser andelen pasienter som får sykdomsmodifiserende medisin
#' innen 2 uker av diagnosedato.
#' Det er en del begrensninger for hvilke pasienter som skal inkluderes.
#' For å inkluderes i nevner må følgende kriterier være oppfylt:
#' * Pasienten må ha Revmatoid Artritt som eneste registrerte diagnose.
#' * Pasienten må være diagnostisert i 2014 eller senere.
#' * Tid fra diagnose til inklusjon i registeret må være under eller lik 90 dager.
#'
#' For å inkluderes i teller må de da i tillegg ha begynt på sykdomsmodifiserende
#' medisin innen 2 uker av diagnosedato.
#'
#' @param d_inklusjon Inklusjonsdatasett
#' @param d_diagnose Diagnosedatasett
#' @param d_medisin Medisindatasett
#'
#' @return
#' Returnerer en tibble med følgende variabler:
#' * \strong{PasientGUID} PasientID
#' * \strong{ki_krit_teller} Indikator for om pasienten oppfyller kriterier for teller.
#' Kan ta verdiene TRUE, FALSE.
#' * \strong{ki_krit_nevner} Indikator for om pasienten oppfyller kriterier for nevner.
#' Kan ta verdiene TRUE eller FALSE.
#' * \strong{...} Eventuelle grupperingsvariabler blir også med i utdata.
#'
#' Pasientene kan ha flere rader hvor ki_krit_teller har ulike besvarelse. For
#' eksempel kan en pasient ha startet på to medisiner innen 2 uker, men kun
#' en av medisinene er en sykdomsmodifiserende medisin. Utdata er da filtrert
#' slik at rader hvor ki_krit_nevner og ki_krit_teller er TRUE er prioritert.
#'
#' @export
#'
#' @examples
#' # d_inkl, d_diag og d_medisin er henholdsvis
#' # inklusjonsdata, diagnosedata og medisindata for NorArtitt.
#' d_ki_sykmod = ki_sykmod(d_inkl, d_diag, d_medisin)
ki_sykmod = function(d_inklusjon, d_diagnose, d_medisin) {

  # FIXME - Vurder behov for å lage nye dataobjekter for å forenkle
  # kobling i diverse figurer/beregninger.
  # FIXME - Oppdatere bruk av sykehusnavn og legemiddel_navn.
  # Må enten bruke nye funksjonene for å hente navn, eller ta utgangspunkt i at
  # det gjøres i preprosseseringsfunksjon (som ikke er laget ferdig enda).
  # FIXME - Fjerne avhengighet av PasientGUID (må kunne ta inn fødselsnummer også).
  # FIXME - Sjekke at jeg ikke importerer flere funksjoner en nødvendig i toppen


# Koble på inklusjonsskjema for å hente ut inklusjonstidspunkt og sykehustilhørighet
  d_inkl_diag_med = d_diagnose %>%
    arrange(dato_diag) |>
    distinct(PasientGUID, .keep_all = TRUE) |>
    left_join(d_inklusjon %>%
      select(
        PasientGUID, InklusjonDato,
        sykehusnavn, sykehus_kortnavn
      ),
    by = "PasientGUID",
    relationship = "one-to-one") %>%
    left_join(d_medisin %>%
      select(
        PasientGUID, StartDato, SluttDato,
        legemiddel_navn, legemiddel_navn_kode, dmard
      ),
    by = "PasientGUID",
    relationship = "one-to-many") %>%
    mutate(
      dager_fra_diag_til_inkl =
        as.numeric(as_date(InklusjonDato) - as_date(dato_diag))
    )

  # Lage ki_krit_nevner
  d_ki_med_krit_nevner = d_inkl_diag_med  |>
    mutate(ki_krit_nevner = diaggrupper_med == 1 &
             diag_stilt_aar >= 2014 &
             dager_fra_diag_til_inkl >= 0 &
             dager_fra_diag_til_inkl <= 90 &
             (DeathDate >= dato_diag + days(14) | is.na(DeathDate)))

  # Legge til teller og reduserer til en rad per pasient
  d_ki = d_ki_med_krit_nevner %>%
    mutate(
      tid_til_oppstart_medisin = as_date(StartDato) - as_date(dato_diag),
      ki_krit_teller =
        case_when(is.na(tid_til_oppstart_medisin) ~ FALSE,
                  ki_krit_nevner &
                    dmard == 1 &
                    tid_til_oppstart_medisin >= 0 &
                    tid_til_oppstart_medisin <= 14 ~ TRUE,
                  TRUE ~ FALSE)

    ) %>%
    group_by(PasientGUID) %>%
    arrange(desc(ki_krit_teller), desc(ki_krit_nevner), .by_group = TRUE) %>%
    distinct(PasientGUID, .keep_all = TRUE) %>%
    ungroup()

  d_ki
}

#' Kvalitetsindikator for andel pasienter som bruker en medisin
#'
#' @description
#' Indikatoren viser andelen pasienter som bruker en gitt medisin i løpet av
#' et år.
#'
#' @param d_diagnose Diagnosedatasett
#' @param d_medisin Medisindatasett
#' @param aarstall Årstallet indikatoren skal beregnes for
#' @param legemiddel En heltallsvektor med legemiddel_navn_kode for hvilke
#' medisiner som skal inkluderes.
#'
#' @return
#' Returnerer en tibble med følgende variabler:
#' * \strong{PasientGUID} PasientID
#' * \strong{ki_krit_teller} Indikator for om pasienten oppfyller
#' kriterier for teller. Kan ta verdiene TRUE, FALSE.
#' * \strong{ki_krit_nevner} Indikator for om pasienten oppfyller
#' kriterier for nevner. Kan ta verdiene TRUE eller FALSE.
#' * \strong{...} Eventuelle grupperingsvariabler blir også med i utdata.
#' @export
#'
#' @examples
#' # d_diag og d_medisin er diagnosedata og medisindata
#' d_ki_medisinbruk = ki_medisinbruk(d_diag, d_medisin, 2020, 20)
ki_medisinbruk = function(d_diagnose, d_medisin, aarstall, legemiddel) {

  # FIXME - Legge inn støtte for å definere hvilke diagnosegrupper det skal
  # beregnes KI for.
  # FIXME - Ta ut årstall fra argumentlisten. Tilrettelegg for at det kan løses
  # vha gruppering av inndata og filtrering etterpå.
  # FIXME - Håndtering av legemiddel_navn (kreve preprosessert data / hente
  # vha legg_til_medisingrupper).
  # FIXME - Fjerne avhengighet av PasientGUID (må kunne ta inn fødselsnummer også).
  d_ki = d_diagnose %>%
  left_join(
    d_medisin %>%
      select(
        PasientGUID, StartDato,
        SluttDato, startaar, sluttaar, legemiddel_navn,
        legemiddel_navn_kode
      ),
    by = "PasientGUID") %>%
  mutate(ki_krit_nevner = diaggrupper_med == 1 &
    diag_stilt_aar <= aarstall &
    (year(DeathDate) >= aarstall | is.na(DeathDate)))

  d_ki_med_krit = d_ki %>%
    filter(ki_krit_nevner) %>%
    mutate(ki_krit_teller = ki_krit_nevner &
      legemiddel_navn_kode %in% legemiddel &
      !is.na(legemiddel_navn_kode) &
      !is.na(startaar) &
      startaar <= aarstall &
      (sluttaar >= aarstall | is.na(SluttDato))) %>%
    group_by(PasientGUID) %>%
    arrange(desc(ki_krit_teller), desc(ki_krit_nevner), .by_group = TRUE) %>%
    distinct(PasientGUID, .keep_all = TRUE) %>%
    ungroup()

  d_ki_med_krit
}

#' Andel RA-pasienter som oppnår remisjon 1 år etter diagnose.
#'
#' @description
#' Indikatoren viser andelen pasienter med RA som har oppnådd remisjon 1 år
#' etter diagnosedato.
#'
#' @details
#' `tisdrom_start` og `tidsrom_slutt` indikerer start og slutt for hva som skal
#' regnes som 1-års kontroll i NorArtritt da det ikke er en fast kontroll etter
#' 1 år. Pasienten må ha RA som siste diagnose, stilt i 2014 eller senere.
#' Pasienten må ha hatt diagnosen i minst 1 år ved datadumpdato, og ha vært
#' til kontroll i tidsrommet avgrenset med `tidsrom_start` og `tidsrom_slutt`.
#' I tillegg kan ingen av variablene for ACR-EULAR målingene være missing.
#' For å inkluderes i teller må også følgende krav være oppfylt for ACR-EULAR:
#' * 1 eller færre ømme ledd.
#' * 1 eller færre hovne ledd.
#' * Crp <= 10.
#' * Globalsykdomsaktivitet <= 10.
#'
#' @param d_diag Diagnosedatasett.
#' @param d_inkl_oppf Sammenslått inklusjon- og oppfølgingsdatasett.
#' @param tidsrom_start Nedre grense av tidsrom for beregning av hva som er 1-års
#' kontroll. Antall dager etter diagnose.
#' @param tidsrom_slutt Øvre grense av tidsrom for beregning av hva som er 1-års
#' kontroll. Antall dager etter diagnose.
#'
#' @return
#' Returnerer et datasett med følgende variabler:
#' * \strong{PasientGUID} Pasientidentifikator.
#' * \strong{ki_krit_teller} Indikator for om pasienten oppfyller
#' kriterier for teller. Kan ta verdiene TRUE, FALSE.
#' * \strong{ki_krit_nevner} Indikator for om pasienten oppfyller
#' kriterier for nevner. Kan ta verdiene TRUE eller FALSE.
#' * \strong{...} Eventuelle grupperingsvariabler blir også med i utdata.
#'
#' @export
#'
#' @examples
#' # d_diag og d_inkl_oppf er diagnose og inklusjon/oppfølgingsdata fra NorArtritt.
#' d_ki_remisjon = ki_remisjon(d_diag, d_inkl_oppf)
ki_remisjon = function(d_diag, d_inkl_oppf, tidsrom_start = 180, tidsrom_slutt = 485) {

  # FIXME - Dato for datadump må hentes dynamisk!
  # FIXME - Legge inn støtte for grupperingsvariabler.
  # FIXME - Validering og håndtering av variabler med "mrs_" prefiks.

  # Henter ut id til pasientene som oppfyller kriterier for diagnose og diagnosetidspunkt
  id_diagnose = d_diag %>%
    select(PasientGUID, diaggrupper_med, dato_diag, diag_stilt_aar, dager_diag_til_datadump) %>%
    arrange(dato_diag) %>%
    distinct(PasientGUID, .keep_all = TRUE) %>%
    filter(
      diaggrupper_med == 1,
      diag_stilt_aar >= 2014,
      dager_diag_til_datadump >= 365
    ) %>%
    pull(PasientGUID)

  # Kobler diagnosedata med skjema for de ulike kontrollene for hver pasient i utvalget.
  d = d_inkl_oppf %>%
    left_join(d_diag %>% select(
      PasientGUID, diaggrupper_med, dato_diag,
      dager_diag_til_datadump, diag_stilt_aar
    ), by = "PasientGUID",
    relationship = "many-to-one") %>%
    mutate(dager_siden_diagnose = ymd(dato_ktrl) - ymd(dato_diag)) %>%
    mutate(ki_krit_nevner = PasientGUID %in% id_diagnose &
      dager_siden_diagnose >= tidsrom_start &
      dager_siden_diagnose <= tidsrom_slutt &
      !(is.na(OmmeLeddAntall)) &
      !(is.na(HovneLeddAntall)) &
      !(is.na(Crp)) &
      !(is.na(PasientGlobalSykdomsaktivitet)) &
        (DeathDate >= dato_diag + days(tidsrom_slutt) | is.na(DeathDate)))

  # Finner hvem som oppfyller krav for teller og reduserer til en rad per pasient.
  d_ki_rem = d %>%
    mutate(ki_krit_teller = ki_krit_nevner &
      OmmeLeddAntall <= 1 &
      HovneLeddAntall <= 1 &
      Crp <= 10 & !is.na(Crp) &
      PasientGlobalSykdomsaktivitet <= 10 &
      !is.na(PasientGlobalSykdomsaktivitet)) %>%
    group_by(PasientGUID) %>%
    arrange(desc(ki_krit_teller), desc(ki_krit_nevner), .by_group = TRUE) %>%
    distinct(PasientGUID, .keep_all = TRUE) %>%
    ungroup()

  d_ki_rem
}

#' Remisjon totalt
#'
#' @description
#' Regner ut total andel pasienter som har oppnådd remisjon.
#'
#' @details
#' Funksjonen beregner andelen av RA-pasientene som har oppnådd remisjon.
#' Ser kun på pasienter som har RA som siste diagnose.
#'
#' @param d_diag Diagnosedatasett
#' @param d_inkl_oppf Sammenkoblet inklusjon- og oppfølgingsdatasett
#'
#' @return
#' Returnerer en tibble med følgende variabler:
#' * \strong{PasientGUID} Pasientidentifikator.
#' * \strong{ki_krit_teller} Indikator for om pasienten oppfyller
#' kriterier for teller. Kan ta verdiene TRUE, FALSE.
#' * \strong{ki_krit_nevner} Indikator for om pasienten oppfyller
#' kriterier for nevner. Kan ta verdiene TRUE eller FALSE.
#' * \strong{...} Eventuelle grupperingsvariabler.
#' @export
#'
#' @examples
#' # d_diag og d_inkl_oppf er diagnose og sammenslått
#' # inklusjon og oppfølgingsdatasett
#' d_remisjon_totalt = remisjon_totalt(d_diag, d_inkl_oppf)
remisjon_totalt = function(d_diag, d_inkl_oppf) {
  # FIXME - Legge inn støtte for grupperingsvariabler.

  # fixme (QA): Funksjonen liknar veldig på ki_remisjon(). Er nok
  #             mykje betre/sikrare/enklare om éin av funksjonane er
  #             spesialtilfelle av den andre (eks. med argument
  #             remisjon_innan, som kan vera 365 eller Inf).
  #             Alt som gjeld førre funksjon gjeld òg denne, så
  #             eg skriv det ikkje om igjen.

  # Henter ut id til pasientene som oppfyller kriterier for diagnose
  id_diagnose = d_diag %>%
    select(PasientGUID, diaggrupper_med, dato_diag, dager_diag_til_datadump) %>%
    arrange(dato_diag) %>%
    distinct(PasientGUID, .keep_all = TRUE) %>%
    filter(diaggrupper_med == 1) %>%
    pull(PasientGUID)

  d = d_inkl_oppf %>%
    left_join(d_diag %>% select(
      PasientGUID, diaggrupper_med, dato_diag,
      dager_diag_til_datadump, diag_stilt_aar
    ), by = "PasientGUID") %>%
    mutate(ki_krit_nevner = ifelse(PasientGUID %in% id_diagnose,
      yes = TRUE, no = FALSE
    )) # fixme (QA): Igjen tullete bruk av ifelse().

  d_rem_totalt = d %>%
    mutate(ki_krit_teller = ifelse(ki_krit_nevner &
      OmmeLeddAntall <= 1 &
      HovneLeddAntall <= 1 &
      Crp <= 10 & !is.na(Crp) &
      PasientGlobalSykdomsaktivitet <= 10 &
      !is.na(PasientGlobalSykdomsaktivitet),
    yes = TRUE, no = FALSE
    )) %>%
    group_by(PasientGUID) %>%
    arrange(desc(ki_krit_teller), desc(ki_krit_nevner), .by_group = TRUE) %>%
    distinct(PasientGUID, .keep_all = TRUE) %>%
    ungroup()

  d_rem_totalt
}

#' Indikator for andel pasienter som er til kontroll innen 3 mnd
#'
#' @description
#' Indikatoren viser andelen av pasientene diagnostisert med Revmatoid Artritt
#' som siste diagnose i 2014 eller senere, som får første oppfølging
#' innen 3 mnd (90 dager) av diagnosedato.
#'
#' @details
#' For pasienter som har inklusjonsskjema 7-90 dager etter diagnose regnes
#' inklusjonsskjema som første kontroll.
#'
#'
#' @param d_inkl_oppf Sammenkoblet inklusjon- og oppfølgingsdatasett
#' @param d_diag Diagnosedatasett
#'
#' @return
#' Returnerer en tibble med følgende variabler:
#' * \strong{PasientGUID} Pasientidentifikator
#' * \strong{ki_krit_teller} Indikator for om pasienten oppfyller
#' kriterier for teller. Kan ta verdiene TRUE, FALSE.
#' * \strong{ki_krit_nevner} Indikator for om pasienten oppfyller
#' kriterier for nevner. Kan ta verdiene TRUE eller FALSE.
#' * \strong{...} Eventuelle grupperingsvariabler
#' @export
#'
#' @examples
#' # d_inkl_oppf er sammenkoblet inklusjons- og oppfølgingsdatasett og
#' # d_diag er diagnosedatasett
#' d_ki_kontroll = ki_kontroll(d_inkl_oppf, d_diag)
ki_kontroll = function(d_inkl_oppf, d_diag) {

  # FIXME - støtte for grupperingsvariabler
  # FIXME - fixme's fra QA under.
  # FIXME - støtte for annen identifikator enn PasientGUID.
  # Pasienter som oppfyller kriterier for diagnose og diagnosetidspunkt.
  id_diagnose = d_diag %>%
    group_by(PasientGUID) %>%
    arrange(dato_diag, .by_group = TRUE) %>%
    distinct(PasientGUID, .keep_all = TRUE) %>%
    filter(
      diaggrupper_med == 1,
      diag_stilt_aar >= 2014
    ) %>%
    pull(PasientGUID)

  # Finner de som oppfyller krav for nevner
  d_base = d_inkl_oppf %>%
    left_join(d_diag %>% select(PasientGUID, diaggrupper_med, diaggrupper_hoved, dato_diag, diag_stilt_aar), by = "PasientGUID") %>%
    mutate(tid_til_inkl = InklusjonDato - dato_diag) %>%
    mutate(ki_krit_nevner = ifelse(PasientGUID %in% id_diagnose &
      diaggrupper_med == 1 &
      tid_til_inkl >= 0 &
      tid_til_inkl <= 90 &
        (DeathDate >= dato_diag + days(90) | is.na(DeathDate)),
      # fixme (QA): Er inklusjon+diagnose der tid til inklusjon er negativ rett handtert?
    yes = TRUE, no = FALSE
    )) # fixme (QA): igjen håplaus bruk av ifelse().

  d_ki_kontroll = d_base %>%
    mutate(
      dager_til_ktrl = dato_ktrl - dato_diag,
      ki_krit_teller = ifelse(ki_krit_nevner & Skjematype == "Inklusjonskjema" & # fixme (QA): teit bruk av ifelse().
        dager_til_ktrl > 7 & # fixme (QA): Mange magiske konstantar her. Gjer dei om til variablar.
        dager_til_ktrl <= 90 | # fixme (QA): For så vidt rett, men for alle som ikkje har pugga detaljane i operatorpresedensane i R kunne det med fordel ha vore nokre parentesar!
        ki_krit_nevner & Skjematype == "Oppfølgingskjema" &
          dager_til_ktrl <= 90,
      yes = TRUE, no = FALSE
      )
    ) %>% # fixme (QA): I skildringa sto det noko med ulike intervall, eks. 28 dagar. Men talet 28 dagar er ingen plass å sjå her, så indikatoren *må* vera rekna ut feil.
    group_by(PasientGUID) %>%
    arrange(desc(ki_krit_teller), desc(ki_krit_nevner), .by_group = TRUE) %>%
    distinct(PasientGUID, .keep_all = TRUE) %>%
    ungroup()

  d_ki_kontroll
}

#' Gjennomsnittlig DAPSA for psoriasisartrittpasienter 1 år etter diagnose.
#'
#' @description
#' Indikatoren viser gjennomsnittlig DAPSA 1 år etter diagnosedato
#' for pasienter med psoriasisartritt.
#'
#' @details
#' `tisdrom_start` og `tidsrom_slutt` indikerer start og slutt for hva som skal
#' regnes som 1-års kontroll i NorArtritt da det ikke er en fast kontroll etter
#' 1 år. Pasienten må ha psoriasisartritt som siste diagnose,
#' stilt i 2014 eller senere.
#' Pasienten må ha hatt diagnosen i minst 1 år ved datadumpdato, og ha vært
#' til kontroll i tidsrommet avgrenset med `tidsrom_start` og `tidsrom_slutt`.
#' I tillegg kan ikke variabelen for DAPSA være missing.
#'
#' Dersom en pasient har hatt flere kontroller i det gyldige tidsintervallet
#' blir bare den kontrollen (som ikke har missing DAPSA-verdi) som er nærmest
#' 365 dager etter diagnose inkludert.
#'
#' @param d_diag Diagnosedatasett.
#' @param d_inkl_oppf Sammenslått inklusjon- og oppfølgingsdatasett.
#' @param tidsrom_start
#' Nedre grense av tidsrom for beregning av hva som er 1-års kontroll.
#' Antall dager etter diagnose.
#' @param tidsrom_slutt
#' Øvre grense av tidsrom for beregning av hva som er 1-års kontroll.
#' Antall dager etter diagnose.
#'
#' @return
#' Returnerer et datasett med følgende variabler:
#' * \strong{PasientGUID} Pasientidentifikator.
#' * \strong{ki_x} Numerisk verdi for DAPSA for pasienten.
#' * \strong{ki_aktuell} Indikator for om pasienten oppfyller
#' kriterier for nevner. Kan ta verdiene TRUE eller FALSE.
#' * \strong{...} Eventuelle grupperingsvariabler blir også med i utdata.
#'
#' @export
#'
#' @examples
#' # d_diag og d_inkl_oppf er diagnose og inklusjon/oppfølgingsdata fra NorArtritt.
#' d_ki_dapsa = ki_dapsa(d_diag, d_inkl_oppf)
ki_dapsa = function(d_diag, d_inkl_oppf, tidsrom_start = 180, tidsrom_slutt = 485) {

  # FIXME - Legge inn støtte for grupperingsvariabler.

  # Henter ut id til pasientene som oppfyller kriterier for diagnose og diagnosetidspunkt
  id_diagnose = d_diag %>%
    select(PasientGUID, diaggrupper_med, dato_diag, diag_stilt_aar, dager_diag_til_datadump) %>%
    arrange(dato_diag) %>%
    distinct(PasientGUID, .keep_all = TRUE) %>%
    filter(
      diaggrupper_rem == 2, # Psoriasisartritt
      diag_stilt_aar >= 2014,
      dager_diag_til_datadump >= 365
    ) %>%
    pull(PasientGUID)

  # Kobler diagnosedata med skjema for de ulike kontrollene for hver pasient i utvalget.
  d = d_inkl_oppf %>%
    left_join(d_diag %>% select(
      PasientGUID, diaggrupper_rem, dato_diag,
      dager_diag_til_datadump, diag_stilt_aar
    ), by = "PasientGUID") %>%
    mutate(dager_siden_diagnose = ymd(dato_ktrl) - ymd(dato_diag)) %>%
    mutate(ki_aktuell = PasientGUID %in% id_diagnose &
             dager_siden_diagnose >= tidsrom_start &
             dager_siden_diagnose <= tidsrom_slutt &
             !(is.na(DAPSA)) &
             (DeathDate >= dato_diag + days(tidsrom_slutt) | is.na(DeathDate)))

  # Finner hvem som oppfyller krav for teller og reduserer til en rad per pasient.
  d_ki_dapsa = d %>%
    mutate(ki_x = DAPSA) %>%
    group_by(PasientGUID) %>%
    arrange(desc(ki_aktuell), abs(365 - dager_siden_diagnose), .by_group = TRUE) %>%
    distinct(PasientGUID, .keep_all = TRUE) %>%
    ungroup()

  d_ki_dapsa
}

#' Gjennomsnittlig ASDAS-CRP for spondyloartrittpasienter 1 år etter diagnose.
#'
#' @description
#' Indikatoren viser gjennomsnittlig ASDAS-CRP 1 år etter diagnosedato
#' for pasienter med spondyloartritt
#'
#' @details
#' `tisdrom_start` og `tidsrom_slutt` indikerer start og slutt for hva som skal
#' regnes som 1-års kontroll i NorArtritt da det ikke er en fast kontroll etter
#' 1 år. Pasienten må ha spondyloartritt som siste diagnose,
#' stilt i 2014 eller senere.
#' Pasienten må ha hatt diagnosen i minst 1 år ved datadumpdato, og ha vært
#' til kontroll i tidsrommet avgrenset med `tidsrom_start` og `tidsrom_slutt`.
#' I tillegg kan ikke variabelen for ASDAS-CRP være missing.
#'
#' Dersom en pasient har hatt flere kontroller i det gyldige tidsintervallet
#' blir bare den kontrollen (som ikke har missing ASDAS-CRP-verdi)
#' som er nærmest 365 dager etter diagnose inkludert.
#'
#' @param d_diag Diagnosedatasett.
#' @param d_inkl_oppf Sammenslått inklusjon- og oppfølgingsdatasett.
#' @param tidsrom_start
#' Nedre grense av tidsrom for beregning av hva som er 1-års kontroll.
#' Antall dager etter diagnose.
#' @param tidsrom_slutt
#' Øvre grense av tidsrom for beregning av hva som er 1-års kontroll.
#' Antall dager etter diagnose.
#'
#' @return
#' Returnerer et datasett med følgende variabler:
#' * \strong{PasientGUID} Pasientidentifikator.
#' * \strong{ki_x} Numerisk verdi for ASDAS-CRP for pasienten.
#' * \strong{ki_aktuell} Indikator for om pasienten oppfyller
#' kriterier for nevner. Kan ta verdiene TRUE eller FALSE.
#' * \strong{...} Eventuelle grupperingsvariabler blir også med i utdata.
#'
#' @export
#'
#' @examples
#' # d_diag og d_inkl_oppf er diagnose og inklusjon/oppfølgingsdata fra NorArtritt.
#' d_ki_asdas = ki_asdas(d_diag, d_inkl_oppf)
ki_asdas = function(d_diag, d_inkl_oppf, tidsrom_start = 180, tidsrom_slutt = 485) {

  # FIXME - Dato for datadump må hentes dynamisk!
  # FIXME - Legge inn støtte for grupperingsvariabler.
  # FIXME - Validering og håndtering av variabler med "mrs_" prefiks.

  # Henter ut id til pasientene som oppfyller kriterier for diagnose og diagnosetidspunkt
  id_diagnose = d_diag %>%
    select(PasientGUID, diaggrupper_med, dato_diag, diag_stilt_aar, dager_diag_til_datadump) %>%
    arrange(desc(dato_diag)) %>%
    distinct(PasientGUID, .keep_all = TRUE) %>%
    filter(
      diaggrupper_med %in% 5:6, # Spondyloartritt
      diag_stilt_aar >= 2014,
      dager_diag_til_datadump >= 365
    ) %>%
    pull(PasientGUID)

  # Kobler diagnosedata med skjema for de ulike kontrollene for hver pasient i utvalget.
  d = d_inkl_oppf %>%
    left_join(d_diag %>% select(
      PasientGUID, diaggrupper_med, dato_diag,
      dager_diag_til_datadump, diag_stilt_aar
    ), by = "PasientGUID") %>%
    mutate(dager_siden_diagnose = ymd(dato_ktrl) - ymd(dato_diag)) %>%
    mutate(ki_aktuell = PasientGUID %in% id_diagnose &
             dager_siden_diagnose >= tidsrom_start &
             dager_siden_diagnose <= tidsrom_slutt &
             !(is.na(Asdas)) &
             (DeathDate >= dato_diag + days(tidsrom_slutt) | is.na(DeathDate)))

  # Finner hvem som oppfyller krav for teller og reduserer til en rad per pasient.
  d_ki_asdas = d %>%
    mutate(ki_x = Asdas) %>%
    group_by(PasientGUID) %>%
    arrange(desc(ki_aktuell), abs(365 - dager_siden_diagnose), .by_group = TRUE) %>%
    distinct(PasientGUID, .keep_all = TRUE) %>%
    ungroup()

  d_ki_asdas
}
