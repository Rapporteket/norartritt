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

  # Koble på inklusjonsskjema for å hente ut inklusjonstidspunkt og sykehustilhørighet
  d_inkl_diag_med = d_diagnose |>
    arrange(dato_diag) |>
    distinct(PasientGUID, .keep_all = TRUE) |>
    left_join(select(d_inklusjon,
        PasientGUID, InklusjonDato, sykehusnavn, sykehus_kortnavn
      ),
      by = "PasientGUID",
      relationship = "one-to-one"
    ) |>
    left_join(select(d_medisin,
        PasientGUID, StartDato, SluttDato,
        legemiddel_navn, legemiddel_navn_kode, dmard
      ),
      by = "PasientGUID",
      relationship = "one-to-many"
    ) |>
    mutate(
      dager_fra_diag_til_inkl =
        as.numeric(as_date(InklusjonDato) - as_date(dato_diag))
    )

  # Lage ki_krit_nevner
  d_ki_med_krit_nevner = d_inkl_diag_med |>
    mutate(ki_krit_nevner = diaggrupper_med == 1 &
      diag_stilt_aar >= 2014 &
      dager_fra_diag_til_inkl >= 0 &
      dager_fra_diag_til_inkl <= 90 &
      (DeathDate >= dato_diag + days(14) | is.na(DeathDate)))

  # Legge til teller og reduserer til en rad per pasient
  d_ki = d_ki_med_krit_nevner |>
    mutate(
      tid_til_oppstart_medisin = as_date(StartDato) - as_date(dato_diag),
      ki_krit_teller =
        case_when(
          is.na(tid_til_oppstart_medisin) ~ FALSE,
          ki_krit_nevner &
            dmard == 1 &
            tid_til_oppstart_medisin >= 0 &
            tid_til_oppstart_medisin <= 14 ~ TRUE,
          TRUE ~ FALSE
        )
    ) |>
    group_by(PasientGUID) |>
    arrange(desc(ki_krit_teller), desc(ki_krit_nevner), .by_group = TRUE) |>
    distinct(PasientGUID, .keep_all = TRUE) |>
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
#' @param diagnosekoder Hvilke diagnoser som skal inkluderes i beregningen.
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
#' d_ki_medisinbruk = ki_medisinbruk(d_diagnose = d_diag, d_medisin = d_medisin, aarstall = 2020, legemiddel = 20, diagnosekoder = 1)
ki_medisinbruk = function(d_diagnose, d_medisin, aarstall, legemiddel, diagnosekoder) {

  d_ki = d_diagnose |>
    left_join(select(d_medisin,
        PasientGUID, StartDato,
        SluttDato, startaar, sluttaar, legemiddel_navn,
        legemiddel_navn_kode
      ),
      by = "PasientGUID"
    ) |>
    mutate(ki_krit_nevner = diaggrupper_rem %in% diagnosekoder &
      diag_stilt_aar <= aarstall &
      (year(DeathDate) >= aarstall | is.na(DeathDate)))

  d_ki_med_krit = d_ki |>
    filter(ki_krit_nevner) |>
    mutate(ki_krit_teller = ki_krit_nevner &
      legemiddel_navn_kode %in% legemiddel &
      !is.na(legemiddel_navn_kode) &
      !is.na(startaar) &
      startaar <= aarstall &
      (sluttaar >= aarstall | is.na(SluttDato))) |>
    group_by(PasientGUID) |>
    arrange(desc(ki_krit_teller), desc(ki_krit_nevner), .by_group = TRUE) |>
    distinct(PasientGUID, .keep_all = TRUE) |>
    ungroup()

  d_ki_med_krit
}


#' Median dager til oppstart medisinbruk
#'
#' @description
#' Indikatorfunksjon for å beregne median tid fra diagnose til oppstart
#' medisinering med aktuell medisin. Funksjonen er ment å brukes på
#' forhåndsbehandlede datasett hvor kobling allerede er gjort.
#' Dette kravet er satt for å begrense kompleksitet i indikatorfunksjonen.
#'
#' @note
#' Pasienter som ikke har registrert medisinskjema vil få satt en sensurdato
#' tilsvarende antall dager fra diagnose til dato datadump er tatt ut.
#' Videre antas det også at pasienter som ikke har startet på medisinering
#' innen 365 dager etter diagnosedato ikke er aktuelle for medisinering.
#' Disse fjernes derfor fra indikatoren.
#'
#' @param d_ra_base Forhåndsbehandlet datasett som inkluderer et uttrekk fra
#' registeret hvor vi har hentet ut pasienter som er aktuelle for de ulike
#' indikatorene for Revmatoid Artritt. Består av data fra inklusjonsskjema og
#' Diagnoseskjema.
#' Funksjonen `lag_ra_indikator_base()` kan brukes for å generere datasettet.
#'
#' @param d_med Datasett med aktuelle medisinskjema.
#'
#' @return
#' Returnerer et datasett med nødvendige koblingsnøkler.
#' Kolonnene `ki_x` og `ki_aktuell` inneholder antall dager fra diagnose til
#' oppstart medisinering og indikator for om raden skal inkluderes eller ikke.
#'
#' @export
#'
#' @examples
ki_sykmod_median = function(d_ra_base, d_med) {

  ovre_grense_medisin = 365

  d_ki_medisin_median = d_ra_base |>
    left_join(
      d_med |>
        filter(dmard == 1) |>
        select(PasientGUID, StartDato, LegemiddelType, dmard),
      by = "PasientGUID",
      relationship = "one-to-many"
    ) |>
    mutate(
      dager_til_medisin = as.numeric(difftime(StartDato, dato_diag, units = "days")),
      sensurert = is.na(dager_til_medisin),
      dager_til_medisin_sens = case_when(
        is.na(dager_til_medisin) & dager_diag_til_datadump > ovre_grense_medisin ~ as.numeric(dager_diag_til_datadump),
        is.na(dager_til_medisin) & dager_diag_til_datadump <= ovre_grense_medisin ~ ovre_grense_medisin,
        TRUE ~ dager_til_medisin
      ),
    ) |>
    select(
      -dmard, -sensurert, -dager_til_medisin,
      -dager_diag_til_datadump, -dager_diag_til_inklusjon
    ) |>
    mutate(
      ki_aktuell = dager_til_medisin_sens >= 0 &
        dager_til_medisin_sens <= ovre_grense_medisin &
        (DeathDate >= dato_diag + days(14) | is.na(DeathDate)),
      ki_x = dager_til_medisin_sens
    ) |>
    select(-dager_til_medisin_sens)

  d_ki_medisin_median
}

#' Gjennomsnitt antall dager til oppstart medisinbruk
#'
#' @description
#' Indikatorfunksjon for å beregne gjennomsnittlig tid fra diagnose til oppstart
#' medisinering med aktuell medisin. Funksjonen er ment å brukes på
#' forhåndsbehandlede datasett hvor kobling allerede er gjort.
#' Dette kravet er satt for å begrense kompleksitet i indikatorfunksjonen.
#'
#' @note
#' Pasienter som ikke har fått registrert medisinering innen 365 dager av diagnose
#' antas å ikke skulle ha medisinering, så disse vil derfor fjernes fra indikatoren.
#' Videre vil det være pasienter som ikke har startet medisinering og samtidig
#' ha fått diagnosen for mindre enn 365 dager siden ved tidspunktet for datauttrekk.
#' Disse pasientene (~ 5% av de som fikk diagnose mindre enn 365 dager før
#' datadump-dato) fjernes fra indikator frem til enten det registreres
#' et medisinskjema (hvorpå de inkluderes) eller det går 365 dager fra diagnose
#' og de ekskluderes.
#'
#' @param d_ra_base Forhåndsbehandlet datasett som inkluderer et uttrekk fra
#' registeret hvor vi har hentet ut pasienter som er aktuelle for de ulike
#' indikatorene for Revmatoid Artritt. Består av data fra inklusjonsskjema og
#' Diagnoseskjema.
#' Funksjonen `lag_ra_indikator_base()` kan brukes for å generere datasettet.
#'
#' @param d_med Datasett med aktuelle medisinskjema.
#'
#' @return
#' Returnerer et datasett med nødvendige koblingsnøkler.
#' Kolonnene `ki_x` og `ki_aktuell` inneholder antall dager fra diagnose til
#' oppstart medisinering og indikator for om raden skal inkluderes eller ikke.
#'
#' @export
#'
#' @examples
ki_sykmod_snitt = function(d_ra_base, d_med) {

  ovre_grense_medisin = 365

  d_ki_medisin_snitt = d_ra_base |>
    left_join(
      d_med |>
        filter(dmard == 1) |>
        select(PasientGUID, StartDato, LegemiddelType, dmard),
      by = "PasientGUID",
      relationship = "one-to-many"
    ) |>
    mutate(dager_til_medisin = as.numeric(
      difftime(
        StartDato,
        dato_diag,
        units = "days"
      )
    )) |>
    filter(!is.na(dager_til_medisin)) |>
    select(-dmard, -dager_diag_til_datadump, -dager_diag_til_inklusjon) |>
    mutate(
      ki_aktuell = dager_til_medisin >= 0 &
        dager_til_medisin <= ovre_grense_medisin &
        (DeathDate >= dato_diag + days(14) | is.na(DeathDate)),
      ki_x = dager_til_medisin
    ) |>
    select(-dager_til_medisin)

  d_ki_medisin_snitt
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

  # Henter ut id til pasientene som oppfyller kriterier for diagnose og diagnosetidspunkt
  id_diagnose = d_diag |>
    select(PasientGUID, diaggrupper_med, dato_diag, diag_stilt_aar, dager_diag_til_datadump) |>
    arrange(dato_diag) |>
    distinct(PasientGUID, .keep_all = TRUE) |>
    filter(
      diaggrupper_med == 1,
      diag_stilt_aar >= 2014,
      dager_diag_til_datadump >= 365
    ) |>
    pull(PasientGUID)

  # Kobler diagnosedata med skjema for de ulike kontrollene for hver pasient i utvalget.
  d = d_inkl_oppf |>
    left_join(select(d_diag,
        PasientGUID, diaggrupper_med, dato_diag,
        dager_diag_til_datadump, diag_stilt_aar
      ),
      by = "PasientGUID",
      relationship = "many-to-one"
    ) |>
    mutate(dager_siden_diagnose = ymd(dato_ktrl) - ymd(dato_diag)) |>
    mutate(ki_krit_nevner = PasientGUID %in% id_diagnose &
      dager_siden_diagnose >= tidsrom_start &
      dager_siden_diagnose <= tidsrom_slutt &
      !(is.na(OmmeLeddAntall)) &
      !(is.na(HovneLeddAntall)) &
      !(is.na(Crp)) &
      !(is.na(PasientGlobalSykdomsaktivitet)) &
      (DeathDate >= dato_diag + days(tidsrom_slutt) | is.na(DeathDate)))

  # Finner hvem som oppfyller krav for teller og reduserer til en rad per pasient.
  d_ki_rem = d |>
    mutate(ki_krit_teller = ki_krit_nevner &
      OmmeLeddAntall <= 1 &
      HovneLeddAntall <= 1 &
      Crp <= 10 & !is.na(Crp) &
      PasientGlobalSykdomsaktivitet <= 10 &
      !is.na(PasientGlobalSykdomsaktivitet)) |>
    group_by(PasientGUID) |>
    arrange(desc(ki_krit_teller), desc(ki_krit_nevner), .by_group = TRUE) |>
    distinct(PasientGUID, .keep_all = TRUE) |>
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

  # Henter ut id til pasientene som oppfyller kriterier for diagnose
  id_diagnose = d_diag |>
    select(PasientGUID, diaggrupper_med, dato_diag, dager_diag_til_datadump) |>
    arrange(dato_diag) |>
    distinct(PasientGUID, .keep_all = TRUE) |>
    filter(diaggrupper_med == 1) |>
    pull(PasientGUID)

  d = d_inkl_oppf |>
    left_join(select(d_diag,
        PasientGUID, diaggrupper_med, dato_diag,
        dager_diag_til_datadump, diag_stilt_aar
      ),
      by = "PasientGUID"
    ) |>
    mutate(ki_krit_nevner = PasientGUID %in% id_diagnose)

  d_rem_totalt = d |>
    mutate(ki_krit_teller = ki_krit_nevner &
      OmmeLeddAntall <= 1 &
      HovneLeddAntall <= 1 &
      Crp <= 10 & !is.na(Crp) &
      PasientGlobalSykdomsaktivitet <= 10 &
      !is.na(PasientGlobalSykdomsaktivitet)) |>
    group_by(PasientGUID) |>
    arrange(desc(ki_krit_teller), desc(ki_krit_nevner), .by_group = TRUE) |>
    distinct(PasientGUID, .keep_all = TRUE) |>
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

  # Pasienter som oppfyller kriterier for diagnose og diagnosetidspunkt.
  id_diagnose = d_diag |>
    group_by(PasientGUID) |>
    arrange(dato_diag, .by_group = TRUE) |>
    distinct(PasientGUID, .keep_all = TRUE) |>
    filter(
      diaggrupper_med == 1,
      diag_stilt_aar >= 2014
    ) |>
    pull(PasientGUID)

  # Finner de som oppfyller krav for nevner
  d_base = d_inkl_oppf |>
    left_join(
      select(d_diag, PasientGUID, diaggrupper_med, diaggrupper_hoved, dato_diag, diag_stilt_aar),
      by = "PasientGUID"
    ) |>
    mutate(tid_til_inkl = InklusjonDato - dato_diag) |>
    mutate(
      ki_krit_nevner = PasientGUID %in% id_diagnose &
        diaggrupper_med == 1 &
        tid_til_inkl >= 0 &
        tid_til_inkl <= 90 &
        (DeathDate >= dato_diag + days(90) | is.na(DeathDate))
    )

  d_ki_kontroll = d_base |>
    mutate(
      dager_til_ktrl = dato_ktrl - dato_diag,
      ki_krit_teller = ki_krit_nevner & Skjematype == "Inklusjonskjema" &
        dager_til_ktrl > 7 &
        dager_til_ktrl <= 90 |
        ki_krit_nevner & Skjematype == "Oppfølgingskjema" &
          dager_til_ktrl <= 90
    ) |>
    group_by(PasientGUID) |>
    arrange(desc(ki_krit_teller), desc(ki_krit_nevner), .by_group = TRUE) |>
    distinct(PasientGUID, .keep_all = TRUE) |>
    ungroup()

  d_ki_kontroll
}

#' Gjennomsnitt antall dager til første kontroll
#'
#' @description
#' Indikatorfunksjon for å beregne gjennomsnittlig tid fra diagnose til første
#' oppfølging for pasienter diagnostisert med Revmatoid Artritt.
#' Funksjonen er ment å brukes på forhåndsbehandlede datasett hvor kobling
#' allerede er gjort. Dette kravet er satt for å flytte
#' kompleksitet fra indikatorfunksjoner til en egen filtreringsfunksjon.
#'
#' @note
#' Pasienter som ikke har vært til kontroll innen 365 dager fjernes fra
#' indikatoren. Pasienter som fikk diagnose for under 365 dager siden men som
#' enda ikke har vært til kontroll fjernes også.
#' Det gjelder ca. X % av pasientene.
#'
#'
#' @param d_ra_base Forhåndsbehandlet datasett som inkluderer et uttrekk fra
#' registeret hvor vi har hentet ut pasienter som er aktuelle for de ulike
#' indikatorene for Revmatoid Artritt. Består av data fra inklusjonsskjema og
#' Diagnoseskjema.
#' Funksjonen `lag_ra_indikator_base()` kan brukes for å generere datasettet.
#'
#' @param d_oppf Datasett med aktuelle oppfølgingsskjema.
#'
#' @return
#' Returnerer et datasett med nødvendige koblingsnøkler.
#' Kolonnene `ki_x` og `ki_aktuell` inneholder antall dager fra diagnose til
#' første kontroll og indikator for om raden skal inkluderes eller ikke.
#'
#' @export
#'
#' @examples
ki_kontroll_snitt = function(d_ra_base, d_inkl_oppf) {
  # Hvis pasienten ikke har vært til kontroll innen 365 dager etter diagnose
  # antar vi at pasienten ikke er aktuell for indikatoren.
  ovre_grense_kontroll = 365

  # Grense for hvilke inklusjonskjema som skal regnes som oppfølging.
  inklusjonskjema_nedre_grense = 7
  inklusjonskjema_ovre_grense = 90

  d_ki_kontroll_snitt = d_ra_base |>
    left_join(select(d_inkl_oppf, PasientGUID, Skjematype, dato_ktrl),
      by = "PasientGUID",
      relationship = "one-to-many"
    ) |>
    mutate(dager_til_kontroll = as.numeric(
      difftime(
        dato_ktrl,
        dato_diag,
        units = "days"
      )
    )) |>
    mutate(er_oppf = case_when(
      Skjematype == "Inklusjonskjema" & dager_til_kontroll >= inklusjonskjema_nedre_grense & dager_til_kontroll <= inklusjonskjema_ovre_grense ~ TRUE,
      Skjematype == "Oppfølgingskjema" ~ TRUE,
      TRUE ~ FALSE
    )) |>
    group_by(PasientGUID) |>
    mutate(er_oppf_fiks = case_when(
      Skjematype == "Oppfølgingskjema" & dager_til_kontroll == min(dager_til_kontroll) ~ FALSE,
      TRUE ~ er_oppf
    )) |>
    arrange(dager_til_kontroll) |>
    mutate(
      ki_aktuell = (er_oppf_fiks & dager_til_kontroll <= ovre_grense_kontroll),
      ki_x = first(dager_til_kontroll[ki_aktuell])
    ) |>
    filter(
      DeathDate >= dato_diag + days(90) | is.na(DeathDate),
      ki_x <= ovre_grense_kontroll
    ) |>
    arrange(desc(ki_aktuell)) |>
    distinct(PasientGUID, .keep_all = TRUE) |>
    select(-er_oppf, -er_oppf_fiks) |>
    ungroup()

  d_ki_kontroll_snitt
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

  # Henter ut id til pasientene som oppfyller kriterier for diagnose og diagnosetidspunkt
  id_diagnose = d_diag |>
    select(PasientGUID, diaggrupper_rem, dato_diag, diag_stilt_aar, dager_diag_til_datadump) |>
    arrange(dato_diag) |>
    distinct(PasientGUID, .keep_all = TRUE) |>
    filter(
      diaggrupper_rem == 2, # Psoriasisartritt
      diag_stilt_aar >= 2014,
      dager_diag_til_datadump >= 365
    ) |>
    pull(PasientGUID)

  # Kobler diagnosedata med skjema for de ulike kontrollene for hver pasient i utvalget.
  d = d_inkl_oppf |>
    left_join(select(d_diag,
        PasientGUID, diaggrupper_rem, dato_diag,
        dager_diag_til_datadump, diag_stilt_aar
      ),
      by = "PasientGUID"
    ) |>
    mutate(dager_siden_diagnose = ymd(dato_ktrl) - ymd(dato_diag)) |>
    mutate(ki_aktuell = PasientGUID %in% id_diagnose &
      dager_siden_diagnose >= tidsrom_start &
      dager_siden_diagnose <= tidsrom_slutt &
      !(is.na(DAPSA)) &
      (DeathDate >= dato_diag + days(tidsrom_slutt) | is.na(DeathDate)))

  # Finner hvem som oppfyller krav for teller og reduserer til en rad per pasient.
  d_ki_dapsa = d |>
    mutate(ki_x = DAPSA) |>
    group_by(PasientGUID) |>
    arrange(desc(ki_aktuell), abs(365 - dager_siden_diagnose), .by_group = TRUE) |>
    distinct(PasientGUID, .keep_all = TRUE) |>
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

  # Henter ut id til pasientene som oppfyller kriterier for diagnose og diagnosetidspunkt
  id_diagnose = d_diag |>
    select(PasientGUID, diaggrupper_med, dato_diag, diag_stilt_aar, dager_diag_til_datadump) |>
    arrange(desc(dato_diag)) |>
    distinct(PasientGUID, .keep_all = TRUE) |>
    filter(
      diaggrupper_med %in% 5:6, # Spondyloartritt
      diag_stilt_aar >= 2014,
      dager_diag_til_datadump >= 365
    ) |>
    pull(PasientGUID)

  # Kobler diagnosedata med skjema for de ulike kontrollene for hver pasient i utvalget.
  d = d_inkl_oppf |>
    left_join(select(d_diag,
        PasientGUID, diaggrupper_med, dato_diag,
        dager_diag_til_datadump, diag_stilt_aar
      ),
      by = "PasientGUID"
    ) |>
    mutate(dager_siden_diagnose = ymd(dato_ktrl) - ymd(dato_diag)) |>
    mutate(ki_aktuell = PasientGUID %in% id_diagnose &
      dager_siden_diagnose >= tidsrom_start &
      dager_siden_diagnose <= tidsrom_slutt &
      !(is.na(Asdas)) &
      (DeathDate >= dato_diag + days(tidsrom_slutt) | is.na(DeathDate)))

  # Finner hvem som oppfyller krav for teller og reduserer til en rad per pasient.
  d_ki_asdas = d |>
    mutate(ki_x = Asdas) |>
    group_by(PasientGUID) |>
    arrange(desc(ki_aktuell), abs(365 - dager_siden_diagnose), .by_group = TRUE) |>
    distinct(PasientGUID, .keep_all = TRUE) |>
    ungroup()

  d_ki_asdas
}
