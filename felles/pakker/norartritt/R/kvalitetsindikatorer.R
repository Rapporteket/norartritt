# Kvalitetsindikatorer for NorArtritt

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

  # Sykdomsmodifiserende midler:
  sykmod_medisin = c(1:8, 10:12, 14:16, 18, 20, 22, 24:26, 28:32)

  # Koble på inklusjonsskjema for å hente ut inklusjonstidspunkt og sykehustilhørighet
  d_inkl_diag_med = d_diagnose %>%
    left_join(d_inklusjon %>%
      select(
        PasientGUID, InklusjonDato,
        sykehusnavn, sykehus_kortnavn
      ),
    by = "PasientGUID"
    ) %>%
    left_join(d_medisin %>%
      select(
        PasientGUID, StartDato,
        legemiddel_navn, legemiddel_navn_kode
      ),
    by = "PasientGUID"
    ) %>%
    mutate(
      dager_fra_diag_til_inkl =
        as.numeric(as_date(InklusjonDato) - dato_diag)
    )

  # Skal se på pasienter som kun har én diagnose
  d_ki_n_diag = d_inkl_diag_med %>%
    group_by(PasientGUID) %>%
    mutate(n_diag = n_distinct(diaggrupper_med)) %>%
    ungroup()

  # Lage ki_krit_nevner
  d_ki_med_krit_nevner = d_ki_n_diag %>%
    mutate(ki_krit_nevner = n_diag == 1 &
      diaggrupper_med == 1 &
      diag_stilt_aar >= 2014 &
      dager_fra_diag_til_inkl >= 0 &
      dager_fra_diag_til_inkl <= 90)

  # Legge til teller og reduserer til en rad per pasient
  d_ki = d_ki_med_krit_nevner %>%
    mutate(
      tid_til_oppstart_medisin = StartDato - dato_diag,
      ki_krit_teller = ki_krit_nevner &
        legemiddel_navn_kode %in% sykmod_medisin &
        tid_til_oppstart_medisin >= 0 &
        tid_til_oppstart_medisin <= 14
    ) %>%
    group_by(PasientGUID) %>%
    arrange(desc(ki_krit_teller), desc(ki_krit_nevner), .by_group = TRUE) %>%
    distinct(PasientGUID, .keep_all = TRUE) %>%
    select(
      PasientGUID, UnitId, diag_stilt_aar, ki_krit_teller,
      ki_krit_nevner, tid_til_oppstart_medisin, dato_diag
    ) %>%
    ungroup()

  d_ki
}

# Andel RA-pasienter som bruker methotrexate per år.
ki_medisinbruk = function(d_diagnose, d_medisin, aarstall, legemiddel) {
  d_ki = d_diagnose %>%
    left_join(d_medisin %>%
      select(
        PasientGUID, StartDato,
        SluttDato, startaar, sluttaar, legemiddel_navn,
        legemiddel_navn_kode
      ), by = "PasientGUID") %>%
    mutate(ki_krit_nevner = diaggrupper_med == 1 &
      diag_stilt_aar <= aarstall)

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
    select(PasientGUID, UnitId, ki_krit_teller, ki_krit_nevner, diag_stilt_aar) %>%
    ungroup()

  d_ki_med_krit
}

# Andel RA-pasienter som oppnår remisjon 1 år etter diagnose.
ki_remisjon = function(d_diag, d_inkl_oppf, tidsrom_start = 180, tidsrom_slutt = 485) {

  # Indikator som ser på hvor stor andel av pasientene med
  # Revmatoid Artritt (diaggrupper_med == 1)
  # som oppnår remisjon ved kontroll 1 år etter diagnose
  # 1 år regnes her som tidsrommet 180-485 dager etter diagnose.

  # For å inkluderes i nevner på pasienten ha Revmatoid Artritt som siste diagnose,
  # og diagnosen må være stilt i 2014 eller senere.
  # Pasienten må også ha hatt diagnosen i minimum 365 dager ved datadump-dato,
  # og pasienten må ha vært til kontroll i tidsrommet 180-485 dager etter diagnose.
  # Ingen av målingene for acreular-kriteriene kan være NA

  # For å inkluderes i teller skal alle kriteriene for nevner være oppfylt,
  # og i tillegg må pasienten oppfylle krav for remisjon gitt ved acr-eular kriteriene.
  # Kriteriene her er:
  # - 1 eller færre ømme ledd,
  # - 1 eller færre hovne ledd,
  # - Crp <= 10,
  # - globalsykdomsaktivitet <= 10.

  # FIXME - Dato for datadump må hentes dynamisk!

  # Henter ut id til pasientene som oppfyller kriterier for diagnose og diagnosetidspunkt
  id_diagnose = d_diag %>%
    select(PasientGUID, diaggrupper_med, dato_diag, diag_stilt_aar, dager_diag_til_datadump) %>%
    arrange(desc(dato_diag)) %>%
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
    ), by = "PasientGUID") %>%
    mutate(dager_siden_diagnose = ymd(dato_ktrl) - ymd(dato_diag)) %>%
    mutate(ki_krit_nevner = PasientGUID %in% id_diagnose &
      dager_siden_diagnose >= tidsrom_start &
      dager_siden_diagnose <= tidsrom_slutt &
      !(is.na(mrs_OmmeLeddAntall)) &
      !(is.na(mrs_HovneLeddAntall)) &
      !(is.na(Crp)) &
      !(is.na(PasientGlobalSykdomsaktivitet)))

  # Finner hvem som oppfyller krav for teller og reduserer til en rad per pasient.
  d_ki_rem = d %>%
    mutate(ki_krit_teller = ki_krit_nevner &
      mrs_OmmeLeddAntall <= 1 & # fixme (QA): Produksjonskode bør aldri bruka variablar som startar med «mrs_», sidan me ikkje har noko kontroll på innhaldet. Desse må handterast spesielt (med validering og parsing) og «vanlege» variablar (med andre namn) lagast.
      mrs_HovneLeddAntall <= 1 & # fixme (QA): Sjå over.
      Crp <= 10 & !is.na(Crp) &
      PasientGlobalSykdomsaktivitet <= 10 &
      !is.na(PasientGlobalSykdomsaktivitet)) %>%
    group_by(PasientGUID) %>%
    arrange(desc(ki_krit_teller), desc(ki_krit_nevner), .by_group = TRUE) %>%
    distinct(PasientGUID, .keep_all = TRUE) %>%
    ungroup()

  d_ki_rem
}

# Remisjon totalt
remisjon_totalt = function(d_diag, d_inkl_oppf) {
  # En funksjon for å beregne oppnåelse av remisjon totalt sett.
  # Denne stiller ingen krav til tidspunkt for oppnåelse, men er laget
  # for å vise totalt andel av pasienter med RA som har oppnådd remisjon,
  # samt antall som har nådd remisjon i et kalenderår.

  # fixme (QA): Forvirrande tekst om kalenderår. Det er ikkje noko
  #             argument for kalenderår, så funksjonen er openbart
  #             ikkje laga for det (ein kan sjølvsagt køyra funksjonen
  #             per kalenderår, med do() eller group_map() elns.,
  #             men det alle variablar (ikkje berre kalenderår), så det
  #             er tullete og forvirrande å dra inn kalenderår her).
  #
  # fixme (QA): Funksjonen liknar veldig på ki_remisjon(). Er nok
  #             mykje betre/sikrare/enklare om éin av funksjonane er
  #             spesialtilfelle av den andre (eks. med argument
  #             remisjon_innan, som kan vera 365 eller Inf).
  #             Alt som gjeld førre funksjon gjeld òg denne, så
  #             eg skriv det ikkje om igjen.

  # Henter ut id til pasientene som oppfyller kriterier for diagnose
  id_diagnose = d_diag %>%
    select(PasientGUID, diaggrupper_med, dato_diag, dager_diag_til_datadump) %>%
    arrange(desc(dato_diag)) %>%
    distinct(PasientGUID, .keep_all = TRUE) %>%
    filter(diaggrupper_med == 1) %>% # fixme (QA): Stemmer det at ein berre skal sjå pasientane der siste diaggrupper_med er lik 1? (Eller skal ein kanskje sjå på siste oppføring der diaggrupper_med er lik 1 for kvar pasient?) Det er uansett ikkje dokumentert kva som er gjort, så det umogleg for meg å vurdera om det er gjort rett eller galt.
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
      mrs_OmmeLeddAntall <= 1 &
      mrs_HovneLeddAntall <= 1 &
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

# andel RA pasienter som er til kontroll innen 3 mnd
ki_kontroll = function(d_inkl_oppf, d_diag) {

  # Skal se på andelen pasienter diagnostisert med RA i 2014 eller senere (som
  # siste diagnose) som var til kontroll innen 3 mnd av diagnosedato. (90 dager)
  # InklusjonDato må være maks 90 dager etter diagnosedato.
  # Vi trenger datoer for diagnose, inklusjon og kontroll.

  # Hvis pasienten har inklusjonsskjema 7-28 dager etter diagnose anses det som første kontroll.
  # Hvis pasienten har inklusjonsskjema 0-7 eller 29-90 dager etter diagnose anses første oppfølgingsskjema som første kontroll.

  # Nevner:
  # RA som siste diagnose, satt i 2014 eller senere
  # DatoInklusjon - dato_diagnose <= 90

  # Teller:
  # Samme som over, men må også ha hatt en kontroll i tidsrommet 0-90 dager etter diagnose.

  # Pasienter som oppfyller kriterier for diagnose og diagnosetidspunkt.
  id_diagnose = d_diag %>%
    group_by(PasientGUID) %>%
    arrange(desc(dato_diag), .by_group = TRUE) %>%
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
      tid_til_inkl <= 180, # fixme (QA): Er inklusjon+diagnose der tid til inklusjon er negativ rett handtert?
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
