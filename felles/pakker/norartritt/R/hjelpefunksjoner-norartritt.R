# Hjelpefunksjoner som brukes i norartritt

#' @importFrom magrittr %>%
#' @importFrom dplyr mutate filter select left_join right_join case_when group_by summarise
NULL

# Hente sykehusnavn
# Hente diagnosegrupper

# Lage en funksjon for å koble alle skjema sammen via skjemaGUID.
# Tror det vil være nyttig for å filtrere bort pasienter som ikke har
# inklusjonsskjema og for å gjøre andre nyttige koblinger.


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
    col_types = c("ici__iiiiiicc"),
    locale = locale(encoding = "windows-1252")
  )

  medisin_kode_999 = readr::read_delim(paste0(mappe, legemiddel_999),
    delim = ";",
    trim_ws = TRUE,
    col_types = c("ic"),
    locale = locale(encoding = "windows-1252")
  )

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
    select(-legemiddel_navn, -medisin, -legemiddel_kodebok_kode) %>%
    right_join(medisin_fil %>%
      select(-LegemiddelType),
    by = "legemiddel_navn_kode"
    )

  # Sjekk at det kun er et legemiddelnavn per kode
  n_navn = d_medisin %>%
    group_by(legemiddel_navn_kode) %>%
    summarise(
      n = dplyr::n_distinct(legemiddel_navn),
      .groups = "drop"
    ) %>%
    filter(n != 1)

  if (!nrow(n_navn) == 0) {
    stop(paste0(
      "LegemiddelType ",
      stringr::str_c(n_navn$legemiddel_navn_kode, collapse = ", "),
      " har flere navn for samme kode"
    ))
  }

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
  sykehus_navnefil = read_delim(adresse,
    delim = ";",
    col_types = c("iccic_"),
    locale = locale(encoding = "windows-1252")
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
  diagnosegrupper = read_delim(adresse,
    delim = ";",
    col_types = c("ciciccicic"),
    locale = locale(encoding = "windows-1252")
  )

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
