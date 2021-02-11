# Hjelpefunksjoner som brukes i norartritt

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
#' @param d medisindata fra NorArtritt. Må inneholde kolonnen LegemiddelType.
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
#' de ulike medisinene som registreres. Legemiddelnavn legges til, indikator
#' for om medisinen er biologisk, dmard, csdmard, tsdmard, en egen
#' indikator for "biologiske og tsdmard" og virkestoffnavn legges alle
#' til opprinnelig datasett.
#'
#' @export
#'
#' @examples
#' # d_medisin er medisindata fra NorArtritt.
#'
#' d_medisin_med_navn = legg_til_medisinnavn(d_medisin)
legg_til_medisinnavn = function(d_medisin) {

  # Leser inn kodebøker for medisiner
  mappe = paste0(***FJERNET ADRESSE***)
  medisin_grupper = "medisin-grupper.csv"
  legemiddel_999 = "legemiddel-kodebok.csv"

  medisin_fil = read_delim(paste0(mappe, medisin_grupper),
    delim = ";",
    trim_ws = TRUE,
    col_types = c("ici__iiiiiicc"),
    locale = locale(encoding = "windows-1252")
  )

  medisin_kode_999 = read_delim(paste0(mappe, legemiddel_999),
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
    mutate(medisin = coalesce(Legemiddel, legemiddel_navn)) %>%
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
      n = n_distinct(legemiddel_navn),
      .groups = "drop"
    ) %>%
    filter(n != 1)

  if (!nrow(n_navn) == 0) {
    stop(paste0(
      "LegemiddelType ",
      str_c(n_navn$legemiddel_navn_kode, collapse = ", "),
      " har flere navn for samme kode"
    ))
  }

  d_medisin
}
