#' Koblingsliste diagnosekoder
#'
#' Koblingsliste for å knytte ICD10-diagnosekoder til diagnosegrupper spesifisert
#' av registeret. Inneholder fire ulike fordelinger av diagnoser som brukes blant
#' annet i årsrapport. Grupperingene er aktuelle i ulike sammenhenger hvor
#' enkelte diagnosegrupper skal presenteres samlet eller hver for seg.
#'
#' Sist Oppdatert 2025-12-01.
#'
#' @format ## `diagnosekobling`
#' En tibble med 34 rader og 10 kolonner:
#' \describe{
#'  \item{Kode}{ICD10-kode}
#'  \item{diaggrupper_med}{diaggrupper_med-kode}
#'  \item{diaggrupper_med_tekst}{Navn for diaggrupper_med-kode}
#'  \item{diaggrupper_rem}{diaggrupper_rem-kode}
#'  \item{diaggrupper_rem_tekst}{Navn for diaggrupper_rem-kode}
#'  \item{rem_maal}{Remisjonsmål brukt for diaggrupper_rem-gruppe}
#'  \item{diaggrupper_hoved}{diaggrupper_hoved-kode}
#'  \item{diaggrupper_hoved_tekst}{Navn for diaggrupper_hoved-kode}
#'  \item{perifer_aksial_diaggruppe}{perifer_aksial_diaggruppe-kode}
#'  \item{perifer_aksial_diaggruppe_tekst}{Navn for perifer_aksial_diaggruppe-kode}
#' }
#' @source Laget av registeret og vedlikeholdt av statistiker ved fagsenteret
#' for medisinske kvalitetsregistre, region Vest.
#'
"diagnosekobling"

#' Koblingsliste medisiner
#'
#' Koblingsliste for å knytte legemiddelType til legemiddel og virkestoff.
#' Inneholder også informasjon om hvilken type legemiddel hver medisin skal regnes
#' som.
#'
#' @format ## `medisinkobling`
#' En tibble med 93 rader og 15 kolonner:
#' \describe{
#'  \item{LegemiddelType}{Kode for legemiddel i MRS}
#'  \item{legemiddel_navn}{Prosessert navn for legemiddel}
#'  \item{legemiddel_navn_kode}{Intern kode for legemiddel}
#'  \item{biokat}{Indikator for om legemiddel er biologisk}
#'  \item{dmard}{Indikator for om legemiddel er DMARD}
#'  \item{csdmard}{Indikator for om legemiddel er csDMARD}
#'  \item{tsdmard}{Indikator for om legemiddel er tsDMARD}
#'  \item{bio_og_tsdmard}{Indikator for om legemiddel er Biologisk eller tsDMARD}
#'  \item{legemiddel_gruppert}{Kode for legemiddel som skal grupperes (Samme virkestoff i ulike medisiner)}
#'  \item{legemiddel_gruppert_navn}{Navn på gruppert legemiddel}
#'  \item{Virkestoff}{Virkestoff i legemiddel}
#'  \item{legemiddelnavn_i_kodebok}{Legemiddelnavn hentet fra kodebok}
#'  \item{Kommentar}{Eventuelle kommentarer}
#' }
#' @source Laget av registeret og vedlikeholdt av statistiker ved fagsenteret
#' for medisinske kvalitetsregistre, region Vest.
#' Sist oppdatert 2025-12-01.
#'
"medisinkobling"

#' Koblingsliste legemiddel
#'
#' Koblingsliste som brukes for å håndtere medisiner som registreres i GTI men
#' som ikke finnes i MRS. Disse blir importert med LegemiddelType = 999, men de
#' må skilles ut for å kunne brukes i analyse.
#'
#' @format ## `legemiddelkobling`
#' En tibble med 98 rader og 2 kolonner:
#' \describe{
#'  \item{legemiddel_kodebok_kode}{Komplett liste med alle legemiddel, inkludert
#'  intern kode for legemiddel som importeres som kode 999}
#'  \item{legemiddel_kodebok}{Navn for legemiddel}
#' }
#'
#' @source Laget av registeret og vedlikeholdt av statistiker ved fagsenteret
#' for medisinske kvalitetsregistre, region Vest.
#' Sist oppdatert 2025-12-01.
"legemiddelkobling"

#' Koblingsliste sykehusnavn
#'
#' Koblingsliste for å lage leservennlige navn og kortnavn for sykehus og
#' avdelinger til bruk i rapporter og analyser.
#'
#' @format ## `sykehuskobling`
#' En tibble med 26 rader og 5 kolonner:
#' \describe{
#'  \item{resh_id}{Resh-id for sykehus}
#'  \item{sykehusnavn}{Leservennlig navn på sykehus}
#'  \item{sykehus_kortnavn}{Leservennlig kortnavn for sykehus}
#'  \item{sykehus_gruppe}{Kode for sykehusgruppe, nødvendig for å
#'  samle avtalespesialister}
#'  \item{sykehus_gruppe_navn}{Navn på gruppe}
#' }
#' @source Laget av registeret og vedlikeholdt av statistiker ved fagsenteret
#' for medisinske kvalitetsregistre, region Vest.
#' Sist oppdatert 2025-12-01.
"sykehuskobling"
