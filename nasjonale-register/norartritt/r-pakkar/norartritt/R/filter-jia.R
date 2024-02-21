
#' Fjern Jia fra datasett
#'
#' @description
#' JIA er en diagnosegruppe som er svært liten foreløpig og registeret ønsker
#' ikke å rapportere for denne pasientgruppen foreløpig. Diagnosene kommer
#' også utelukkende fra St. Olavs og helseplattformen, så det er fremdeles
#' usikkert om det vil registreres fra andre sykehus og regioner.
#'
#' Funksjonen fjerner pasienter som kun har disse diagnosene fra
#' inklusjonsskjema, oppfølgingsskjema, diagnoseskjema og medisinskjema.
#'
#' Pasienter som har andre relevante diagnoser i tillegg beholder sine skjema
#' bortsett fra diagnoseskjema relatert til JIA.
#'
#' Filtrering legges inn i `vask_data_norartritt` og kjøres på opprinnelig
#' datasett `d_full_Inklusjonskjema` osv.
#'
#' @param inkl Inklusjonskjema
#' @param oppf Oppfølgingsskjema
#' @param diag Diagnoseskjema
#' @param med Medisinskjema
#'
#' @return
#' Returnerer en liste med filtrerte versjoner av inndata.
#' @export
#'
#' @examples
#' fjern_jia(d_full_Inklusjonskjema, d_full_Oppfølgingskjema,
#' d_full_Diagnoseskjema, d_full_Medisinskjema)
#'
#' @keywords internal
filtrer_jia = function(inkl, oppf, diag, med) {

}
