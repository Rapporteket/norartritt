

#' Henter organisasjonstre fra accessunits
#'
#' @description
#' Henter ut UnitId og Title fra accessUnits-tabell i norartritt database.
#' Brukes for å filtrere data basert på tildelt tilgang.
#' Inneholder flere enheter som ikke aktivt skal brukes. Enkelte enheter har
#' også flere tilknyttede UnitId, så dette må tas høyde for.
#' Har også med HF-nivå her men data registreres ikke direkte på disse UnitID.
#' Kan eventuelt brukes for å lage et øvre rapporteringsnivå.
#' Orgnummer er også tilgjengelig og vil være nyttig for å overføre data til
#' behandlingskvalitet.
#'
#' @returns
#' Tibble med alle UnitId og Title for registrerte enheter.
#'
#' @examples
#' get_org_access()
get_org_access = function() {

con = rapbase::rapOpenDbConnection("norartritt")
d_units = DBI::dbGetQuery(con$con, "SELECT UnitId, Title FROM accessunits") |>
  tibble::as_tibble()
rapbase::rapCloseDbConnection(con$con)

d_units
}





get_org_access()


