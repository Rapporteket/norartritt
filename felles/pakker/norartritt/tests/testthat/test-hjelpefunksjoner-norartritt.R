# tester hjelpefunksjoner-norartritt.R

context("legg_til_medisinnavn")

# Gir advarsel hvis det finnes en LegemiddelType som ikke har navn i kodebok
# Gir advarsel hvis det finnes flere navn for en LegemiddelType

context("legg_til_sykehusnavn")
# Gir advarsel om det finnes UnitID som ikke har navn i kodebok

context("legg_til_diagnosegrupper")
# Gir advarsel om det finnes diagnosekoder som ikke har navn i kodebok
# Gir advarsel om noen mangler Kode

context("velg_tidligste_inklusjondato")
# Sjekk at NA returnerees hvis det ikke finnes dato på noen av skjema
dato = c("NA", "NA", "2020-11-23", "2020-08-27")
d = tibble::tibble(
  PasientGUID = c("A", "A", "B", "B"),
  InklusjonDato = (as.POSIXct(dato, tz = "UTC", format = "%Y-%m-%d"))
) # Sjekke om det skal være HMS her

d_ut_forventet = tibble::tibble(
  PasientGUID = c("A", "A", "B", "B"),
  InklusjonDato = as.Date(c(NA, NA, "2020-08-27", "2020-08-27"))
)

expect_identical(
  velg_tidligste_inklusjondato(pas_id = PasientGUID, d),
  d_ut_forventet
)

# Sjekk at tidligste tilgjengelige dato velges hvis det også finnes NA
dato = c("NA", "2019-11-12", "2020-11-23", "2020-08-27")
d = tibble::tibble(
  PasientGUID = c("A", "A", "B", "B"),
  InklusjonDato = (as.POSIXct(dato, tz = "UTC", format = "%Y-%m-%d"))
) # Sjekke om det skal være HMS her

d_ut_forventet = tibble::tibble(
  PasientGUID = c("A", "A", "B", "B"),
  InklusjonDato = as.Date(c("2019-11-12", "2019-11-12", "2020-08-27", "2020-08-27"))
)

expect_identical(
  velg_tidligste_inklusjondato(pas_id = PasientGUID, d),
  d_ut_forventet
)

# Sjekk at funksjonen aksepterer annen pas_id
dato = c("NA", "2019-11-12", "2020-11-23", "2020-08-27")
d = tibble::tibble(
  fnr = c(1, 1, 2, 2),
  InklusjonDato = (as.POSIXct(dato, tz = "UTC", format = "%Y-%m-%d"))
)

d_ut_forventet = tibble::tibble(
  fnr = c(1, 1, 2, 2),
  InklusjonDato = as.Date(c("2019-11-12", "2019-11-12", "2020-08-27", "2020-08-27"))
)

expect_identical(
  velg_tidligste_inklusjondato(pas_id = fnr, d),
  d_ut_forventet
)
