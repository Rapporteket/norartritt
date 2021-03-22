# tester hjelpefunksjoner-norartritt.R

context("legg_til_medisinnavn")

test_that("Advarsel hvis data inneholder LegemiddelType som ikke har navn i kodebok", {
  d = tibble::tibble(
    LegemiddelType = c(1, 2, 3, 765),
    Legemiddel = c(NA, NA, NA, NA)
  )

  expect_error(
    legg_til_medisinnavn(d),
    "LegemiddelType 765 er ikke definert i medisinkodeboken"
  )
})

test_that("Leser inn data fra medisinkodebok som forventet", {
  d = tibble::tibble(
    LegemiddelType = c(1, 2, 3),
    Legemiddel = c(NA, NA, NA)
  )

  d_ut_forventet = tibble::tibble(
    LegemiddelType = c(1, 2, 3),
    Legemiddel = c(NA, NA, NA),
    legemiddel_navn_kode = c(1L, 2L, 3L),
    legemiddel_navn = c(
      "Cimzia (certolizumab pegol)",
      "Enbrel (etanercept)",
      "Humira (adalimumab)"
    ),
    biokat = c(1L, 1L, 1L),
    dmard = c(1L, 1L, 1L),
    csdmard = c(0L, 0L, 0L),
    tsdmard = c(0L, 0L, 0L),
    bio_og_tsdmard = c(1L, 1L, 1L),
    legemiddel_gruppert = c(1L, 2L, 3L),
    legemiddel_gruppert_navn = c(
      "Cimzia (certolizumab pegol)",
      "Enbrel (etanercept)",
      "Humira (adalimumab)"
    ),
    Virkestoff = c(
      "certolizumab pegol",
      "etanercept",
      "adalimumab"
    )
  )

  expect_identical(
    legg_til_medisinnavn(d),
    d_ut_forventet
  )
})

context("legg_til_sykehusnavn")
# Gir advarsel om det finnes UnitID som ikke har navn i kodebok

context("legg_til_diagnosegrupper")
# Gir advarsel om det finnes diagnosekoder som ikke har navn i kodebok
# Gir advarsel om noen mangler Kode

context("velg_tidligste_inklusjondato")

test_that("NA returneres hvis ingen inklusjonsdato finnes", {
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
})

test_that("Tidligste dato returneres hvis det også finnes NA", {
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
})

# Sjekk at funksjonen aksepterer annen pas_id
test_that("Fungerer med alternativ pasientidentifikator", {
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
})
