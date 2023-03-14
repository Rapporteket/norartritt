# tester hjelpefunksjoner-norartritt.R

# legg_til_medisinnavn ----------------------------------------------------
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
    legemiddel_navn_kode = c(1L, 2L, 3L),
    LegemiddelType = c(1L, 2L, 3L),
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


# legg_til_sykehusnavn ----------------------------------------------------
test_that("Feilmelding hvis UnitID i datasett ikke finnes i kodebok", {
  d = tibble::tibble(UnitId = c(1, 2, 34))
  feilmelding = "Det mangler kobling for UnitId: 1, 2, 34"

  expect_error(legg_til_sykehusnavn(d),
    error = feilmelding
  )
})

test_that("Leser inn enhetsinformasjon fra sykehusfil som forventet", {
  d = tibble::tibble(UnitId = c(110629L, 102977L, 104579L))

  d_forventet_ut = tibble::tibble(
    UnitId = c(110629L, 102977L, 104579L),
    sykehusnavn = c(
      "Martina Hansens hospital", "Haukeland universitetssykehus",
      "St. Olavs hospital"
    ),
    sykehus_kortnavn = c("MHH", "HUS", "St. Olav"),
    sykehus_gruppe = c(1L, 2L, 3L),
    sykehus_gruppe_navn = c("MHH", "HUS", "St. Olav")
  )

  expect_identical(
    legg_til_sykehusnavn(d),
    d_forventet_ut
  )
})


# legg_til_diagnosegrupper ------------------------------------------------
# Gir advarsel om det finnes diagnosekoder som ikke har navn i kodebok
test_that("Feilmelding hvis diagnosekoder ikke finnes i diagnosekodebok", {
  d = tibble::tibble(
    Kode = c("M7654", "M49494"),
    Navn = c("Ny diagnose", "Narvestad")
  )

  feilmelding = "Kode: M7654, M49494 finnes ikke i diagnosekodebok"

  expect_error(
    legg_til_diagnosegrupper(d),
    feilmelding
  )
})

test_that("Pasienter med Leddsykdom får forventet info fra diagnosekodebok", {
  d = tibble::tibble(
    Kode = c("M123", "M130", NA_character_),
    Navn = c("Palindrom revmatisme", "Polyartritt", "Leddsykdom")
  )

  d_forventet = tibble::tibble(
    Kode = c("M123", "M130", NA_character_),
    Navn = c("Palindrom revmatisme", "Polyartritt", "Leddsykdom"),
    diaggrupper_med = c(8L, 4L, 8L),
    diaggrupper_med_tekst = c(
      "Andre kroniske artritter",
      "Polyartritt",
      "Andre kroniske artritter"
    ),
    diaggrupper_rem = c(3L, 3L, NA_integer_),
    diaggrupper_rem_tekst = c(rep("Andre perifere artritter", 2), NA_character_),
    rem_maal = c(rep("DAS28-CRP", 2), NA_character_),
    diaggrupper_hoved = c(5L, 3L, 5L),
    diaggrupper_hoved_tekst = c(
      "Andre kroniske artritter",
      "Polyartritt",
      "Andre kroniske artritter"
    ),
    perifer_aksial_diaggruppe = c(rep(1L, 2), 3L),
    perifer_aksial_diaggruppe_tekst = c(rep("Perifer", 2), "Udifferensiert")
  )

  expect_identical(
    legg_til_diagnosegrupper(d),
    d_forventet
  )
})

test_that("Leser inn diagnoseinformasjon som forventet", {
  d = tibble::tibble(
    Kode = c("M123", "M130", "M131"),
    Navn = c("Palindrom revmatisme", "Polyartritt", "Monoartritt")
  )

  d_forventet = tibble::tibble(
    Kode = c("M123", "M130", "M131"),
    Navn = c("Palindrom revmatisme", "Polyartritt", "Monoartritt"),
    diaggrupper_med = c(8L, 4L, 4L),
    diaggrupper_med_tekst = c(
      "Andre kroniske artritter",
      "Polyartritt",
      "Polyartritt"
    ),
    diaggrupper_rem = c(3L, 3L, 3L),
    diaggrupper_rem_tekst = c(rep("Andre perifere artritter", 3)),
    rem_maal = c(rep("DAS28-CRP", 3)),
    diaggrupper_hoved = c(5L, 3L, 3L),
    diaggrupper_hoved_tekst = c(
      "Andre kroniske artritter",
      "Polyartritt",
      "Polyartritt"
    ),
    perifer_aksial_diaggruppe = c(rep(1L, 3)),
    perifer_aksial_diaggruppe_tekst = c(rep("Perifer", 3))
  )

  expect_identical(
    legg_til_diagnosegrupper(d),
    d_forventet
  )
})


# velg_tidligste_inklusjonsdato -------------------------------------------
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


# konverter_skjematype ----------------------------------------------------
test_that("Oppfølgingsskjema blir konvertert til inklusjon hvis inklusjon mangler", {
  d_inkl = tibble::tibble(
    PasientGUID = c("a", "b"),
    Skjematype = c("Inklusjonskjema", "Inklusjonskjema"),
    FormTypeId = c(1, 1),
    FormDate = as.POSIXct(c("2021-02-23", "2021-02-23")),
    SkjemaGUID = c("skjema_1", "skjema_2")
  )



  d_oppf = tibble::tibble(
    PasientGUID = c("a", "b", "c"),
    Skjematype = c(
      "Oppfølgingskjema",
      "Oppfølgingskjema",
      "Oppfølgingskjema"
    ),
    FormTypeId = c(2, 2, 2),
    FormDate = as.POSIXct(c(
      "2021-02-24",
      "2021-02-24",
      "2021-02-25"
    )),
    SkjemaGUID = c("skjema_3", "skjema_4", "skjema_5")
  )


  forventet_ut = tibble::tibble(
    PasientGUID = c("a", "b", "c", "a", "b"),
    Skjematype = c(
      "Inklusjonskjema", "Inklusjonskjema",
      "Inklusjonskjema", "Oppfølgingskjema",
      "Oppfølgingskjema"
    ),
    FormTypeId = c(1, 1, 1, 2, 2),
    FormDate = as.POSIXct(c(
      "2021-02-23", "2021-02-23",
      "2021-02-25", "2021-02-24",
      "2021-02-24"
    )),
    SkjemaGUID = c("skjema_1", "skjema_2", "skjema_5", "skjema_3", "skjema_4")
  )



  expect_identical(
    konverter_skjematype(inkl = d_inkl, oppf = d_oppf),
    forventet_ut
  )
})

test_that("Duplikate inklusjonsskjema konverteres til oppfølging", {
  d_inkl_duplikat = tibble::tibble(
    PasientGUID = c("a", "a", "a", "b"),
    Skjematype = c("Inklusjonskjema", "Inklusjonskjema", "Inklusjonskjema", "Inklusjonskjema"),
    FormTypeId = c(1, 1, 1, 1),
    FormDate = as.POSIXct(c("2021-02-23", "2020-01-15", "2021-01-01", "2021-02-23")),
    SkjemaGUID = c("skjema_1", "skjema_2", "skjema_6", "skjema_3")
  )

  d_oppf_ok = tibble::tibble(
    PasientGUID = c("a", "b"),
    Skjematype = c(
      "Oppfølgingskjema",
      "Oppfølgingskjema"
    ),
    FormTypeId = c(2, 2),
    FormDate = as.POSIXct(c(
      "2021-02-24",
      "2021-02-24"
    )),
    SkjemaGUID = c("skjema_4", "skjema_5")
  )

  duplikat_forventet_ut = tibble::tibble(
    PasientGUID = c("a", "b", "a", "a", "a", "b"),
    Skjematype = c(
      "Inklusjonskjema", "Inklusjonskjema",
      "Oppfølgingskjema", "Oppfølgingskjema",
      "Oppfølgingskjema", "Oppfølgingskjema"
    ),
    FormTypeId = c(1, 1, 2, 2, 2, 2),
    FormDate = as.POSIXct(c(
      "2020-01-15", "2021-02-23",
      "2021-02-23", "2021-01-01",
      "2021-02-24", "2021-02-24"
    )),
    SkjemaGUID = c("skjema_2", "skjema_3", "skjema_1", "skjema_6", "skjema_4", "skjema_5")
  )

  expect_identical(
    konverter_skjematype(inkl = d_inkl_duplikat, oppf = d_oppf_ok),
    duplikat_forventet_ut
  )
})


# fjern_uaktuelle_diagnoser -----------------------------------------------
test_that("Uaktuelle diagnoser blir filtrert bort som forventet", {
  diagnose_med_uakt = tibble::tibble(
    pas_id = c(1, 2, 3),
    Navn = c(
      "Revmatoid artritt", "Psoreasisartritt",
      "Artrose"
    )
  )
  diagnose_med_uakt_ut = tibble::tibble(
    pas_id = c(1, 2),
    Navn = c("Revmatoid artritt", "Psoreasisartritt")
  )

  expect_identical(
    fjern_uaktuelle_diagnoser(diagnose_med_uakt),
    diagnose_med_uakt_ut
  )
})

# fjerne_skjema_hjelpefunksjon --------------------------------------------
test_that("skjema filtreres ut som forventet", {
  a = tibble::tibble(PasientGUID = c("1", "2", "3", "4"))
  b = tibble::tibble(PasientGUID = c("1", "2", "3", "5"))

  forventet_ut = b %>%
    filter(dplyr::row_number() %in% 1:3)

  expect_identical(
    fjerne_skjema_hjelpefunksjon(d_hoved = a, d_motpart = b),
    forventet_ut
  )
  expect_identical(
    fjerne_skjema_hjelpefunksjon(d_hoved = a, d_motpart = a),
    a
  )
})
