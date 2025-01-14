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
    LegemiddelType = c(1, 2, 3, 38),
    Legemiddel = c(NA, NA, NA, NA)
  )

  d_ut_forventet = tibble::tibble(
    legemiddel_navn_kode = c(1L, 2L, 3L, 38L),
    LegemiddelType = c(1L, 2L, 3L, 38L),
    legemiddel_navn = c(
      "Cimzia (certolizumab pegol)",
      "Enbrel (etanercept)",
      "Humira (adalimumab)",
      "Hyrimoz (adalimumab)"
    ),
    biokat = c(1L, 1L, 1L, 1L),
    dmard = c(1L, 1L, 1L, 1L),
    csdmard = c(0L, 0L, 0L, 0L),
    tsdmard = c(0L, 0L, 0L, 0L),
    bio_og_tsdmard = c(1L, 1L, 1L, 1L),
    legemiddel_gruppert = c(1L, 2L, 3L, 3L),
    legemiddel_gruppert_navn = c(
      "Cimzia (certolizumab pegol)",
      "etanercept",
      "adalimumab",
      "adalimumab"
    ),
    Virkestoff = c(
      "certolizumab pegol",
      "etanercept",
      "adalimumab",
      "adalimumab"
    ),
    Kommentar = c(
      rep(NA_character_, 4)
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

  expect_error(legg_til_sykehusnavn(d), feilmelding)
})

test_that("Leser inn enhetsinformasjon fra sykehusfil som forventet", {
  d = tibble::tibble(UnitId = c(110629L, 102977L, 104579L))

  d_forventet_ut = tibble::tibble(
    UnitId = c(110629L, 102977L, 104579L),
    sykehusnavn = c(
      "Martina Hansens hospital", "Haukeland universitetsjukehus",
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
    pas_id = c(1, 2, 3, 4, 5),
    Navn = c(
      "Revmatoid artritt", "Psoriasisartritt",
      "Artrose", "Polyartritt", "Septisk Artritt"
    ),
    Kode = c("M058", "M073", NA_character_, "M130", NA_character_)
  )
  diagnose_med_uakt_ut = tibble::tibble(
    pas_id = c(1, 2),
    Navn = c("Revmatoid artritt", "Psoriasisartritt"),
    Kode = c("M058", "M073")
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


# konverter_missing_til_na ------------------------------------------------
test_that("Konverterer 0 og -1 til NA som forventet ", {
  d = tibble::tibble(
    BASDAI = c(4.1, 0, 0, 0, 0, 0.7),
    Das28 = c(1.63, -1, 3.64, -1, -1, -1),
    Das283 = c(1.21, -1, 3.2, -1, -1, -1),
    Das28Crp = c(1.87, -1, 3.01, -1, -1, 1.48),
    Das28Crp3 = c(1.42, -1, 2.49, -1, -1, 1.59),
    Cdai = c(5.2, -1, 9, -1, 26.9, 1.2),
    Sdai = c(5.3, -1, 9.1, -1, -1, 1.4),
    Asdas = c(1.84, -1, -1, -1, -1, 0.94),
    DAPSA = c(-1, -1, 1.2, 1.4, -1, -1),
    Total = c(-1, -1, 0.12, 0.88, 0.5, -1),
    KlePaaSelv = c(-1L, 2L, 3L, -1L, 2L, 3L),
    OppISengen = c(1L, 2L, -1L, 1L, 2L, 3L),
    LofteKopp = c(1L, -1L, 3L, -1L, 2L, 3L),
    Utendors = c(-1L, 2L, 3L, -1L, 2L, 3L),
    Vaske = c(1L, 2L, 3L, -1L, 2L, -1L),
    Boye = c(1L, 2L, 3L, -1L, 2L, 3L),
    Skru = c(1L, 2L, 3L, -1L, 2L, 3L),
    KommeInn = c(1L, 2L, 3L, -1L, 2L, 3L),
    Rand12Q01 = c(-1L, 1L, 2L, 3L, 4L, 5L),
    Rand12Q02 = c(-1L, 1L, 2L, 3L, 1L, 2L),
    Rand12Q03 = c(-1L, 1L, 2L, 1L, 2L, -1L),
    Rand12Q04 = c(-1L, 1L, 2L, 1L, 2L, -1L),
    Rand12Q05 = c(-1L, 1L, 2L, 1L, 2L, -1L),
    Rand12Q06 = c(-1L, 1L, 2L, 1L, 2L, -1L),
    Rand12Q07 = c(-1L, 1L, 2L, 1L, 2L, -1L),
    Rand12Q08 = c(-1L, 1L, 2L, 3L, 4L, 5L),
    Rand12Q09 = c(-1L, 1L, 2L, 3L, 4L, 5L),
    Rand12Q10 = c(-1L, 1L, 2L, 3L, 4L, 5L),
    Rand12Q11 = c(-1L, 1L, 2L, 3L, 4L, 5L),
    Rand12Q12 = c(-1L, 1L, 2L, 3L, 4L, 5L)
  )

  d_forventet = tibble::tibble(
    BASDAI = c(
      4.1, NA_real_, NA_real_,
      NA_real_, NA_real_, 0.7
    ),
    Das28 = c(
      1.63, NA_real_, 3.64,
      NA_real_, NA_real_, NA_real_
    ),
    Das283 = c(
      1.21, NA_real_, 3.2,
      NA_real_, NA_real_, NA_real_
    ),
    Das28Crp = c(
      1.87, NA_real_, 3.01,
      NA_real_, NA_real_, 1.48
    ),
    Das28Crp3 = c(
      1.42, NA_real_, 2.49,
      NA_real_, NA_real_, 1.59
    ),
    Cdai = c(
      5.2, NA_real_, 9,
      NA_real_, 26.9, 1.2
    ),
    Sdai = c(
      5.3, NA_real_, 9.1,
      NA_real_, NA_real_, 1.4
    ),
    Asdas = c(
      1.84, NA_real_, NA_real_,
      NA_real_, NA_real_, 0.94
    ),
    DAPSA = c(
      NA_real_, NA_real_, 1.2,
      1.4, NA_real_, NA_real_
    ),
    Total = c(
      NA_real_, NA_real_, 0.12,
      0.88, 0.5, NA_real_
    ),
    KlePaaSelv = c(
      NA_integer_, 2L, 3L,
      NA_integer_, 2L, 3L
    ),
    OppISengen = c(
      1L, 2L, NA_integer_,
      1L, 2L, 3L
    ),
    LofteKopp = c(
      1L, NA_integer_, 3L,
      NA_integer_, 2L, 3L
    ),
    Utendors = c(
      NA_integer_, 2L, 3L,
      NA_integer_, 2L, 3L
    ),
    Vaske = c(
      1L, 2L, 3L,
      NA_integer_, 2L, NA_integer_
    ),
    Boye = c(
      1L, 2L, 3L, NA_integer_,
      2L, 3L
    ),
    Skru = c(
      1L, 2L, 3L, NA_integer_,
      2L, 3L
    ),
    KommeInn = c(
      1L, 2L, 3L,
      NA_integer_, 2L, 3L
    ),
    Rand12Q01 = c(
      NA_integer_, 1L, 2L,
      3L, 4L, 5L
    ),
    Rand12Q02 = c(
      NA_integer_, 1L, 2L,
      3L, 1L, 2L
    ),
    Rand12Q03 = c(
      NA_integer_, 1L, 2L,
      1L, 2L, NA_integer_
    ),
    Rand12Q04 = c(
      NA_integer_, 1L, 2L,
      1L, 2L, NA_integer_
    ),
    Rand12Q05 = c(
      NA_integer_, 1L, 2L,
      1L, 2L, NA_integer_
    ),
    Rand12Q06 = c(
      NA_integer_, 1L, 2L,
      1L, 2L, NA_integer_
    ),
    Rand12Q07 = c(
      NA_integer_, 1L, 2L,
      1L, 2L, NA_integer_
    ),
    Rand12Q08 = c(
      NA_integer_, 1L, 2L,
      3L, 4L, 5L
    ),
    Rand12Q09 = c(
      NA_integer_, 1L, 2L,
      3L, 4L, 5L
    ),
    Rand12Q10 = c(
      NA_integer_, 1L, 2L,
      3L, 4L, 5L
    ),
    Rand12Q11 = c(
      NA_integer_, 1L, 2L,
      3L, 4L, 5L
    ),
    Rand12Q12 = c(
      NA_integer_, 1L, 2L,
      3L, 4L, 5L
    )
  )

  expect_identical(konverter_missing_til_na(d), d_forventet)
})
