# test-kvalitetsindikatorer.R

# ki_dapsa()

test_that("ki_dapsa() gjev ut forventa resultat", {
  d_diag = tibble::tibble(
    PasientGUID = as.character(1:5),
    diaggrupper_rem = c(2, 2, 2, 2, 1),
    dato_diag = as.Date(
      c("2022-04-05", "2020-01-15", "2018-10-30", "2012-02-24", "2021-05-17")
    ),
    diag_stilt_aar = lubridate::year(dato_diag),
    dager_diag_til_datadump = as.Date("2022-12-31") - dato_diag
  )

  d_inkl_oppf = tibble::tibble(
    PasientGUID = c("1", "2", "2", "3", "4", "5"),
    dato_ktrl = as.Date(
      c(
        "2022-12-12", "2020-03-01", "2021-01-16",
        "2019-09-30", "2013-02-20", "2022-06-02"
      )
    ),
    DAPSA = c(17.4, 50, 35.2, NA_real_, 5, 10),
    DeathDate = c(rep(NA, 6))
  )

  d_forventa = tibble::tibble(
    PasientGUID = as.character(1:5),
    dato_ktrl = as.Date(
      c(
        "2022-12-12", "2021-01-16", "2019-09-30", "2013-02-20", "2022-06-02"
      )
    ),
    DAPSA = c(17.4, 35.2, NA_real_, 5, 10),
    DeathDate = c(rep(NA, 5)),
    diaggrupper_rem = c(2, 2, 2, 2, 1),
    dato_diag = as.Date(
      c("2022-04-05", "2020-01-15", "2018-10-30", "2012-02-24", "2021-05-17")
    ),
    dager_diag_til_datadump = as.Date("2022-12-31") - dato_diag,
    diag_stilt_aar = lubridate::year(dato_diag),
    dager_siden_diagnose = dato_ktrl - dato_diag,
    ki_aktuell = c(FALSE, TRUE, FALSE, FALSE, FALSE),
    ki_x = DAPSA
  )

  expect_identical(ki_dapsa(d_diag, d_inkl_oppf), d_forventa)
})

# ki_asdas()

test_that("ki_asdas() gjev ut forventa resultat", {
  d_diag = tibble::tibble(
    PasientGUID = as.character(1:5),
    diaggrupper_med = c(5, 5, 5, 5, 1),
    dato_diag = as.Date(
      c("2022-04-05", "2020-01-15", "2018-10-30", "2012-02-24", "2021-05-17")
    ),
    diag_stilt_aar = lubridate::year(dato_diag),
    dager_diag_til_datadump = as.Date("2022-12-31") - dato_diag
  )

  d_inkl_oppf = tibble::tibble(
    PasientGUID = c("1", "2", "2", "3", "4", "5"),
    dato_ktrl = as.Date(
      c(
        "2022-12-12", "2020-03-01", "2021-01-16",
        "2019-09-30", "2013-02-20", "2022-06-02"
      )
    ),
    Asdas = c(2.8, 3.2, 3, NA_real_, 2.5, 3.4),
    DeathDate = c(rep(NA, 6))
  )

  d_forventa = tibble::tibble(
    PasientGUID = as.character(1:5),
    dato_ktrl = as.Date(
      c(
        "2022-12-12", "2021-01-16", "2019-09-30", "2013-02-20", "2022-06-02"
      )
    ),
    Asdas = c(2.8, 3, NA_real_, 2.5, 3.4),
    DeathDate = c(rep(NA, 5)),
    diaggrupper_med = c(5, 5, 5, 5, 1),
    dato_diag = as.Date(
      c("2022-04-05", "2020-01-15", "2018-10-30", "2012-02-24", "2021-05-17")
    ),
    dager_diag_til_datadump = as.Date("2022-12-31") - dato_diag,
    diag_stilt_aar = lubridate::year(dato_diag),
    dager_siden_diagnose = dato_ktrl - dato_diag,
    ki_aktuell = c(FALSE, TRUE, FALSE, FALSE, FALSE),
    ki_x = Asdas
  )

  expect_identical(ki_asdas(d_diag, d_inkl_oppf), d_forventa)
})

# ki_sykmod ---------------------------------------------------------------

# ki_sykmod_snitt -------------------------------------------------------

test_that("ki_sykmod_snitt() gir forventet resultat", {
  # Omfattende testdatasett

  # - Medisin fra liste og utenfor
  # - Kun medisin utenfor liste
  # - Medisin innenfor liste
  # - Ingen medisin

  # - Oppstart samme dag
  # - Håndtering av startdato før diagnosedato
  # - flere grensetilfeller?

  d_test = tibble::tibble(
    pasID = c(1:10),
    dato_diag = c()
  )
})

# ki_remisjon -------------------------------------------------------------


# remisjon_totalt ---------------------------------------------------------


# ki_kontroll -------------------------------------------------------------

test_that("Funksjonen gir forventet resultat", {
  # Må ha dekning for alle ulike potensielle kombinasjoner av inklusjonsskjema,
  # oppfølgingsskjema, diagnosedato, kontrolldato, etc.

  # Pasient inkludert før diagnose
    # oppfølging innen 90
    # oppfølging etter 90
  # Pasient uten inklusjonsskjema
    # oppfølging innen 90
    # oppfølging innen 7 og 90
    # oppfølging > 90
  # Pasient med flere inklusjonsskjema
    # innen 7 og 90
    # etter 7 og før 90
    # etter 7 og etter 90
  # Pasient med inklusjon og oppfølging samme dag
    # og oppfølging innen 90
    # uten oppf innen 90
  # Pasient med Inklusjon innen en uke og
    # oppfølging innen 90
    # oppfølging innen en uke
    # oppfølging over 90 etter diag
  # Pasient med inklusjon mellom 7 og 30 dager,
    # ingen oppfølging,
    # oppfølging innen 30,
    # oppfølging innen 90,
    # oppf etter 90
  # Pasient med inklusjon > 30
    # ingen oppfølging,
    # oppfølging innen 90
    # oppfølging > 90
  # Pasient som har dødd innen 90 men oppfylt krav
  # Pasient som har dødd innen 90 uten å oppfylle krav.



  # Pasient inkludert før diagnose
  # oppfølging innen 90
  # oppfølging etter 90
  d_inkl_oppf_test = tibble(
    PasientGUID = c("1", "1", "1"),
    Skjematype = c("Inklusjonskjema", "Oppfølgingskjema", "Oppfølgingskjema"),
    InklusjonDato = c(lubridate::ymd("2024-01-01", "2024-01-01", "2024-01-01")),
    DeathDate = c(lubridate::ymd(NA, NA, NA)),
    dato_ktrl = c(lubridate::ymd("2024-01-01", "2024-02-01", "2024-06-01"))
  )

  d_diag_test = tibble(
    PasientGUID = "1",
    diaggrupper_med = 1,
    diaggrupper_hoved = 1,
    dato_diag = lubridate::ymd("2024-01-05"),
    diag_stilt_aar = 2024
  )

  d_ki_kontroll_forventet = tibble(
    PasientGUID = "1",
    Skjematype = "Inklusjonskjema",
    InklusjonDato = lubridate::ymd("2024-01-01"),
    DeathDate = lubridate::ymd(NA),
    dato_ktrl = lubridate::ymd("2024-01-01"),
    diaggrupper_med = 1,
    diaggrupper_hoved = 1,
    dato_diag = lubridate::ymd("2024-01-05"),
    diag_stilt_aar = 2024,
    tid_til_inkl = lubridate::as.difftime(-4, units = "days"),
    ki_krit_nevner = FALSE,
    dager_til_ktrl = lubridate::as.difftime(-4, units = "days"),
    ki_krit_teller = FALSE
  )

  testthat::expect_identical(ki_kontroll(d_inkl_oppf_test, d_diag_test),
    expected = d_ki_kontroll_forventet
  )
})

# FIXME - Oppdatere docs til å reflektere at hele inndatasettet blir med ut.
