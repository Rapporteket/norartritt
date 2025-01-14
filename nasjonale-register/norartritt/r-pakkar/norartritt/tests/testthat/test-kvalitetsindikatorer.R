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
