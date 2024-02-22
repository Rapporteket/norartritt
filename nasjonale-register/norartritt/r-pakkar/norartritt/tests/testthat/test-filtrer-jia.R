
d_inklusjon_test = tibble::tibble(
  PasientGUID = c("a", "b", "c", "d", "e"),
  SkjemaGUID = c("skjema_1", "skjema_2", "skjema_3", "skjema_4", "skjema_5"),
  variabel = c(1,2,3,4,5)
)

d_inkl_resultat = tibble::tibble(
  PasientGUID = c("a", "e"),
  SkjemaGUID = c("skjema_1","skjema_5"),
  variabel = c(1,5)
)

d_oppf_test = tibble::tibble(
  PasientGUID = c("a", "a", "c", "d", "e"),
  HovedskjemaGUID = c("skjema_1", "skjema_1", "skjema_3", "skjema_4", "skjema_5"),
  variabel = c(1,2,3,4,5)
)

d_oppf_resultat = tibble::tibble(
  PasientGUID = c("a", "a", "e"),
  HovedskjemaGUID = c("skjema_1", "skjema_1", "skjema_5"),
  variabel = c(1,2,5)
)

d_diag_test = tibble::tibble(
  PasientGUID = c("a", "a", "b", "c", "d", "e"),
  HovedskjemaGUID = c("skjema_1", "skjema_1", "skjema_2", "skjema_3", "skjema_4", "skjema_4"),
  Navn = c("Juvenil idiopatisk artritt", "Revmatoid artritt", "Juvenil ankyloserende spondylitt",
           "Juvenil seronegativ polyartritt", "Polyartikulær juvenil idiopatisk artritt", "Revmatoid artritt"),
  Kode = c("M080", "M058", "M081", "M083", "M088", "M058")
  )

d_diag_resultat = tibble::tibble(
  PasientGUID = c("a", "e"),
  HovedskjemaGUID = c("skjema_1", "skjema_4"),
  Navn = c("Revmatoid artritt", "Revmatoid artritt"),
  Kode = c("M058", "M058")
)

d_med_test = tibble::tibble(
  PasientGUID = c("a", "b", "c", "d", "d"),
  HovedskjemaGUID = c("skjema_1", "skjema_2", "skjema_3", "skjema_4", "skjema_4"),
  medisin = c("saft", "brus", "øl", "sjokolademelk", "saft")
)

d_med_resultat =  tibble::tibble(
  PasientGUID = c("a"),
  HovedskjemaGUID = c("skjema_1"),
  medisin = c("saft")
)

test_that("Riktige skjema returneres for alle datasett", {

expect_identical(filter_jia(inkl = d_inklusjon_test,
                            oppf = d_oppf_test,
                            diag = d_diag_test,
                            med = d_med_test)[[1]], d_inkl_resultat)

expect_identical(filter_jia(inkl = d_inklusjon_test,
                            oppf = d_oppf_test,
                            diag = d_diag_test,
                            med = d_med_test)[[2]], d_oppf_resultat)

expect_identical(filter_jia(inkl = d_inklusjon_test,
                            oppf = d_oppf_test,
                            diag = d_diag_test,
                            med = d_med_test)[[3]], d_diag_resultat)

expect_identical(filter_jia(inkl = d_inklusjon_test,
                            oppf = d_oppf_test,
                            diag = d_diag_test,
                            med = d_med_test)[[4]], d_med_resultat)
})




