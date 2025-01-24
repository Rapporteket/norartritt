
library(tidyverse)
library(rapwhale)
library(norartritt)

les_data_norartritt(dato = "2025-01-17")

# Hent ut alle unike kombinasjoner av pasientGUID og SkjemaGUID fra Inklusjonsskjema.
  # Dette skal i teorien definere alle pasientforløp i registeret.

pasientforløp = d_full_Inklusjonskjema |>
  distinct(PasientGUID, SkjemaGUID) |>
  group_by(PasientGUID) |>
  mutate(antall_forløp = n_distinct(SkjemaGUID)) |>
  arrange(desc(antall_forløp))

# Pasienter med flere inklusjonsskjema:
flere_inklusjon = pasientforløp |>
  filter(antall_forløp > 1)

# Alle unike Pasientforløp
FID = unique(d_full_Inklusjonskjema$SkjemaGUID)

# Hente ut alle hovedskjemaGUID fra alle sideskjema:
alle_sideskjema_hovedskjemaGUID =
  unique(c(
    d_full_Billeddiagnostikkskjema$HovedskjemaGUID,
    d_full_Bivirkningskjema$HovedskjemaGUID,
    d_full_Diagnoseskjema$HovedskjemaGUID,
    d_full_Historiskdose$mrs_HovedskjemaGUID,
    d_full_Infusjonslogg$HovedskjemaGUID,
    d_full_Injeksjonskjema$HovedskjemaGUID,
    d_full_Komorbidtilstandskjema$HovedskjemaGUID,
    d_full_Medisineringskjema$HovedskjemaGUID,
    d_full_Oppfølgingskjema$HovedskjemaGUID,
    d_full_Oppfølgingskjema2$HovedskjemaGUID,
    d_full_Promsinklusjonsskjema$mrs_HovedskjemaGUID)
  )

# Per datadump fra 2025-01-15 er det 43287 FID funnet i sideskjema, men bare
# 42270 FID i hovedskjema.

# Hvilke sykehus finnes i FID-sideskjema men ikke i FID-hovedskjema?
d_full_Billeddiagnostikkskjema |>
  select(PasientGUID,
         SkjemaGUID_billeddiag = SkjemaGUID,
         HovedskjemaGUID_billeddiag = HovedskjemaGUID,
         UnitId_billeddiag = UnitId) |>
  full_join(d_full_Bivirkningskjema |>
              select(PasientGUID,
                     SkjemaGUID_bivirkning = SkjemaGUID,
                     HovedskjemaGUID_bivirkning = HovedskjemaGUID,
                     UnitId_bivirkning = UnitId))


hent_forløpsinfo_sideskjema = function(d) {

  skjemanavn = sym(d)

  skjemanavn |>
    select(PasientGUID, Skjematype, UnitId, SkjemaGUID, HovedskjemaGUID)
}

sideskjema = c("d_full_Billeddiagnostikkskjema",
               "d_full_Bivirkningskjema",
               "d_full_Diagnoseskjema",
               "d_full_Historiskdose",
               "d_full_Infusjonslogg",
               "d_full_Injeksjonskjema",
               "d_full_Komorbidtilstandskjema",
               "d_full_Medisineringskjema",
               "d_full_Oppfølgingskjema",
               "d_full_Oppfølgingskjema2",
               "d_full_Promsinklusjonsskjema")

sideskjema_forløp = tibble::tibble(
  PasientGUID = character(),
  Skjematype = character(),
  UnitId = character(),
  SkjemaGUID = character(),
  HovedskjemaGUID = character()
)

rename_column_if_exists <- function(d, old_name = "mrs_PasientGUID", new_name = "PasientGUID") {
  if (old_name %in% colnames(d)) {
    d <- d %>% rename(!!new_name := !!old_name)
  }
  return(d)
}


for(skjema in sideskjema) {

  d = get(skjema)
  d = rename_column_if_exists(d, old_name = "mrs_PasientGUID", new_name = "PasientGUID")
  d = rename_column_if_exists(d, old_name = "mrs_Skjematype", new_name = "Skjematype")
  d = rename_column_if_exists(d, old_name = "mrs_UnitId", new_name = "UnitId")
  d = rename_column_if_exists(d, old_name = "mrs_SkjemaGUID", new_name = "SkjemaGUID")
  d = rename_column_if_exists(d, old_name = "mrs_HovedskjemaGUID", new_name = "HovedskjemaGUID")

  d_forløp = d |> select(PasientGUID, Skjematype, UnitId, SkjemaGUID, HovedskjemaGUID) |>
    mutate(UnitId = as.character(UnitId))

  sideskjema_forløp = sideskjema_forløp |>
    bind_rows(d_forløp)
}

unike_hovedskjema = sideskjema_forløp |> distinct(HovedskjemaGUID)

manglende_inkl_skjema = unike_hovedskjema$HovedskjemaGUID[which(!d_full_Inklusjonskjema$SkjemaGUID %in% unike_hovedskjema$HovedskjemaGUID)]

d_full_Oppfølgingskjema2[which(!d_full_Oppfølgingskjema2$HovedskjemaGUID %in% d_full_Inklusjonskjema$SkjemaGUID),]

alle_ktrl = d_full_Oppfølgingskjema |> bind_rows(d_full_Oppfølgingskjema2)

alle_ktrl |> distinct(PasientGUID, HovedskjemaGUID, UnitId) |>
  group_by(PasientGUID) |>
  summarise(antall_hovedskjema = n_distinct(HovedskjemaGUID)) |>
  arrange(desc(antall_hovedskjema))

d_alle_hovedskjema = alle_ktrl |>
  distinct(HovedskjemaGUID, .keep_all = TRUE) |>
  select(PasientGUID, HovedskjemaGUID, UnitId)

inkl_guids = d_full_Inklusjonskjema |> distinct(SkjemaGUID) |> pull(SkjemaGUID)

manglende_hovedskjema = d_alle_hovedskjema |>
  filter(!HovedskjemaGUID %in% inkl_guids)

alle_ktrl |> filter(HovedskjemaGUID %in% manglende_hovedskjema$HovedskjemaGUID) |>
  legg_til_sykehusnavn() |>
  count(sykehusnavn)

d_full_Inklusjonskjema |> legg_til_sykehusnavn() |> filter(str_detect(sykehusnavn, "Levanger"))
alle_ktrl |> legg_til_sykehusnavn() |> filter(str_detect(sykehusnavn, "Levanger"))



# -------------------------------------------------------------------------

d_alle_oppf = d_full_Oppfølgingskjema |> bind_rows(d_full_Oppfølgingskjema2)

ids_hovedskjema = d_alle_oppf |> distinct(HovedskjemaGUID) |> pull(HovedskjemaGUID)
ids_inkl = d_full_Inklusjonskjema |> distinct(SkjemaGUID) |> pull(SkjemaGUID)

sum(ids_hovedskjema %in% ids_inkl) # 28870 hovedskjemaGUID finnes i Inklusjonsskjema / 39656
sum(!ids_hovedskjema %in% ids_inkl) # 12493 hovedskjemaGUID finnes ikke i inklusjonsskjema / 64


d_alle_oppf |> filter(!HovedskjemaGUID %in% ids_inkl) |>
  legg_til_sykehusnavn() |>
  count(sykehusnavn)

ids_levanger = d_alle_oppf |> legg_til_sykehusnavn() |> filter(str_detect(sykehusnavn, "Levanger")) |> distinct(PasientGUID) |>  pull(PasientGUID) |> head(10)

d_full_Oppfølgingskjema2 |> filter(PasientGUID %in% ids_levanger)
