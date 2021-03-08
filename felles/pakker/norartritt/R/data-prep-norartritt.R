
# Preprossesering av data for NorArtritt

# Lage en funksjon for å forberede alle datasett i NorArtritt for analyse.
# Hovedfokus er for bruk i årsrapport, men jeg antar objektene vil være
# å foretrekke i øvrige analyser også.

# TODO
# Liste opp alt som skal forbedres i alle de ulike objektene
# Generelle ting som skal fikses i flere at objektene
# Funksjonen skal implementeres i les_data_norartritt() slik at en får ut
# både fullstendige datasett og ferdig prosseserte datasett.


# Inklusjon
# (Rett inklusjonsdato) Trekke ut tidligste inklusjonsdato fra inklusjon/oppfølgingsskjema
# (Rett_skjematype) Konvertere første oppfølgingsskjema til inklusjonsskjema.
# Oppfølging
# Medisin
# Diagnose

# Fjerne skjema - eksempel - fjerne diagnose hvis inklusjon mangler.
# Filtrere bort pasienter som ikke har aktuelle diagnoser.
# Har de kun uaktuelle diagnoser skal de ikke være med i registeret i det hele tatt


# Vurdere om det er nødvendig med mini diagnosekodebøker.
# Hent heller inn diagnosegrupper i dataprep


# # Lage datovariabler:
#   # Oppfølging:
#   - aar_ktrl = year(Formdate)
#   - dato_ktrl = as_date(FormDate)
#   # Diagnose:
#   - aar = year(Dato)
#   - dato_diag = as_date(FormDate)
#   - diag_stilt_aar = year(dato_diag)
#   - dager_diag_til_datadump = ymd(dato_datadump)-ymd(dato_diag)
#   # Medisin:
#   - startaar = year(StartDato)
#   - StartDato = as_date(StartDato)
#   - sluttaar = year(SluttDato)
#   - SluttDato = as_date(SluttDato)
#   # Inklusjon:
#   - aar_ktrl = year(InklusjonDato)
#   - dato_ktrl = as_date(InklusjonDato)
#
# # Lage nyttige objekter?
#   #d_med_vasket
#   - Fjerne missing StartDato
#   - Fjerne duplikatregistrering av samme legemiddel på samme StartDato
#     - Prioritere de som har sluttdato av duplikater.
#   # d_diag_pers
#   - kun siste diagnose for hver pasient
#   # d_diag_med
#   - Siste diagnose, hele medisinhistorikken
#   - Legge inn kode 99 for pasienter som ikke går på noen medisin.
#   # d_diag_aktmed
#   - siste diagnose og aktive medisinforløp i rapporteringsår.
#
