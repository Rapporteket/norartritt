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
