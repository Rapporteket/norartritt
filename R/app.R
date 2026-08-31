#' Client (ui) for the norartritt app
#'
#' @return An shiny app ui object
#' @export

appUi = function() {

  appTitle = "NorArtritt"

  tagList(
    navbarPage(
      title = regTitle(appTitle),
      windowTitle = appTitle,
      theme = rapTheme(),
      id = "tabs",

      tabPanel(
        title = "Veiledning",
        value = "tab_veiledning",
        navbarWidgetInput("navbar-widget", selectOrganization = TRUE),
        mainPanel(width = 12,
                  htmlOutput("veiledning", inline = TRUE)
        )
      ),
      navbarMenu(
        title = "Rapporter",
        tabPanel(
          title = "Rapport 1",
          value = "tab_rapport_1",
          sidebarLayout(
            sidebarPanel(
              shiny::renderText("SidebarPanel Placeholder")
            ),
            mainPanel(
              shiny::renderText("mainPanel placeholder")
            )
          )
        ),
        tabPanel(
          title = "Rapport 2",
          value = "tab_rapport_2",
          sidebarLayout(
            sidebarPanel(
              shiny::renderText("SidebarPanel Placeholder")
            ),
            mainPanel(
              shiny::renderText("mainPanel placeholder")
            )
          )
        ),
        tabPanel(
          title = "Rapport 3",
          value = "tab_rapport_3",
          sidebarLayout(
            sidebarPanel(
              shiny::renderText("SidebarPanel Placeholder")
            ),
            mainPanel(
              shiny::renderText("mainPanel placeholder")
            )
          )
        ),
        tabPanel(
          title = "Rapport 4",
          value = "tab_rapport_4",
          sidebarLayout(
            sidebarPanel(
              shiny::renderText("SidebarPanel Placeholder")
            ),
            mainPanel(
              shiny::renderText("mainPanel placeholder")
            )
          )
        ),
      ),
      tabPanel(
        title = "Datadump",
        value = "tab_datadump",
        sidebarLayout(
          sidebarPanel(
            width = 4,
            uiOutput("dumpTabControl"),
            dateRangeInput(
              "dumpDateRange",
              "Velg periode:",
              start = ymd(Sys.Date()) - years(1),
              end = Sys.Date(),
              separator = "-",
              weekstart = 1
            ),
            radioButtons(
              "dumpFormat",
              "Velg filformat:",
              choices = list(
                csv = "csv",
                `csv2 (nordisk format)` = "csv2",
                `xlsx-csv` = "xlsx-csv",
                `xlsx-csv2 (nordisk format)` = "xlsx-csv2"
              )
            ),
            downloadButton("dumpDownload", "Hent!")
          ),
          mainPanel(
            htmlOutput("dumpDataInfo")
          )
        )
      ),
      navbarMenu(
          title = "Verktøy",
          tabPanel(
            title = "Metadata",
            value = "tab_metadata",
            sidebarLayout(
              sidebarPanel(uiOutput("metaControl")),
              mainPanel(htmlOutput("metaData"))
            )
          ),
          tabPanel(
            title = "Eksport",
            value = "tab_eksport",
            sidebarLayout(
              sidebarPanel(
                exportUCInput("norartrittExport")
              ),
              mainPanel(
                exportGuideUI("norartrittExportGuide")
              )
            )
          ),
          tabPanel(
            title = "Bruksstatisitkk",
            value = "tab_bruksstatistikk",
            sidebarLayout(
              sidebarPanel(
                statsInput("norartrittStats"),
                statsGuideUI("norartrittStats")
              ),
              mainPanel(
                statsUI("norartrittStats")
              )
            )
          ),
          tabPanel(
            title = "Utsendelser",
            value = "tab_utsendelser",
            sidebarLayout(
              sidebarPanel(
                autoReportFormatInput("norartrittDispatchment"),
                autoReportOrgInput("norartrittDispatchment"),
                autoReportInput("norartrittDispatchment")
              ),
              mainPanel(
                autoReportUI("norartrittDispatchment")
              )
            )
          )
        ) # navbarmenu - Verktøy
    ) # navbarPage
  ) # tagList

}


#' appServer
#'
#' Server logic for the norartritt app
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return A shiny app server object
#' @export
appServer <- function(input, output, session) {

  # logShinyInputChanges(input)

  reportParams <- reactive(
    list(
      hospitalName = hospitalName(),
      reshId = user$org(),
      registryName = registryName(),
      userRole = user$role(),
      userFullName = userFullName,
      shinySession = session
    )
  )

# tabs --------------------------------------------------------------------


# Output ------------------------------------------------------------------

  # Veiledning
  output$veiledning <- renderUI({
    renderRmd(
      system.file("veiledning.Rmd", package = "norartritt"),
      outputType = "html_fragment"
    )
  })
}
