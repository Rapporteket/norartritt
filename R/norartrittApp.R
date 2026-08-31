#' Run the NorArtritt Shiny Application
#'
#'
#' @return An object representing the NorArtritt app
#' @export
norartrittApp <- function() {

  rapbase::loggerSetup()

  shiny::shinyApp(
    ui = appUi,
    server = appServer
  )
}
