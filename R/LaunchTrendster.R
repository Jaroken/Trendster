
#' Trendster
#'
#' @description
#' Many helpful trending packages in one app
#'
#' @import DT
#' @rawNamespace import(shiny, except = c(renderDataTable, dataTableOutput))
#' @import shinydashboard
#' @import prophet
#' @import rhandsontable
#' @import CausalImpact
#' @import plotly
#' @import gtrendsR
#' @import zoo
#' @import magrittr
#' @importFrom magrittr "%>%"
#'
#' @docType package
#' @name Trendster
NULL


#' Function to execute Shiny app
#'
#' @export
shinyApp <- function() { shiny::runApp(system.file('shiny', package='Trendster'), launch.browser = TRUE) }



