
#' Trendster
#'
#' @description
#' Many helpful trending packages in one app
#'
#'
#' @import shiny
#' @import shinydashboard
#' @import prophet
#' @import rhandsontable
#' @import CausalImpact
#' @import plotly
#' @import gtrendsR
#' @import zoo
#' @import dplyr
#' @import magrittr
#' @import AnomalyDetection
#' @import Rcpp
#' @import rstan
#'
#' @docType package
#' @name Trendster
NULL

#' @export
shinyApp <- function() { shiny::runApp(system.file('shiny', package='Trendster'), launch.browser = TRUE) }



