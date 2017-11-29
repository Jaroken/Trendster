

shinydashboard::dashboardPage(skin = "black",
  shinydashboard::dashboardHeader(titleWidth = 450, title ="Trendster"),



  #### Side Bars ####

  shinydashboard::dashboardSidebar(
    width = 450,
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    shinydashboard::sidebarMenu(
                     shinydashboard::sidebarSearchForm(textId = "keyword", buttonId = "go",label = "Get Data - Search Google Trends"),
                     shinydashboard::menuItem("Google Trends Data", tabName = "gtrends", icon= icon("th"), badgeLabel = "Google",
                                              badgeColor = "green"),
                     shinydashboard::menuItem("Forecast with prophet", icon = icon("th"), tabName = "Forecast", badgeLabel = "Facebook",
                                              badgeColor = "blue"),
                     shinydashboard::menuItem("Anomaly Detection", tabName = "Anom",icon = icon("th"), badgeLabel = "Twitter",
                                              badgeColor = "light-blue"),
                     shinydashboard::menuItem("Causal Impact", tabName = "cause",icon = icon("th"), badgeLabel = "Google",
                                              badgeColor = "green")


    )
  ),
  #### Body of UI ####
  shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      shinydashboard::tabItem(tabName = "gtrends",
            shinydashboard::box(
              title="", status="warning",
              shiny::selectizeInput("gprop", "Select Google Sources", c("web", "news", "images", "froogle", "youtube"), selected =  c("web", "news", "images", "froogle", "youtube"), multiple = TRUE,
                                    options = NULL),
              shiny::selectizeInput("geo", "Specific Region: Default All",
                                    c(as.vector(unique(gtrendsR::countries$country_code)), "") , selected="")
            ),
            shinydashboard::box(
              title="", status="warning",
              shiny::dateInput("date1", "Start Date:", value = Sys.Date()-720),
              shiny::dateInput("date2", "End Date:", value = Sys.Date()-10)
            ),
              shinydashboard::tabBox(
                width = 12,
                selected = "Google 1",
                shiny::tabPanel("Google 1", DT::dataTableOutput("dt1")),
                shiny::tabPanel("Google 2", DT::dataTableOutput("dt2")),
                shiny::tabPanel("Google 3", DT::dataTableOutput("dt3")),
                shiny::tabPanel("Google 4", DT::dataTableOutput("dt4")),
                shiny::tabPanel("Google 5", DT::dataTableOutput("dt5")),
                shiny::tabPanel("Google 6", DT::dataTableOutput("dt6"))
              )
            ),
      shinydashboard::tabItem(tabName = "Forecast",
              shinydashboard::tabBox(
                                width = 12,
              tabPanel("Forecast", plotly::plotlyOutput("plot")),
              tabPanel("Plot Components", shiny::plotOutput("plot2"))
              ),
              shinydashboard::box(
                title = "Inputs", status = "warning", height = 500, width = 12,
                column(width=4,
                       shiny::numericInput("timeforward", label = "Forecast Forward by:", value=30 ),
                       shiny::uiOutput('changepoints'),
                       shiny::numericInput("chgprior","Automatic Changepoints ( More/Less Value = More/Less Changepoints", value = 0.05),
                       shiny::numericInput("holidaynum", "Number of Holidays", value = 0),
                       shiny::textOutput("holidaysinc"),
                       rhandsontable::rHandsontableOutput('holidaytable')),
                column(width=4,
                shiny::numericInput("uncertainty", "# of simulated draws", value=1000),
                shiny::numericInput("intervalwidth", "Width of the uncertainty intervals provided for the forecast", value = 0.8, min = 0, max = 1),
                shiny::numericInput("season", "Strength of the seasonality model", value=10),
                shiny::numericInput("holiday", "Strength of Holiday's in Model (if inc)", value =10)
                ),
                column(width=4,
                       shiny::selectInput("growth", "Specify a linear or logistic trend", choices = c('linear','logistic')),
                       shiny::numericInput("cap", "If Logistic: Set Capacity", value=1),
                       shiny::numericInput("mcmc", "Number of MCMC samples", value=0))

      )),
      shinydashboard::tabItem(tabName = "Anom",
                                shiny::column(width = 6,
                                shiny::sliderInput("maxanom", "Max percent of cases anomalous", min=0, max=1, value = 0.03, step=0.01)),
                                shiny::column(width = 6,
                                shiny::numericInput("periodicity", "Periodicity", value = 12)),
                                shinydashboard::tabBox(width=12,
                                  shiny::tabPanel("Chart",
                                    plotly::plotlyOutput("anomplot")),
                                  shiny::tabPanel("Table",
                                   DT::dataTableOutput("anomtab"))
                              )),
      shinydashboard::tabItem(tabName = "cause",
                              shiny::fluidRow(
                              column(width=3,
                              shiny::uiOutput("pstart")
                              ),
                              column(width=3,
                              shiny::uiOutput("pend")
                              ),
                              column(width=3,
                              shiny::uiOutput("estart")
                              ),
                              column(width=3,
                              shiny::uiOutput("eend")
                              )),
                              shinydashboard::tabBox(width=12,
                                shiny::tabPanel("Charts",
                                    shiny::tabsetPanel(
                                         tabPanel("Prediction",

                              plotly::plotlyOutput("causeplot1")
                              ),
                              shiny::tabPanel("Pointwise",
                                plotly::plotlyOutput("causeplot2")
                              ),
                              shiny::tabPanel("Cumulative",
                                plotly::plotlyOutput("causeplot3")
                              ))),
                              shiny::tabPanel("Report",
                                 DT::dataTableOutput("causalTable"),
                                 shiny::textOutput("causalReport")
                                 )
                               )
      )
      )

)
)


