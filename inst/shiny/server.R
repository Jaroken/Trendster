library(magrittr)
library(prophet)
library(data.table)
function(input, output, session) {

output$out  <- shiny::renderText({
  paste(input$keyword)
  })

googleDat<-function(keyword, geo="", time="today+5-y", gprop=c("web", "news", "images", "froogle", "youtube")){
  gtrendsR::gtrends(keyword = keyword, geo=geo, time = time, gprop = gprop)
}


GT<-shiny::eventReactive(input$go, {
  googleDat(keyword = input$keyword, time = paste(input$date1, input$date2), gprop = input$gprop, geo=input$geo)
})


df <- shiny::reactive({
  growth <- input$growth

  if(growth == 'linear'){
    GT<- data.frame(GT()$interest_over_time$date, GT()$interest_over_time$hits)
    colnames(GT)<-c("ds","y")
  }

  if(growth == 'logistic'){
    GT<- data.frame(GT()$interest_over_time$date, GT()$interest_over_time$hits, input$cap)
    colnames(GT)<-c('ds','y','cap')
  }

  GT$ds<- as.Date(GT$ds)
  GT
})

output$holidaytable<-rhandsontable::renderRHandsontable({
  listofholidays <- list("Christmas Eve", "Christmas Day", "Hanukkah", "Kwanzaa", "Boxing Day", "New Year's Eve", "New Year's Day", "Easter", "Halloween", "Valentine's Day", "Mother's Day", "Father's Day")


  ds <- df()$ds[1:input$holidaynum]


  holiday<- vector()
  for(x in 1:input$holidaynum){
  holiday[x] <- paste("holidayExample",x)
  }

  lower_window <- -2
  upper_window <- 2

  if(input$holidaynum==0){
    ds <- NA
    holiday<- NA
    lower_window <- NA
    upper_window <- NA
  }

  holidays<- data.frame(ds, holiday, lower_window, upper_window, stringsAsFactors = FALSE)
  rhandsontable::rhandsontable(holidays, rowHeaders = NULL, width = 550, height = 500) %>%
    rhandsontable::hot_col(col = "ds", type = "autocomplete", source = df()$ds) %>%
    rhandsontable::hot_col(col = "holiday", type = "autocomplete", source = listofholidays, strict = FALSE)


})

output$holidaysinc <- renderText({
  paste(input$holidaynum, " holidays are included in model")
})

output$changepoints<-shiny::renderUI({
  shiny::selectizeInput("changepoints", "Select Changepoints", (GT()$interest_over_time$date), selected=NULL, multiple = TRUE)

})

output$pstart<-shiny::renderUI({
shiny::selectizeInput("pstart", "Pre-period Start", choices=as.Date(GT()$interest_over_time$date), selected = as.Date(GT()$interest_over_time$date)[1])
})
output$pend<-shiny::renderUI({
shiny::selectizeInput("pend", "Pre-period End", choices=as.Date(GT()$interest_over_time$date), selected =as.Date(GT()$interest_over_time$date)[round(nrow(df())*0.70, digits=0)])
})
output$estart<-shiny::renderUI({
shiny::selectizeInput("estart", "Post-period Start", choices=as.Date(GT()$interest_over_time$date), selected =as.Date(GT()$interest_over_time$date)[round(nrow(df())*0.70, digits=0)+1])
})
output$eend<-shiny::renderUI({
shiny::selectizeInput("eend", "Post-period End", choices=as.Date(GT()$interest_over_time$date), selected =as.Date(GT()$interest_over_time$date)[nrow(df())])
})


m<-shiny::reactive({
  holidays<-as.data.frame(rhandsontable::hot_to_r(input$holidaytable))
  if(input$holidaynum==0){
    holidays <- NULL
  }

  prophet::prophet(df=df(), growth = input$growth ,yearly.seasonality=TRUE, mcmc.samples = input$mcmc, changepoint.prior.scale = input$chgprior,
                      changepoints = input$changepoints, uncertainty.samples = input$uncertainty, seasonality.prior.scale = input$season,
                      holidays.prior.scale = input$holiday, holidays = holidays, interval.width = input$intervalwidth)

})

forecast<-shiny::reactive({
  future <- prophet::make_future_dataframe(m=m(), periods = input$timeforward)
  future$cap <- input$cap
  forecast <- predict(m(), future)

})


output$plot<- plotly::renderPlotly({

  df<-c(m(), forecast())
  df<-data.frame(df$ds,df$yhat, c(df$history$y, rep(NA,(length(df$yhat)-length(df$history$y)))), df$yhat_lower,
                 df$yhat_upper)
  names(df)<- c("Date", "Prediction", "Actuals", "Lower Prediction", "Upper Prediction")

  if(is.null(input$changepoints)){
    df$changepoints <- NA
  }

  if(!is.null(input$changepoints)){
    df$changepoints <- df$Date == input$changepoints
  }



  gg<-plotly::plot_ly(df, x = ~Date, y = ~`Lower Prediction`, name='Lower Prediction', type = 'scatter', mode = 'lines', line= list(color = 'rgb(22, 96, 167)'), showlegend = FALSE)%>%
    plotly::add_trace(y = ~`Upper Prediction`, fill='tonexty', name = 'Upper Prediction', mode = 'lines', fillcolor = 'rgba(168, 216, 234, 0.5)', showlegend = FALSE) %>%
    plotly::add_trace(y = ~Prediction, name = 'Prediction', mode = 'lines', line=list(color = 'rgb(255, 16, 67)'), showlegend = FALSE) %>%
    plotly::add_trace(y = ~Actuals, name = 'Actuals',type = 'scatter', mode = 'markers+lines', marker = list(color = 'rgba(67,67,67,1)', size = 8), line=list(color = 'rgb(67, 67, 67)'), showlegend = FALSE)
  plotly::rangeslider(gg)
})

output$plot2 <- shiny::renderPlot({

  prophet_plot_components(m(), forecast())


})

output$dt1 <-DT::renderDataTable({
  as.data.frame(GT()[1])
})

output$dt2 <-DT::renderDataTable({
  as.data.frame(GT()[2])
})
output$dt3 <-DT::renderDataTable({
  as.data.frame(GT()[3])
})
output$dt4 <-DT::renderDataTable({
  as.data.frame(GT()[4])
})
output$dt5 <-DT::renderDataTable({
  as.data.frame(GT()[5])
})
output$dt6 <-DT::renderDataTable({
  as.data.frame(GT()[6])
})

anom<-shiny::reactive({
df  <-GT()
df<-data.frame(df$interest_over_time$date, df$interest_over_time$hits)
names(df) <- c("timestamp", "count")
df$timestamp<-as.POSIXlt(df$timestamp, tz="UTC", "%Y-%m-%d %H:%M:%OS")
df$count<-as.numeric(df$count)

AnomalyDetection::AnomalyDetectionVec(df$count, max_anoms = input$maxanom, period =input$periodicity, plot = TRUE)
})

output$anomplot <- plotly::renderPlotly({
  anom<-anom()$anoms
  date1<-GT()$interest_over_time$date
  hit1<-GT()$interest_over_time$hits
  anom1<- rep(NA, length(date1))
  anom1[anom$index] <- hit1[anom$index]
  df<-data.frame(date1, hit1, anom1)
  colnames(df)<- c("Date","Actuals","Anom")

  line <- list(
    type = "line",
    line = list(color = "grey"),
    xref = "x",
    yref = "y"
  )



  outto <- floor(length(hit1)/input$periodicity)
  lines<-list()
  for(i in 1:outto){
  line[["x0"]] <- df$Date[input$periodicity*i]
  line[["x1"]] <- df$Date[input$periodicity*i]
  line[["y0"]] <- min(as.numeric(hit1))
  line[["y1"]] <- max(as.numeric(hit1))
  lines <- c(lines, list(line))
  }


  plotly::plot_ly(df,x = ~Date, y = ~Actuals, name='Actuals', type = 'scatter', mode = 'lines')%>%
    plotly::add_trace(y = ~Anom, name = 'Anomalies', mode = 'markers')%>%
    plotly::layout(shapes=lines)

  })

output$anomtab <- DT::renderDataTable({
  dt<-anom()$anoms
  dt<-data.table::data.table(dt)
  dt[, Dates := GT()$interest_over_time$date[index]]

  })

Impact <- reactive({
  df  <-GT()
  df<-data.frame(df$interest_over_time$hits)
  preperiod<- c(which(as.Date(GT()$interest_over_time$date) %in% as.Date(input$pstart)) , which(as.Date(GT()$interest_over_time$date) %in%  as.Date(input$pend) ))
  postperiod<- c(which(as.Date(GT()$interest_over_time$date)  %in%  as.Date(input$estart)) , which(as.Date(GT()$interest_over_time$date)  %in% as.Date(input$eend)))

  df<-zoo::zoo(df)
  CausalImpact::CausalImpact(df, preperiod, postperiod)

})

output$causeplot1<-plotly::renderPlotly({

  minmin <- min(min(as.numeric(Impact()$series$pred.effect)),min(as.numeric(Impact()$series$pred.effect.lower)),
                min(as.numeric(Impact()$series$pred.effect.upper)),min(as.numeric(Impact()$series$response)))
  maxmax <- max(max(as.numeric(Impact()$series$pred.effect)),max(as.numeric(Impact()$series$pred.effect.lower)),
                max(as.numeric(Impact()$series$pred.effect.upper)),max(as.numeric(Impact()$series$response)))


  line1 <- list(
    type = "line",
    line = list(color = "orange"),
    xref = "x",
    yref = "y"
  )

    line1[["x0"]] <- as.Date(input$pstart)
    line1[["x1"]] <- as.Date(input$pstart)
    line1[["y0"]] <- min(as.numeric(Impact()$series$response))
    line1[["y1"]] <- max(as.numeric(Impact()$series$response))


  line2 <- list(
    type = "line",
    line = list(color = "orange"),
    xref = "x",
    yref = "y"
  )

    line2[["x0"]] <- as.Date(input$pend)
    line2[["x1"]] <- as.Date(input$pend)
    line2[["y0"]] <- min(as.numeric(Impact()$series$response))
    line2[["y1"]] <- max(as.numeric(Impact()$series$response))


    line3 <- list(
      type = "line",
      line = list(color = "blue"),
      xref = "x",
      yref = "y"
    )

    line3[["x0"]] <- as.Date(input$estart)
    line3[["x1"]] <- as.Date(input$estart)
    line3[["y0"]] <- min(as.numeric(Impact()$series$response))
    line3[["y1"]] <- max(as.numeric(Impact()$series$response))


    line4 <- list(
      type = "line",
      line = list(color = "blue"),
      xref = "x",
      yref = "y"
    )

    line4[["x0"]] <- as.Date(input$eend)
    line4[["x1"]] <- as.Date(input$eend)
    line4[["y0"]] <- min(as.numeric(Impact()$series$response))
    line4[["y1"]] <- max(as.numeric(Impact()$series$response))




  date1<-as.Date(GT()$interest_over_time$date)

  p<- plotly::plot_ly(x= date1, y=Impact()$series$point.pred.lower, name='Lower Prediction', type = 'scatter', mode = 'lines', line= list(color = 'rgb(22, 96, 167)'), showlegend = FALSE)%>%
    plotly::add_trace(y = Impact()$series$point.pred.upper, fill='tonexty', name = 'Upper Prediction', mode = 'lines', fillcolor = 'rgba(168, 216, 234, 0.5)', showlegend = FALSE) %>%
    plotly::add_trace(y = Impact()$series$point.pred, name = 'Prediction', mode = 'lines', line=list(color = 'rgb(255, 16, 67)'), showlegend = FALSE) %>%
    plotly::add_trace(y = Impact()$series$response, name = 'Actuals',type = 'scatter', mode = 'markers+lines', marker = list(color = 'rgba(67,67,67,1)', size = 8), line=list(color = 'rgb(67, 67, 67)'), showlegend = FALSE)%>%
    plotly::layout(shapes=list(line1,line2,line3,line4))


})

output$causeplot2<-plotly::renderPlotly({

  minmin <- min(min(as.numeric(Impact()$series$point.effect)),min(as.numeric(Impact()$series$point.effect.lower)),
                min(as.numeric(Impact()$series$point.effect.upper)),min(as.numeric(Impact()$series$response)))
  maxmax <- max(max(as.numeric(Impact()$series$point.effect)),max(as.numeric(Impact()$series$point.effect.lower)),
                max(as.numeric(Impact()$series$point.effect.upper)),max(as.numeric(Impact()$series$response)))



  line1 <- list(
    type = "line",
    line = list(color = "orange"),
    xref = "x",
    yref = "y"
  )

  line1[["x0"]] <- as.Date(input$pstart)
  line1[["x1"]] <- as.Date(input$pstart)
  line1[["y0"]] <- minmin
  line1[["y1"]] <- maxmax


  line2 <- list(
    type = "line",
    line = list(color = "orange"),
    xref = "x",
    yref = "y"
  )

  line2[["x0"]] <- as.Date(input$pend)
  line2[["x1"]] <- as.Date(input$pend)
  line2[["y0"]] <- minmin
  line2[["y1"]] <- maxmax


  line3 <- list(
    type = "line",
    line = list(color = "blue"),
    xref = "x",
    yref = "y"
  )

  line3[["x0"]] <- as.Date(input$estart)
  line3[["x1"]] <- as.Date(input$estart)
  line3[["y0"]] <- minmin
  line3[["y1"]] <- maxmax

  line4 <- list(
    type = "line",
    line = list(color = "blue"),
    xref = "x",
    yref = "y"
  )

  line4[["x0"]] <- as.Date(input$eend)
  line4[["x1"]] <- as.Date(input$eend)
  line4[["y0"]] <- minmin
  line4[["y1"]] <- maxmax




  date1<-as.Date(GT()$interest_over_time$date)



  p<- plotly::plot_ly(x= date1, y=Impact()$series$point.effect.lower, name='Lower Point Effect', type = 'scatter', mode = 'lines', line= list(color = 'rgb(22, 96, 167)'), showlegend = FALSE)%>%
    plotly::add_trace(y = Impact()$series$point.effect.upper, fill='tonexty', name = 'Upper Point Effect', mode = 'lines', fillcolor = 'rgba(168, 216, 234, 0.5)', showlegend = FALSE) %>%
    plotly::add_trace(y = Impact()$series$point.effect, name = 'Point Effect', mode = 'lines', line=list(color = 'rgb(255, 16, 67)'), showlegend = FALSE) %>%
    plotly::add_trace(y = Impact()$series$response, name = 'Actuals',type = 'scatter', mode = 'markers+lines', marker = list(color = 'rgba(67,67,67,1)', size = 8), line=list(color = 'rgb(67, 67, 67)'), showlegend = FALSE)%>%
    plotly::layout(shapes=list(line1,line2,line3,line4))


})

output$causeplot3<-plotly::renderPlotly({

  minmin <- min(min(as.numeric(Impact()$series$cum.effect)),min(as.numeric(Impact()$series$cum.effect.lower)),min(as.numeric(Impact()$series$cum.effect.upper)))
  maxmax <- max(max(as.numeric(Impact()$series$cum.effect)),max(as.numeric(Impact()$series$cum.effect.lower)),max(as.numeric(Impact()$series$cum.effect.upper)))


  line1 <- list(
    type = "line",
    line = list(color = "orange"),
    xref = "x",
    yref = "y"
  )

  line1[["x0"]] <- as.Date(input$pstart)
  line1[["x1"]] <- as.Date(input$pstart)
  line1[["y0"]] <- minmin
  line1[["y1"]] <- maxmax


  line2 <- list(
    type = "line",
    line = list(color = "orange"),
    xref = "x",
    yref = "y"
  )

  line2[["x0"]] <- as.Date(input$pend)
  line2[["x1"]] <- as.Date(input$pend)
  line2[["y0"]] <- minmin
  line2[["y1"]] <- maxmax


  line3 <- list(
    type = "line",
    line = list(color = "blue"),
    xref = "x",
    yref = "y"
  )

  line3[["x0"]] <- as.Date(input$estart)
  line3[["x1"]] <- as.Date(input$estart)
  line3[["y0"]] <- minmin
  line3[["y1"]] <- maxmax


  line4 <- list(
    type = "line",
    line = list(color = "blue"),
    xref = "x",
    yref = "y"
  )

  line4[["x0"]] <- as.Date(input$eend)
  line4[["x1"]] <- as.Date(input$eend)
  line4[["y0"]] <- minmin
  line4[["y1"]] <- maxmax




  date1<-as.Date(GT()$interest_over_time$date)

  p<- plotly::plot_ly(x= date1, y=Impact()$series$cum.effect.lower, name='Lower Cumulative Effect', type = 'scatter', mode = 'lines', line= list(color = 'rgb(22, 96, 167)'), showlegend = FALSE)%>%
    plotly::add_trace(y = Impact()$series$cum.effect.upper, fill='tonexty', name = 'Upper Cumulative Effect', mode = 'lines', fillcolor = 'rgba(168, 216, 234, 0.5)', showlegend = FALSE) %>%
    plotly::add_trace(y = Impact()$series$cum.effect, name = 'Cumulative Effect', mode = 'lines', line=list(color = 'rgb(255, 16, 67)'), showlegend = FALSE) %>%
    plotly::layout(shapes=list(line1,line2,line3,line4))


})


output$causalTable<-DT::renderDataTable(data.table::data.table((Impact()$summary)), options = list(scrollX = TRUE))

output$causalReport<-shiny::renderText({Impact()$report})



}
