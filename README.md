### Trendster

A shiny dashboard to explore time series packages from Twitter, Facebook, and Google using Google Trends Data. 
These packages perform anomaly detection, forecasting, and causal impact. To get started type in a search term and click the magnifying glass. The app will download Google Trends data into the app and then you can play around with the three different libraries.


### Installation

```{r installation, eval = FALSE}
devtools::install_github("Jaroken/Trendster")
```
  
The 4 main libraries can be installed individually if desired

```{r, eval = FALSE}
install.packages("devtools")
devtools::install_github("twitter/AnomalyDetection")
install.packages("gtrendsR")
install.packages("CausalImpact")
install.packages("prophet"")
```
  
### Gettign started

to launch the app simply put in the following code into R console

```{r}
Trendster::shinyApp()
```


