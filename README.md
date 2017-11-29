Tr### Trendster

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
Trendster::RunShinyApp()
```

Then in the text search field in the top left (on the sidebar) you type in the search term you want to explore. Click on the magnifying glass to initialize the search. If no data appears in the Google Trends tabs your search may have been too specific. Your data is loaded once the Google1 tab has generated a table with the trends data. You can then move over to the tabs for the various packages (e.g. Causal Impact).

