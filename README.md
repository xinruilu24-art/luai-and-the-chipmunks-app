# Students & Generative AI: STA304 Shiny App

**Live app:** https://xinruilu815-luai-and-the-chipmunks-app.share.connect.posit.cloud/

An interactive R Shiny website for our STA304 (Fall 2026) group survey on how university students use generative AI for academic work. Readers can filter the data by student status, cGPA band and AI use frequency, change what is plotted, adjust axis ranges and colours, and read a plain-language interpretation next to each result.

## Files

| File | What it is |
|---|---|
| `app.R` | The whole app (user interface and server) |
| `survey_data.csv` | Our cleaned survey data (130 respondents). It contains no names, student numbers or course IDs. |
| `manifest.json` | The list of R packages Posit Connect Cloud installs when it builds the app |

## Run it locally

```r
install.packages(c("shiny", "bslib", "ggplot2", "dplyr", "tidyr", "readr", "DT", "scales", "ggiraph"))
shiny::runApp()
```

Or open `app.R` in RStudio and click **Run App**.
