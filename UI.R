1# ui.R

shinyUI(fluidPage(
  
  titlePanel("UK property sales"),
  
  sidebarLayout(
    sidebarPanel( 
      
      p("View property sale information in the UK"),
      tags$small("For best results, use the first prefix of the postcode (e.g. 'HP12') only. Full postcodes may not match enough data to display, but can still be downloaded when the query has completed."),
      br(),
      tags$small("The earliest data available starts at 1995."),
      br(),br(),
      
      textInput("postcodeInput", 
                  label = "Please enter a postcode",
                  value = "HP12"),
      
      dateRangeInput("datePicker", 
                     label = "Select Date range", 
                     start = "2014-01-01", 
                     end = "2015-12-31", min = NULL, max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en", separator = " to ", width = NULL),
      
      textInput("streetInput", 
                label = "[Optional] Filter by street name"),
      
      p(tags$b("Click 'Find Results' to refresh your query")),
      actionButton("submit", "Find Results"),
      
      checkboxInput("showTrend", 
                    label = "Show Trend",
                    value = FALSE),
      
      checkboxInput("removeOutliers", 
                    label = "Remove Outliers",
                    value = FALSE),
      br(),br(),
      downloadButton('downloadData', 'Download Results')
      
    ),
    
    mainPanel(
        tabsetPanel(
          tabPanel("Graphs",fluidRow(plotOutput("avgPrice", height=220)),
                   fluidRow(plotOutput("saleScatter", height=220)),
                   fluidRow(plotOutput("saleBoxplot", height=200))),
          tabPanel("Results Table", dataTableOutput("resultsTable"))
        )
    )
  )
))
