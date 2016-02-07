library(shiny)
library(scales)
library(bigrquery)
library(ggplot2)
library(dplyr)
library(lubridate)


remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.000001, 0.98), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

project<-"cloud-storage-test-1205"

# server.R
options(scipen=5)

shinyServer(
  function(input, output) {
    
    # Getting the data
    dataMain1 <- eventReactive(input$submit, {
      tableName<-ifelse(input$datePicker[1]>=as.Date("2014-01-01"), "2014_2015","ppcomplete")
      sql <- paste0("SELECT price, dateOfTransfer AS date, PAON AS address1, SAON AS address2, Street, town_city, County, postcode FROM [govdata.",tableName,"] WHERE REGEXP_MATCH(postcode, r'^", 
                 toupper(input$postcodeInput), 
                 "')  LIMIT 10000000")
      results<-query_exec(sql, project = project)
      results$date<-as.POSIXct(results$date)
      results$postcode<-as.factor(results$postcode)
      results<-filter(results,
                    date>as.POSIXct(input$datePicker[1]),
                    date<as.POSIXct(input$datePicker[2]))
      
      if ( length(input$streetInput > 0 ) ) {
        results<-filter(results, grepl(input$streetInput, Street, ignore.case = TRUE) )
      }
      
      results
    })
    
    dataMain<-reactive({
      data<-dataMain1()
      if (input$removeOutliers) {
        data$price<-remove_outliers(data$price)
      }
      data
    })

    plotData <- reactive({
      data2<-dataMain() %>% group_by(date) %>% summarise(avgprice = mean(price), countsale = length(price) ) %>% as.data.frame()
      data2
    })
    
    
    # Average price plot
    output$avgPrice <- renderPlot({
      data<-plotData()
      town<-dataMain()$town_city[1]
      
      
      plot<-ggplot(data, aes(x=date, y=avgprice)) +
        geom_line() +
        ggtitle(paste("Average property sale value in", input$postcodeInput, "(", town, ")")) +
        xlab("Date") +
        ylab("Average price paid (per day), £") + scale_y_continuous(labels=comma)
      if (input$showTrend) {
        plot<-plot+               
          geom_line(stat="smooth", method="loess",
                    size = 1,
                    colour = "#0072B2",
                    alpha = 0.75)
        
      }
      plot
    })
    
    # Sale scatter
    output$saleScatter <- renderPlot({
      data<-dataMain()
      town<-dataMain()$town_city[1]
      plot<-ggplot(data, aes(x=date, y=price)) + 
        geom_point(aes(alpha=0.5), size=0.5) + 
        scale_y_continuous(labels = comma) + 
        ggtitle(paste("Scatterplot of individual sales in", input$postcodeInput, "(", town, ")")) +
        xlab("Date") +
        ylab("Sale price") + theme(legend.position="none")
      
      if (input$showTrend) {
        plot<-plot+               
          geom_line(stat="smooth", method="loess",
                    size = 1,
                    colour = "#0072B2",
                    alpha = 0.75)
      }
      plot
      
    })    
    
    
    # Sale boxplot
    output$saleBoxplot <- renderPlot({
      data<-dataMain()
      town<-dataMain()$town_city[1]
      plot<-ggplot(data, aes(x=1,y= price)) + geom_boxplot() +
        scale_y_continuous(labels = comma) + 
        ggtitle(paste("Boxplot of sale prices")) +
        ylab("Sale price") + coord_flip() 
      plot
      
    })    
    
    output$resultsTable <- renderDataTable({
      tableData<-dataMain()
      tableData<-select(tableData, date, address1, address2, Street, postcode, price)
      tableData<-arrange(tableData, desc(date))
      tableData
    }, options = list(pageLength = 10))
    
    
    
    output$downloadData <- downloadHandler(
      filename = function() { paste0("Property Sales-",input$postcodeInput, ".csv") },
      content = function(file) {
        write.csv(dataMain(), file)
      }) # end downloadData
  })

