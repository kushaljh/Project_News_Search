#install.packages("DT")
library("shiny")
library("httr")
library("jsonlite")
library("dplyr")
library("stringr")

library("DT")
library("plotrix")

shinyServer(
  function(input, output) {
    
    base.uri.1 <- "http://api.nytimes.com/svc/archive/v1/"
    base.uri.2 <- ".json?api-key="
    api.key <- "3d4af681d88646f5afb0b40f2a1e510b"
  
    input.data <- reactive({
      year.in <- input$year
      month.in <- input$month
      day.in <- input$day
      return(c(year.in, month.in, day.in))
    })

    get.uri.data <- function(year, month) {
       uri <- paste0(base.uri.1, year, "/" ,month, base.uri.2, api.key)   
       data <- fromJSON(content(GET(uri), "text"))
       return(data)
    }
         
    get.news.data <- function() {
      
      parameters <- input.data()
      news.data <- get.uri.data(parameters[1], parameters[2])
      news.data.response <- news.data$response
      news.data.response.docs <- news.data.response$docs
      news.data.response.docs.headline <- news.data.response.docs$headline
      news.data.response.docs.keywords <- news.data.response.docs$keywords
      news.data.table <- data.frame(news.data.response.docs$pub_date,
                                    news.data.response.docs.headline$main, news.data.response.docs$section_name)
      colnames(news.data.table) <- c("Publication Date", "Headline", "Section")
      news.data.table$Headline <- paste0("<a href='",news.data.response.docs$web_url,"'>",news.data.table$Headline,"</a>")
      news.data.table$Section <- str_replace_na(news.data.table$Section, replacement = "Section not defined")
      news.data.table$`Publication Date` <- str_sub(news.data.table$`Publication Date`, 1, 10)
      news.data.table$Day <- str_sub(news.data.table$`Publication Date`, 9, 10)
      return(news.data.table)
    }
   
     output$table <- renderDataTable({
      
      parameters <- input.data()
      table.final <- get.news.data()
      table.final <- filter(table.final, table.final$Day == parameters[3])
      table.final <- datatable(table.final, escape = FALSE)
      return(table.final)
      
    })
     
     output$plot1 <- renderPlotly({
      
       data.plot <- get.news.data()
       data.plot <- group_by(data.plot, Section)
       data.plot2 <- summarise(data.plot, 
                               Count = n()) %>% 
         arrange(-Count)
       
       plot <- ggplot(data = data.plot) +
         geom_bar(mapping = aes(x = Section), fill = rainbow(n = length(data.plot2$Count)))+
         labs(title = "Section Trend, Monthly Basis",
              y = "Count",
              x  = "Section") +
         theme_light() + 
         theme(
           axis.text.x = element_blank(),
           axis.text.y = element_blank(),
           axis.ticks = element_blank(),
           panel.grid.minor = element_blank(), 
           panel.grid.major = element_blank(),
           panel.background = element_rect(fill=NA),
           plot.background = element_rect(fill=NA))
         
       plot <- ggplotly(plot)
       return(plot)
     })
     
     output$plot2 <- renderPlotly({
       
       parameters <- input.data()
       data.plot <- get.news.data()
       data.plot <- filter(data.plot, data.plot$Day == parameters[3]) %>% 
         group_by(Section)
       data.plot2 <- summarise(data.plot, 
                               Count = n()) %>% 
         arrange(-Count)
       
       plot <- ggplot(data = data.plot) +
         geom_bar(mapping = aes(x = Section), fill = rainbow(n = length(data.plot2$Count))) +
         labs(title = "Section Trend, Daily Basis",
              y = "Count",
              x  = "Section") + 
         theme_minimal() +
         theme(
           axis.text.x = element_blank(),
           axis.text.y = element_blank(),
           axis.ticks = element_blank(),
           panel.grid.minor = element_blank(), 
           panel.grid.major = element_blank(),
           panel.background = element_rect(fill=NA),
           plot.background = element_rect(fill=NA)) 
         
       plot <- ggplotly(plot)
       return(plot)
     })
     
     output$plot3 <- renderPlot({
       
       parameters <- input.data()
       data.plot <- get.news.data()
       data.plot <- filter(data.plot, data.plot$Day == parameters[3]) %>% 
         group_by(Section)
       data.plot2 <- summarise(data.plot, 
                               Count = n()) %>% 
         arrange(-Count)
       
       top.10 <- data.plot2[1:10, ]
       plot <- pie3D(top.10$Count,labels = top.10$Section ,explode=0.2,
             main="Daily top 10")
       return(plot)
     })
})

