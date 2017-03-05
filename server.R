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
      section.in <- input$section
      return(c(year.in, month.in, day.in, section.in))
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
      table.final <- filter(table.final, table.final$Day == parameters[3]) %>% select(-`Day`)
      table.final <- datatable(table.final, escape = FALSE, class = "hover")#, style = "bootstrap"
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
     
     output$plot.trend <- renderPlotly({
       parameters <- input.data()
       news.data <- get.uri.data(parameters[1], parameters[2])
       
       plot.data <- get.news.data() %>%
         select(`Section`, `Day`) %>%
         filter(Section == parameters[4]) %>%
         group_by(Day) %>%
         summarize(`No of Articles` = n())
       
       p <- ggplot(data = plot.data) +
         geom_point(mapping = aes(x = Day, y = `No of Articles`),
                    color = "blue",
                    alpha=0.4, size = plot.data$`No of Articles`) + 
         labs(x = "Day of Month", 
              y = "Number of Articles") +
         theme_minimal() +
         theme(
           axis.text.x = element_blank(),
           axis.text.y = element_blank(),
           axis.ticks = element_blank(),
           panel.grid.minor = element_blank(), 
           panel.grid.major = element_blank(),
           panel.background = element_rect(fill=NA),
           plot.background = element_rect(fill=NA))
       
       ggplotly(p)
     })
     
     output$plot.trendline <- renderPlotly({
       parameters <- input.data()
       news.data <- get.uri.data(parameters[1], parameters[2])
       
       plot.data <- get.news.data() %>%
         select(`Section`, `Day`) %>%
         filter(Section == parameters[4]) %>%
         group_by(Day) %>%
         summarize(`No of Articles` = n())
       
       plot_ly(plot.data, x = ~plot.data$Day, y = ~plot.data$`No of Articles`,
               type = 'scatter', mode = 'lines') %>% 
         layout(title = paste0("Monthwise Trend of ", parameters[4]),
                xaxis = list(title = "Day of Month"), 
                yaxis = list(title = "Number of Articles"))
     })
})

