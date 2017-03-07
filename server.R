library("shiny")
library("httr")
library("jsonlite")
library("dplyr")
library("stringr")
library("DT")
library("tidyr")
library("ggplot2")
library("plotly")

shinyServer(
  function(input, output) {
    base.uri.1 <- "http://api.nytimes.com/svc/archive/v1/"
    base.uri.2 <- ".json?api-key="
    api.key <- "3d4af681d88646f5afb0b40f2a1e510b"
    
    input.article <- reactive({
      year.article <- input$year.article
      month.article <- input$month.article
      day.article <- input$day.article
      return(c(year.article, month.article, day.article))
    })
    
    input.viz <- reactive({
      year.viz <- input$year.viz
      month.viz <- input$month.viz
      day.viz <- input$day.viz
      section.viz <- input$section.viz
      return(c(year.viz, month.viz, day.viz, section.viz))
    })
    
    input.pop <- reactive({
      year.pop <- input$year.pop
      month.pop<- input$month.pop
      section1.pop <- input$section1.pop
      section2.pop <- input$section2.pop
      return(c(year.pop, month.pop, section1.pop, section2.pop))
    })
    
    input.top <- reactive({
      year.top <- input$year.top
      month.top<- input$month.top
      return(c(year.top, month.top))
    })
    
    get.uri.data <- function(year, month) {
      uri <- paste0(base.uri.1, year, "/" ,month, base.uri.2, api.key)   
      data <- fromJSON(content(GET(uri), "text"))
      print(uri)
      return(data)
    }
    
    get.news.data <- function(pass.year,pass.month) {
      
      news.data <- get.uri.data(pass.year, pass.month)
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
    
    get.values.data <- function() {
      
      parameters <- input.top()
      data <- as.data.frame(get.uri.data(parameters[1], parameters[2]))
      news.data.table <-  data %>% 
        select(response.docs.keywords)
      
      GetValues <- function(index) {
        keywords <- as.data.frame(news.data.table[[1]][index])
        return(keywords$value)
      }
      
      values <- GetValues()
      
      for (index in 2:nrow(news.data.table)) {
        values <- c(values, GetValues(index))
      }
      values <- as.data.frame(as.list(values))
      cols <- ncol(values)
      values <- gather(values, "Key", "Values", 1:cols) %>% select(Values)
      values <- as.data.frame(table(values$Values))
      colnames(values) <- c("Values", "Frequency")
      
      values <- arrange(values, -Frequency)
      values <- values[1:10, ]
      return (values)
    }
    
    output$table <- renderDataTable({
      
      parameters <- input.article()
      print(parameters)
      table.final <- get.news.data(parameters[1], parameters[2])
      table.final <- filter(table.final, table.final$Day == parameters[3]) %>% select(-`Day`)
      table.final <- datatable(table.final, escape = FALSE, class = "hover")
      return(table.final)
      
    })
    
    output$plot1 <- renderPlotly({
      
      parameters <- input.viz()
      data.plot <- get.news.data(parameters[1], parameters[2])
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
          panel.grid.major = element_blank())
      
      plot <- ggplotly(plot)
      return(plot)
    })
    
    output$plot2 <- renderPlotly({
      
      parameters <- input.viz()
      data.plot <- get.news.data(parameters[1], parameters[2])
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
          panel.grid.major = element_blank())
      
      plot <- ggplotly(plot)
      return(plot)
    })
    
    output$plot3 <- renderPlotly({
      
      parameters <- input.viz()
      data.plot <- get.news.data(parameters[1], parameters[2])
      data.plot <- filter(data.plot, data.plot$Day == parameters[3]) %>% 
        group_by(Section)
      data.plot2 <- summarise(data.plot, 
                              Count = n()) %>% 
        arrange(-Count)
      
      top.10 <- data.plot2[1:10, ] %>% 
        plot_ly(labels = ~Section, values = ~Count) %>% 
        add_pie(hole = 0.5) %>% 
        layout(title = "TOP 10 SECTIONS, DAILY BASIS",  showlegend = F,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      top.10
      
    })
    
    output$plot.trend <- renderPlotly({
      parameters <- input.viz()
      plot.data <- get.news.data(parameters[1], parameters[2]) %>%
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
          axis.ticks = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank())
      ggplotly(p)
    })
    
    output$plot.trendline <- renderPlotly({
      parameters <- input.pop()
      
      GetData <- function(index) {
        plot.data <- get.news.data(parameters[1], parameters[2]) %>%
          select(`Section`, `Day`) %>%
          filter(Section == parameters[index]) %>%
          group_by(Day) %>%
          summarize(`No of Articles` = n())
        colnames(plot.data)[2] <- parameters[index]
        return(plot.data)
      }
      
      plot.data.1 <- GetData(3)
      plot.data.2 <- GetData(4)
      plot.final <- full_join(plot.data.1, plot.data.2)
      
      traces <- parameters[3:4]
      
      plot_ly(plot.final, x = ~plot.final[[1]], y = ~plot.final[[2]],
              type = 'scatter', mode = 'lines', name = traces[1]) %>% 
        add_trace(x = ~plot.final[[1]], y = ~plot.final[[3]], name = traces[2]) %>% 
        layout(title = paste("Popularity of", traces[1], "vs", traces[2]),
               xaxis = list(title = "Day of Month"), 
               yaxis = list(title = "Number of Articles"))
    })
    
    output$plot.values <- renderPlotly ({
      values <- get.values.data()
      JitCoOr <- jitter(as.numeric(factor(values$Values)))
      JitCoOr2 <- jitter(as.numeric(factor(values$Frequency)))
      
      plot <- ggplot(data, aes(x = Frequency, y = Values)) +
        geom_point(data = values, aes(x = JitCoOr2, y = JitCoOr, size = Frequency, color = Values), alpha = .5) +
        geom_text(data = values, aes(x = JitCoOr2, y = JitCoOr, label = Values)) +
        scale_size(range = c(5, 20)) +
        scale_x_discrete(breaks = 1:7, labels = c("Low", " ", " ", "Medium", " ", " ", "High"), limits = 1:7) +
        scale_y_discrete(breaks = 1:10 , labels = as.vector(arrange(values, JitCoOr)[, 1]), limits = 1:10) + 
        theme_bw() +
        labs(title = "Top 10 most-discussed about topics in New York Times", x = "Distribution according to recurrence",
             y = "Topics of Interest", color = "", size = "")
      
      ggplotly(plot) %>% layout(plot, showlegend = FALSE)
    })
  })

