library("shiny")
library("httr")
library("jsonlite")
library("dplyr")
library("stringr")

#my.server <- 
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
         
    
    output$table <- renderDataTable({
      
      parameters <- input.data()
      
      news.data <- get.uri.data(parameters[1], parameters[2])
      news.data.response <- news.data$response
      news.data.response.docs <- news.data.response$docs
      news.data.response.docs.headline <- news.data.response.docs$headline
      news.data.response.docs.keywords <- news.data.response.docs$keywords
      news.data.table <- data.frame(news.data.response.docs$pub_date,
                              news.data.response.docs.headline$main, news.data.response.docs$section_name)
      colnames(news.data.table) <- c("Publication Date", "Headline", "Section")
      View(news.data.table)
      news.data.table$Headline <- paste0("<a href='",news.data.response.docs$web_url,"'>",news.data.table$Headline,"</a>")
      news.data.table$Section <- str_replace_na(news.data.table$Section, replacement = "Section not defined")
      news.data.table$`Publication Date` <- str_sub(news.data.table$`Publication Date`, 1, 10)
      news.data.table$Day <- str_sub(news.data.table$`Publication Date`, 9, 10)
      news.data.table.final <- filter(news.data.table, news.data.table$Day == parameters[3])
      
      return(news.data.table.final)
    })
    
})

