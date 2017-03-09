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
    
    data.viz <- reactive({
      parameters <- input.viz()
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
    })
    
    input.pop <- reactive({
      year.pop <- input$year.pop
      month.pop<- input$month.pop
      section1.pop <- input$section1.pop
      section2.pop <- input$section2.pop
      return(c(year.pop, month.pop, section1.pop, section2.pop))
    })
    
    # Returns the input month and the input year for the 'Popular Topics' visualisation
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
      # Function to get the summary of top 10 news values from all the articles 
      # published in the input month and year
      get.values.data <- function() {
      
      parameters <- input.top()
      data <- as.data.frame(get.uri.data(parameters[1], parameters[2]))
      news.data.table <-  data %>% 
        select(response.docs.keywords)
      
      # Function to return values 
      GetValues <- function(index) {
        keywords <- as.data.frame(news.data.table[[1]][index])
        return(keywords$value)
      }
      
      values <- GetValues(1)
      
      # Stores all the values
      for (index in 2:nrow(news.data.table)) {
        values <- c(values, GetValues(index))
      }
      values <- as.data.frame(as.list(values))
      cols <- ncol(values)
      values <- gather(values, "Key", "Values", 1:cols) %>% select(Values)
      values <- as.data.frame(table(values$Values))
      colnames(values) <- c("Values", "Frequency")
      
      # Arranges the values according to their frequency of occurence in descending order 
      # and returns the top 10 of those values
      values <- arrange(values, -Frequency)
      values <- values[1:10, ]
      return (values)
    }
    
    output$table <- renderDataTable({
      
      parameters <- input.article()
      table.final <- get.news.data(parameters[1], parameters[2])
      table.final <- filter(table.final, table.final$Day == parameters[3]) %>% select(-`Day`)
      table.final <- datatable(table.final, escape = FALSE, class = "hover")
      return(table.final)
      
    })
    
    output$plot1 <- renderPlotly({
      
      data.plot <- data.viz()
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
      
      plot <- ggplotly(plot) %>% 
        layout(autosize = F, width = 900, height = 500)
      plot
    })
    
    output$plot2 <- renderPlotly({
      
      parameters <- input.viz()
      data.plot <- data.viz()
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
      
      plot <- ggplotly(plot) %>% 
        layout(autosize = F, width = 900, height = 500)
      
      plot
    })
    
    output$plot3 <- renderPlotly({
      
      parameters <- input.viz()
      data.plot <- data.viz()
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
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               autosize = F, width = 900, heigth = 500)
      top.10
      
    })
    
    output$plot.trend <- renderPlotly({
      # Render function to plot the popularity of a section over a month
      
      # Storing the different parameters to be used in the function
      parameters <- input.viz()
      
      # Storing the data required for the plot to a data frame
      plot.data <- data.viz() %>% 
        select(`Section`, `Day`) %>%
        filter(Section == parameters[4]) %>%
        group_by(Day) %>%
        summarize(`No of Articles` = n())
      
      # Creating a scatter plot and storing it to a variable
      p <- ggplot(data = plot.data) +
        geom_point(mapping = aes(x = Day, y = `No of Articles`),
                   color = "blue",
                   alpha=0.4, size = 5) + 
        labs(x = "Day of Month", 
             y = "Number of Articles") +
        theme_minimal() +
        theme(
          axis.ticks = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank())
      
      # Making the graph interactive
      p <- ggplotly(p) %>% 
        layout(autosize = F, width = 900, height = 500)
      
      # Plotting the graph
      p
    })
    
    output$plot.trendline <- renderPlotly({
      # Render function to render comparison chart of sections selected as input
      
      # Storing the different parameters to be used in the function
      parameters <- input.pop()
      
      GetData <- function(index) {
        # Function to filter data and select the column of interest 
        # (column corresponding to the input index)
        plot.data <- get.news.data(parameters[1], parameters[2]) %>%
          select(`Section`, `Day`) %>%
          filter(Section == parameters[index]) %>%
          group_by(Day) %>%
          summarize(`No of Articles` = n())
        colnames(plot.data)[2] <- parameters[index]
        return(plot.data)
      }
      
      # Storing data of input sections in variables
      plot.data.1 <- GetData(3)
      plot.data.2 <- GetData(4)
      
      # Storing the data for the different sections to one data frame.
      plot.final <- full_join(plot.data.1, plot.data.2)
      
      # Storing the names of the sections selected
      traces <- parameters[3:4]
      
      # Plotting an interactive comparison line graph of the sections selected
      plot_ly(plot.final, x = ~plot.final[[1]], y = ~plot.final[[2]],
              type = 'scatter', mode = 'lines', name = traces[1]) %>% 
        add_trace(x = ~plot.final[[1]], y = ~plot.final[[3]], name = traces[2]) %>% 
        layout(title = paste("Popularity of", traces[1], "vs", traces[2]),
               xaxis = list(title = "Day of Month"), 
               yaxis = list(title = "Number of Articles"),
               autosize = F, width = 900, height = 500)
    })
    
    # Render function to visualise the top 10 popular topics for an input month and year
    output$plot.values <- renderPlotly ({
      values <- get.values.data()
      
      # Factorises the values and frequencies so as to enable relative sizing
      # while plotting 
      JitCoOr <- jitter(as.numeric(factor(values$Values)))
      JitCoOr2 <- jitter(as.numeric(factor(values$Frequency)))
      
      # Creates and stores a scatter plot 
      plot <- ggplot(data, aes(x = Frequency, y = Values)) +
        geom_point(data = values, aes(x = JitCoOr2, y = JitCoOr, size = Frequency, color = Values), alpha = .5) +
        scale_size(range = c(5, 20)) +
        scale_x_discrete(breaks = 1:7, labels = c("Low", " ", " ", "Medium", " ", " ", "High"), limits = 1:7) +
        scale_y_discrete(breaks = 1:10 , labels = as.vector(arrange(values, JitCoOr)[, 1]), limits = 1:10) + 
        theme_bw() +
        labs(title = "Top 10 most-discussed about topics in New York Times", x = "Distribution according to recurrence",
             y = "Topics of Interest", color = "", size = "")
      
      # Plots the scatter plot on a graph and customises its layout 
      ggplotly(plot) %>% 
        layout(plot, showlegend = FALSE, hovermode = FALSE,
               autosize = F, width = 900, height = 500)
    })
    
    output$textInfo <- renderText({
      
      "<h1>Introduction</h1>
      <h4>
      <p>Nowadays, when we are always looking forward to something new, always looking for new 
        technology, we might want to rest a minute and look back to what we have been through. 
        Many of the times, ideas are inspired by an incident from the past. We decided that it 
        will be fascinating if we could build a web app for people to search back in time, a 
        time that is special or of interest to them. With this app, an individual will be able to 
        know a day back in time when he or she was not even born, or she could revisit a day that 
        means a lot to her.
      </p>
      <p>
        The dataset we used is the New York Times Archive API. NYT Archive API is a great data 
        source because New York Times itself covers news and articles from worldwide, and is a 
        rather believable source. With NYT data, we will be able to cover a wide range of events 
        or incidents around the world on that day. NYT Archive API is very neatly organized, and 
        contains enough information for our web apps, accordingly, it contains the dates that an 
        article was posted. NYT API is categorized into sections like movies, books, top searched 
        etc. We used the Archive API that provides lists of NYT articles by a month and goes back 
        all the way to 1851. The archive API is similar to the article search API where we can 
        simply pass the year and month we want, and the API will return a JSON object with 
        articles of that month.
      </p>
      <p>
        In order to access this API, we signed up for an API key and acknowledged that it is 
        limited to 2000 requests per day. Though there is a limitation to access to this data, 
        it will be enough for our current use and web building.
      </p>
      <p>
        From the API, the columns we extracted from the dataset were: snippet, main headline, 
        section, keywords, as well as the URL source. On our web page, we have five different 
        tabs that take in different data inputs. The first tab is an About page that explains 
        our web page. The next tab is an article search where the user will be able to select 
        articles by date. A result will show indicating the publication date, headline, and 
        section. By clicking on the headline of each result, a new page will open displaying 
        the full article. Users will be about to change how many entries they want to see on 
        this page, and there is also a search bar for interest searches.\"
      </p>
      <p>
        The second tab shows the Section Visualization. This part includes four visualizations 
        that are organized by Monty, Day, Top 10, and Popularity. Visualizations use colors to 
        indicate different sections and displayed using hover-interactive bar plot, pie plot, 
        and dot plot.
      </p>
      <p>
        The third tab indicates section comparison where users will be able to choose two 
        sections they want to compare. Our comparison visualization is indicated by a line, 
        which clear show the comparing trend of the two sections. Each line represents the 
        number of articles posted according to the day of the month. The line graph is also 
        hovered interactive, showing major points and line labels.
      </p>
      <p>
        The last tab gives a dot visualization on ten of the most discussed topics of the 
        month of interest. Users will be able to choose a month from the sidebar and visualize 
        the ten most discussed topics of that month. The popularity of each topic will be 
        indicated by the size of the dot, the bigger the dot, the more popular the topic.
      </p>
      <p>
        Our goal for this project is not just building a good looking app, we want our app 
        to be as useful to the society as possible. With our app, people will be able to learn 
        from the past, revisits and remember memorable moments from their lives. We are currently 
        in a world with lots of issues, people might feel overwhelmed, but in fact, it has always 
        been like this. Let's look back and see how things were resolved. We have made great 
        improvements till this point today, let's have more faith going towards tomorrow.
      </p>
      <p>
        All the visualizations are dated from 1981 to the current date before the ection parameter was not build
        by New York times before 1981. Also, data for years from 2006 - 2007 is broken, so it gives error.
      </h4>
      <h3 style=\"font-weight:normal;\">Developers: Manesh Jhawar, Kushal Jhunjhunwalla, Helly Shah, Alice Li</h3>
      <h3 style=\"font-weight:normal;\">Powered by : NYT Developers API</h3>
      "
    })
    
    output$articleText <- renderText({
     "<h4> 
      This feature lets you search all the articles available for the given year, month and day in a tabular format.
      <br>
      The table consists of the exact data of publication of the article, the hedline and its section name.
      <br>
      On clicking the headline, it redirects the user to the NYT's article page.
      </h4>"
    })
    
    output$plot1Text <- renderText({
     "<h4>
      The following grapph shows the number of different sections for the given year and month in a bar graph format.
      Differnet colors reflect different sections. Hover over the graph for mor e information.
      </h4>"
    })
   
     output$plot2Text <- renderText({
      "<h4>
      The following grapph shows the number of different sections for the given year, month and day in a bar graph format.
      <br>
      Differnet colors reflect different sections. Hover over the graph for mor e information.
      </h4>"
    })
     
     output$plot3Text <- renderText({
       "<h4>
       The following grapph shows the top 10 section on a particular given day in a pie cahrt format. 
       <Br>
       Different colors represent different sections. Hover over the graph for more information.
       </h4>"
     })
     
     output$plot4Text <- renderText({
       "<h4>
       The following graph shows the number of articles for a particluar section over the give month and year in a poimt graph format. 
       <Br>
       Hover over the graph for more information.
       </h4>"
     })
     
     output$plot5Text <- renderText({
       "<h4>
       The following graph compares the popularity of two section for a given month and year. 
       <br>
       It plots the number of articles for each section ove a line graph in different colors. Hover over the graph for inforamtion.
       </h4>"
     })
     
     output$plot6Text <- renderText({
       "<h4>
       The following graph computes the 10 most popular topics for the given year and month and plots them over a point graph. 
       <br>
       Different colors reflect differnt rpics and theirs sizes are proportional to their popularity to compare two topic's relative popularity.
       </h4>"
     })
     
    
  })

