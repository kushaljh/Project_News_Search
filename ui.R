library("shiny")
library("plotly")
library("DT")
#Shiny UI for the app. 
shinyUI (
  #NavBarPage function for the ui layout.
  navbarPage("NewSearch",
    theme = "bootstrap.css",
    #TabPanel for Information about the app. 
    tabPanel("About",
      htmlOutput("textInfo")
    ),
    #TabPanel for the article search
    tabPanel("Article Search",
      sidebarLayout(
        sidebarPanel(
          selectInput("year.article", label = h3("Select Year"), choices = as.list(seq(1852, 2017, by = 1)), selected = 2017),
          selectInput("month.article", label = h3("Select Month"), 
                      choices = list("January" = 1, "February" = 2, "March" = 3, "April" = 4, "May" = 5,
                      "June" = 6, "July" = 7, "August" = 8, "September" = 9, "October" = 10,
                      "November" = 11, "December" = 12), selected = 1),
          selectInput("day.article", label = h3("Select Day"), choices = sprintf("%02d", c(seq(01,31, by = 1))), selected = 1)
        ),
        #MainPanel for rendering all the article results.                                               
        mainPanel(
          h3("ARTICLE SEARCH"),
          htmlOutput("articleText"),
          br(),
          DT :: dataTableOutput("table")
        )
      )
    ),
    #TabPanel for Section Visuals. 
    tabPanel("Section Visualizations",
      sidebarLayout(
        #SidePanel for input widgets.
        sidebarPanel(
          selectInput("year.viz", label = h3("Select Year"), choices = as.list(seq(1981, 2017, by = 1)), selected = 2017),
          selectInput("month.viz", label = h3("Select Month"), 
                      choices = list("January" = 1, "February" = 2, "March" = 3, "April" = 4, "May" = 5,
                      "June" = 6, "July" = 7, "August" = 8, "September" = 9, "October" = 10,
                      "November" = 11, "December" = 12), selected = 1),
          selectInput("day.viz", label = h3("Select Day"), choices = sprintf("%02d", c(seq(01,31, by = 1))), selected = 1),
          selectInput("section.viz", label = h3("Section"), 
                      choices = as.list(c("World", "U.S.", "Politics", "Business", "Opinion",
                      "Tech", "Science", "Health", "Sports", "Arts",
                      "Style", "Food", "Travel")), selected = "World")
      ),
      #MainPanel for rendering all the output plots.                                                
      mainPanel(
        tabsetPanel(
          tabPanel("Monthly Section Distribution",
            htmlOutput("plot1Text"),
            br(),
            plotlyOutput("plot1")),
          tabPanel("Daily Section Distribution",
            htmlOutput("plot2Text"),
            br(),
            plotlyOutput("plot2")),
            tabPanel("Top 10 Section Daily Analysis",
              br(),
              htmlOutput("plot3Text"),
              plotlyOutput("plot3")),
            tabPanel("Section Popularity",
              htmlOutput("plot4Text"),
              br(),
              plotlyOutput("plot.trend")))
        )
      )
    ),
    #TabPanel for Section Popularity. 
    tabPanel("Section Popularity Comparision",
      sidebarLayout(
        #SidePanel for input widgets.
        sidebarPanel(
          selectInput("year.pop", label = h3("Select Year"), choices = as.list(seq(1981, 2017, by = 1)), selected = 2017),
          selectInput("month.pop", label = h3("Select Month"), 
                      choices = list("January" = 1, "February" = 2, "March" = 3, "April" = 4, "May" = 5,
                      "June" = 6, "July" = 7, "August" = 8, "September" = 9, "October" = 10,
                      "November" = 11, "December" = 12), selected = 1),
          selectInput("section1.pop", label = h3("Select Section 1"), 
                      choices = as.list(c("World", "U.S.", "Politics", "Business", "Opinion",
                      "Tech", "Science", "Health", "Sports", "Arts",
                      "Style", "Food", "Travel")), selected = "World"),
          selectInput("section2.pop", label = h3("Select Section 2"), 
                      choices = as.list(c("World", "U.S.", "Politics", "Business", "Opinion",
                      "Tech", "Science", "Health", "Sports", "Arts",
                      "Style", "Food", "Travel")), selected = "U.S.")
        ),
        #MainPanel for rendering all the output plots. 
        mainPanel(
          h3("Section Popularity Comparision: Line"),
          htmlOutput("plot5Text"),
          br(),
          plotlyOutput("plot.trendline")))
        ),
    #TabPabel for Popular Topics. 
    tabPanel("Popular Topics",
      sidebarLayout(
        #SidePanel for input widgets.
        sidebarPanel(
          selectInput("year.top", label = h3("Select Year"), choices = as.list(seq(1981, 2017, by = 1)), selected = 2017),
          selectInput("month.top", label = h3("Select Month"), 
                      choices = list("January" = 1, "February" = 2, "March" = 3, "April" = 4, "May" = 5,
                      "June" = 6, "July" = 7, "August" = 8, "September" = 9, "October" = 10,
                      "November" = 11, "December" = 12), selected = 1)
        ),
        #MainPanel for rendering all the output plots. 
      mainPanel(
        h3("10 Most Discussed-About Topics"),
        htmlOutput("plot6Text"),
        br(),
        plotlyOutput("plot.values")
      )
    )
  )
)
)

