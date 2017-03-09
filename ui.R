library("shiny")
library("plotly")
library("DT")
shinyUI (
  navbarPage("NewSearch",
             theme = "bootstrap.css",
             tabPanel("About"),
             tabPanel("Article Search",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("year.article", label = h3("Select Year"), choices = as.list(seq(1852, 2017, by = 1)), selected = 2000),
                          selectInput("month.article", label = h3("Select Month"), 
                                      choices = list("January" = 1, "February" = 2, "March" = 3, "April" = 4, "May" = 5,
                                                     "June" = 6, "July" = 7, "August" = 8, "September" = 9, "October" = 10,
                                                     "November" = 11, "December" = 12), selected = 1),
                          selectInput("day.article", label = h3("Select Day"), choices = sprintf("%02d", c(seq(01,31, by = 1))), selected = 1)
                        ),
                        
                        mainPanel(
                          h3("ARTICLE SEARCH"),
                          br(),
                          DT :: dataTableOutput("table")
                        )
                      )
             ),
             tabPanel("Section Visualizations",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("year.viz", label = h3("Select Year"), choices = as.list(seq(1852, 2017, by = 1)), selected = 2000),
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
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Monthly Section Distribution",
                                     br(),
                                     plotlyOutput("plot1")),
                            tabPanel("Daily Section Distribution",
                                     br(),
                                     plotlyOutput("plot2")),
                            tabPanel("Top 10 Section Daily Analysis",
                                     br(),
                                     plotlyOutput("plot3")),
                            tabPanel("Section Popularity",
                                     br(),
                                     plotlyOutput("plot.trend")))
                          
                        )
                      )
             ),
             tabPanel("Section Popularity Comparision",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("year.pop", label = h3("Select Year"), choices = as.list(seq(1852, 2017, by = 1)), selected = 2000),
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
                        mainPanel(
                          h3("Section Popularity Comparision: Line"),
                          br(),
                          plotlyOutput("plot.trendline")))
             ),
             tabPanel("Popular Topics",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("year.top", label = h3("Select Year"), choices = as.list(seq(1852, 2017, by = 1)), selected = 2000),
                          selectInput("month.top", label = h3("Select Month"), 
                                      choices = list("January" = 1, "February" = 2, "March" = 3, "April" = 4, "May" = 5,
                                                     "June" = 6, "July" = 7, "August" = 8, "September" = 9, "October" = 10,
                                                     "November" = 11, "December" = 12), selected = 1)
                        ),
                        mainPanel(
                          h3("10 most discussed-about topics"),
                          br(),
                          plotlyOutput("plot.values")
                        )
                      )
             )
  )
)

