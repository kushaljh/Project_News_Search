library("shiny")
library("plotly")
library("DT")
shinyUI (
  navbarPage("News Search",
             theme = "bootstrap.css",
             tabPanel("About",
                      h4(
                        "Contributors: Manesh Jhawar, Kushal Jhunjhunwalla, Helly Shah, Alice Li"
                      ),
                      br(),
                      h1("Introduction"),
                      p("Nowadays, when we are always looking forward to something new, always looking for new 
                        technology, we might want to rest a minute and look back to what we have been through. 
                        Many of the times, ideas are inspired by an incident from the past. We decided that it 
                        will be fascinating if we could build a web app for people to search back in time, a 
                        time that is special or of interest to them. With this app, an individual will be able to 
                        know a day back in time when he or she was not even born, or she could revisit a day that 
                        means a lot to her."
                        ),
                      br(),
                      p(
                        "The dataset we used is the New York Times Archive API. NYT Archive API is a great data 
                        source because New York Times itself covers news and articles from worldwide, and is a 
                        rather believable source. With NYT data, we will be able to cover a wide range of events 
                        or incidents around the world on that day. NYT Archive API is very neatly organized, and 
                        contains enough information for our web apps, accordingly, it contains the dates that an 
                        article was posted. NYT API is categorized into sections like movies, books, top searched 
                        etc. We used the Archive API that provides lists of NYT articles by a month and goes back 
                        all the way to 1851. The archive API is similar to the article search API where we can 
                        simply pass the year and month we want, and the API will return a JSON object with 
                        articles of that month."
                      ),
                      br(),
                      p(
                        "In order to access this API, we signed up for an API key and acknowledged that it is 
                        limited to 2000 requests per day. Though there is a limitation to access to this data, 
                        it will be enough for our current use and web building."
                      ),
                      br(),
                      p(
                        "From the API, the columns we extracted from the dataset were: snippet, main headline, 
                        section, keywords, as well as the URL source. On our web page, we have five different 
                        tabs that take in different data inputs. The first tab is an About page that explains 
                        our web page. The next tab is an article search where the user will be able to select 
                        articles by date. A result will show indicating the publication date, headline, and 
                        section. By clicking on the headline of each result, a new page will open displaying 
                        the full article. Users will be about to change how many entries they want to see on 
                        this page, and there is also a search bar for interest searches."
                      ),
                      br(),
                      p(
                        "The second tab shows the Section Visualization. This part includes four visualizations 
                        that are organized by Monty, Day, Top 10, and Popularity. Visualizations use colors to 
                        indicate different sections and displayed using hover-interactive bar plot, pie plot, 
                        and dot plot."
                      ),
                      br(),
                      p(
                        "The third tab indicates section comparison where users will be able to choose two 
                        sections they want to compare. Our comparison visualization is indicated by a line, 
                        which clear show the comparing trend of the two sections. Each line represents the 
                        number of articles posted according to the day of the month. The line graph is also 
                        hovered interactive, showing major points and line labels."
                      ),
                      br(),
                      p(
                        "The last tab gives a dot visualization on ten of the most discussed topics of the 
                        month of interest. Users will be able to choose a month from the sidebar and visualize 
                        the ten most discussed topics of that month. The popularity of each topic will be 
                        indicated by the size of the dot, the bigger the dot, the more popular the topic."
                      ),
                      br(),
                      p(
                        "Our goal for this project is not just building a good looking app, we want our app 
                        to be as useful to the society as possible. With our app, people will be able to learn 
                        from the past, revisits and remember memorable moments from their lives. We are currently 
                        in a world with lots of issues, people might feel overwhelmed, but in fact, it has always 
                        been like this. Let's look back and see how things were resolved. We have made great 
                        improvements till this point today, let's have more faith going towards tomorrow."
                      )
                      ),
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
                          selectInput("year.viz", label = h3("Select Year"), choices = as.list(seq(1852, 2017, by = 1)), selected = 2017),
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
                          selectInput("year.pop", label = h3("Select Year"), choices = as.list(seq(1852, 2017, by = 1)), selected = 2017),
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
                          selectInput("year.top", label = h3("Select Year"), choices = as.list(seq(1852, 2017, by = 1)), selected = 2017),
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

