library("shiny")

ui <- fluidPage(
  theme = "bootstrap.css",
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      selectInput("year", label = h3("Select Year"), choices = as.list(seq(1852, 2017, by = 1)), selected = 1),
      selectInput("month", label = h3("Select Month"), 
                  choices = list("January" = 1, "February" = 2, "March" = 3, "April" = 4, "May" = 5,
                                 "June" = 6, "July" = 7, "August" = 8, "September" = 9, "October" = 10,
                                 "November" = 11, "December" = 12), selected = 1),
      selectInput("day", label = h3("Select Day"), choices = as.list(seq(1, 31, by = 1)), selected = 1),
      selectInput("section", label = h3("Select Section"), choices = as.list("World", "U.S.", "Politics", "Business", "Opinion",
                                                                             "Tech", "Science", "Health", "Sports", "Arts",
                                                                             "Style", "Food", "Travel"), selected = 3)
    ),
    
    mainPanel(
    )
  )
)

shinyUI(ui)