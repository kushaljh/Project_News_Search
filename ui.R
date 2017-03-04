library("shiny")

months.list <- list("January" = 1, "February" = 2, "March" = 3, "April" = 4, "May" = 5,
                    "June" = 6, "July" = 7, "August" = 8, "September" = 9, "October" = 10,
                    "November" = 11, "December" = 12)
years.list <- as.list(seq(1852, 2017, by = 1))
days.list <- sprintf("%02d", c(seq(01,31, by = 1)) )
shinyUI(
    fluidPage(
    sidebarLayout(
      sidebarPanel(
        selectInput("year", label = h3("Select Year"), choices = years.list, selected = 1),
        selectInput("month", label = h3("Select Month"),
                    choices = months.list,
                    selected = 1),
        selectInput("day", label = h3("Select Day (DD format)"), choices = days.list, selected = 1)
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("TABLE", 
                   h3("Articles"),
                   br(),
                   dataTableOutput("table"))
        )
      )
    )
  )
)