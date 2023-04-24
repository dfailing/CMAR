# Load required packages
library(dplyr)
library(DT)
library(here)
library(readr)
library(shiny)
library(shinydashboard)
library(tidyr)

# Define UI for the app
ui <- dashboardPage(
  title = "Cry Me A River! Trail Runs",
  dashboardHeader(
    title = tags$a("Cry Me A River!", titleWidth = "auto", href = "https://crymearivertrailruns.com/", style = "color: white;")
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Start Here", tabName = "start", icon = icon("list")),
      menuItem("Course Records", tabName = "course_records", icon = icon("trophy")),
      menuItem("Mileage Totals", tabName = "mileage_totals", icon = icon("table")),
      menuItem("All Results", tabName = "all_results", icon = icon("table")),
      menuItem("Register @ RunRace", href = "https://www.runrace.net/findarace.php?id=23188IL", icon = icon("clipboard"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML('
        .skin-blue .main-header .logo {
          background-color: #3c8dbc;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #3c8dbc;
        }
      ')),
      tags$script(
        '
      $(document).ready(function() {
        $("a[href^=\'http\'],a[href^=\'https\']").not("[href*=\'http://127.0.0.1\'], [href*=\'http://localhost\']").attr("target", "_blank");
      });
      '
      )
    ),
    tags$style(type="text/css",
               ".dataTables_wrapper .dataTables_filter {
        float:left;
    }
    .dataTables_wrapper .dataTables_length {
        float:right;
    }"
    ),
    tabItems(
      tabItem(
        tabName = "start",
        fluidRow(
          box(
            width = 12,
            title = NULL,
            p(HTML("<b>Getting Started:</b><br>
              <ul>
                <li><b>Course Records</b> lists the fastest finishing times at each race distance. The 5 mile, 6 mile, and 50 mile races were only run once, so course records are not displayed.</li>
                <li><b>Mileage Totals</b> for all finishers are given, along with their completed distance each year, and are sorted by total mileage by default.  Type a runner's name into the search box to see their lifetime results. By default, the table is sorted by total mileage.</li>
                <li><b>All Results</b> is a merged table of all events for all years. Type a runner's name into the search box to see their lifetime results. You can also search by any combination of other fields. Try '2015 50 mile' to pull up results for that year and event. By default, the table is sorted by year and event.</li>
                <li><b>Register</b> for this year's race so that you don't miss out on the fun!
                <li><b>Course Maps</b> below allow you to view Strava routes or download GPX files to navigate using your watch, phone, or other GPS device.
              </ul><br>
              <b>Course Maps:</b><br>
              <ul>
                <li>Full Loop - <a href='https://www.strava.com/routes/3084308043342515312' target='_blank'>Strava</a> | <a href='GPX/CMAR-Full-Loop.gpx' download>GPX</a></li>
                <li>Half Marathon - <a href='https://www.strava.com/routes/3084309444938383044' target='_blank'>Strava</a> | <a href='GPX/CMAR-Half-Marathon.gpx' download>GPX</a></li>
                <li>Out & Back - <a href='http://www.strava.com' target='_blank'>Strava</a> | <a href='GPX/CMAR-Out-and-Back.gpx' download>GPX</a></li>
                <li>Lake Loop - <a href='https://www.strava.com/routes/3084304554337206980' target='_blank'>Strava</a> | <a href='GPX/CMAR-Lake-Loop.gpx' download>GPX</a></li>
              </ul><br>
              <b>Results Sources:</b><br>
              <ul>
                <li>2015 - <a href='https://www.runrace.net/findarace.php?id=15192IL&tab=a4'>RunRace</a></li>
                <li>2016 - <a href='https://raceresultsplus.com/event/cry-me-river-trail-runs'>RaceResultsPlus</a>, <a href='https://ultrarunning.com/calendar/event/cry-me-a-river/race/16497/results'>UltraRunning</a></li>
                <li>2017 - <a href='https://results.chronotrack.com/event/results/event/event-31624?&lc=en'>ChronoTrack</a>, <a href='http://www.runrace.net/findarace.php?id=17189IL1&tab=a4'>RunRace</a></li>
                <li>2018 - <a href='https://runrace.net/findarace.php?id=18188IL&tab=a4'>RunRace</a></li>
                <li>2019 - <a href='https://www.runrace.net/findarace.php?id=19187IL&tab=a4'>RunRace</a></li>
                <li>2020 - Canceled due to COVID-19</li>
                <li>2021 - <a href='https://www.runrace.net/findarace.php?id=21190IL&tab=a4'>RunRace</a></li>
                <li>2022 - <a href='https://www.runrace.net/findarace.php?id=22189IL&tab=a4'>RunRace</a></li>
              </ul>"))
          )
        )
      ),
      
      tabItem(tabName = "course_records",
              {course_records <- read_csv(here("data", "course_records.csv")) %>%
                mutate(Record = paste0(Rank, ". ", paste(First, Last), " (", Age, "), ", Time, ", ", Year)) %>% 
                select(Event, Gender, Record)
              
              lapply(unique(course_records$Event), function(event) {
                fluidRow(lapply(unique(course_records$Gender), function(gender) {
                  sub_table_name <- paste0(event, " - ", gender)
                  column(
                    width = 6,
                    course_records %>%
                      filter(Event == event, Gender == gender) %>%
                      select(Record) %>%
                      rename(!!sub_table_name := Record) %>%
                      datatable(
                        options = list(
                          dom = 't',
                          pageLength = 10,
                          searching = FALSE,
                          columnDefs = list(
                            list(targets = 0, visible = FALSE),
                            list(targets = "_all", orderable = FALSE)
                          )
                        )
                      )
                  )
                }))
              })}),
      tabItem(
        tabName = "mileage_totals",
        fluidRow(
          box(
            width = 12,
            DTOutput("mileage_table")
          )
        )
      ),
      tabItem(
        tabName = "all_results",
        fluidRow(
          box(
            width = 12,
            DTOutput("all_results_table")
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  mileage_data <- reactive({
    read_csv(here("data", "mileage_totals.csv")) %>%
      datatable(options = list(
        pageLength = 10,
        scrollX = TRUE,
        scrollY = TRUE,
        columnDefs = list(list(targets = 0, visible = FALSE))
      ))
  })
  
  output$mileage_table <- renderDT({
    mileage_data()
  })
  
  all_results_data <- reactive({
    read_csv(here("data", "all_results.csv")) %>%
      datatable(options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = TRUE,
        columnDefs = list(list(targets = 0, visible = FALSE))
      ))
  })
  
  output$all_results_table <- renderDT({
    all_results_data()
  })
}

# Run the app
shinyApp(ui = ui, server = server)