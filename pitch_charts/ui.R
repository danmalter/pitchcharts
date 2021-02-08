#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

ui <- dashboardPage(
    dashboardHeader(title = "Baseball Dashboard"),
    dashboardSidebar(width = 250, sidebarMenu(
                     menuItem("Batter Pitch Chart", tabName = 'pitchCharts', icon = icon("star-o"),
                              menuSubItem("Batter Pitch Chart (click here)", tabName = 'pitchCharts'),
                              menuSubItem(pickerInput("pitcher_throw_batter_view", label = h5("Pitcher Throw Type"),
                                               c("Left Handed" = "L", "Right Handed" = "R"),
                                               selected=c("L", "R"),
                                               multiple = TRUE)),
                              menuSubItem(pickerInput("pitch_type_batter_view", label = h5("Pitch Type"),
                                             choices = unique(sort(batter_df$pitch)),
                                             selected = unique(sort(batter_df$pitch)),
                                            multiple = TRUE,
                                            options = list(`actions-box` = TRUE))),
                              menuSubItem(pickerInput("event_batter_view", label = h5("Event"),
                                             choices = unique(sort(batter_df$event)),
                                             selected=c("Single", "Double", "Home Run", 
                                               "Flyout", "Groundout"),
                                            multiple = TRUE,
                                            options = list(`actions-box` = TRUE))),
                              menuItem(pickerInput("chart_pitch_scatter", label = h5("Batter Pitch Chart Type"), choices = c('Pitch Chart', 'Density Plot'), 
                                           selected = "Pitch Chart"))
                            ),
                     menuItem("Pitch Breakdown", tabName = 'pitchCharts2', icon = icon("star-o"),
                              menuSubItem("Pitch Breakdown Chart (click here)", tabName = 'pitchCharts2', icon = icon("star-o")),
                              menuSubItem(pickerInput("chart_type", label = h5("Pitch Breakdown by Type"), choices = c('pitch', 'event'), 
                                           selected = "event")),
                              menuSubItem(pickerInput("pitcher_throw", label = h5("Pitcher Throw Type"),
                                               c("Left Handed" = "L", "Right Handed" = "R"),
                                               selected=c("L", "R"),
                                               multiple = TRUE)),
                              menuSubItem(pickerInput("pitch_type", label = h5("Pitch Type"),
                                             choices = unique(sort(batter_df$pitch)),
                                             selected=c("Changeup", "Cureveball", "Cutter", "Four-Seam Fastball",
                                               "Sinker", "Slider", "Slow Curve", "Splitter", "Two-Seam Fastball"),
                                            multiple = TRUE,
                                            options = list(`actions-box` = TRUE))),
                              menuSubItem(pickerInput("event", label = h5("Event"),
                                             choices = unique(sort(batter_df$event)),
                                             selected=c("Single", "Double", "Home Run", 
                                               "Flyout", "Groundout"),
                                            multiple = TRUE,
                                            options = list(`actions-box` = TRUE)))
                              ),
                     menuItem("Spraycharts", tabName = 'sprayCharts', icon = icon("star-o"),
                              menuSubItem("View Spraycharts (click here)", tabName = 'sprayCharts', icon = icon("star-o")),
                              menuSubItem(pickerInput("stadiums", label = h5("Spraychart Stadium"), choices = stadiums, 
                                           selected = "white_sox"))
                            ),
                     menuItem("About", tabName = "about", icon = icon("question-circle")),
                     menuItem(selectizeInput("batters", label = h5("Batter Name"), choices = unique(sort(batter_df$matchup.batter.fullName)), 
                                           selected = "Aaron Judge"), icon = icon("")),
                     menuItem(
                            dateRangeInput('dateRange',
                                           label = span(tagList(icon("calendar"), "Date Range:")),
                                           start = min(batter_df$game_date), max(batter_df$game_date),
                                           format = "yyyy-mm-dd")
                                )
                     )
                  ),
dashboardBody(
                tags$head(
                  tags$style(type="text/css", "select { max-width: 360px; }"),
                  tags$style(type="text/css", ".span4 { max-width: 360px; }"),
                  tags$style(type="text/css",  ".well { max-width: 360px; }")
                ),
                
                tabItems(  
                  tabItem(tabName = "about",
                          h2("About this App"),
                          HTML('<br/>'),
                          fluidRow(
                            box(title = "Author: Danny Malter", background = "black", width=10, collapsible = TRUE,
                                helpText(p(strong("This application is a demonstration of using a Shiny app with baseball data for filtering and futher in-game analysis."))),
                                helpText(p("Please contact me on",
                                           a(href ="https://twitter.com/danmalter", "Twitter",target = "_blank"),
                                           " or at my",
                                           a(href ="http://danmalter.github.io/", "personal page", target = "_blank"),
                                           ", for more information, to suggest improvements or report errors."))
                            )
                          )
                  ),
                  tabItem(tabName = "pitchCharts",
                            box(plotOutput("batterPlot"), title = "Batter Pitch Chart", width=11, collapsible = TRUE),
                            box(uiOutput("img"), width = 5, background = "black")
                          ),
                  tabItem(tabName = "pitchCharts2",
                            box(plotOutput("batterTypePlot"), title = "Batter Pitch Charts Broken Down", width=11, collapsible = TRUE),
                            box(uiOutput("img2"), width = 5, background = "black")
                          ),
                  tabItem(tabName = "sprayCharts",
                            box(plotlyOutput("spraychart"), title = "Spraycharts", width=11, collapsible = TRUE),
                            box(uiOutput("img3"), width = 5, background = "black")
                      )
                ) 
)
)


