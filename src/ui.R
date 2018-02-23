if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(shiny,
               leaflet,
               shinydashboard,
               shinyWidgets,
               lubridate)



ui <- dashboardPage(title="ID_Dashboard",
                    dashboardHeader(title= "ID_Dashboard 1.0"),
                    dashboardSidebar(
                      includeCSS("styles.css"),
                      conditionalPanel(
                        condition="input.conditionedPanels==1",
                        HTML("<br/>"),
                        style="padding: 0px;",
                        actionButton('refresh_data',
                                     "Refresh")
                      )
                    ),
                    dashboardBody({

                      tabsetPanel(
                        id = "conditionedPanels",
                        tabPanel('One',
                                 value=1,
                                 style="padding=0px;",
                                 fluidRow(
                                   box(title='ID capacity',
                                       solidHeader=FALSE,
                                       collapsible=TRUE,
                                       color='lime',
                                       width=12,
                                       heigth=600,
                                       fluidRow(
                                         pickerInput('ID_choice',
                                                     NULL,
                                                     choices=list('Netherlands',
                                                                  'Belgium',
                                                                  'Denmark',
                                                                  'France',
                                                                  'Germany',
                                                                  'Switzerland'),width = 150
                                         ),
                                         plotOutput('ID_plot',
                                                    height="250",
                                                    width="90%")
                                       )
                                   ),
                                   column(5,offset = 0,
                                          style='padding: 0px;',
                                          align='center',
                                          box(title='IGCC',
                                              solidHeader=FALSE,
                                              collapsible=TRUE,
                                              color='lime',
                                              width=12,
                                              heigth=300,
                                              fluidRow(
                                                plotOutput('igcc_plot',
                                                           height="250",
                                                           width="90%")
                                              )
                                          )
                                   )

                                 )
                        )
                      )
                    })
)
