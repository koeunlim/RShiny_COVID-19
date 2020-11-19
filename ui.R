#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

shinyUI(fluidPage(

    theme=shinytheme("spacelab"),
    tags$head(
        tags$style(HTML(".navbar .navbar-header {float: right}"))
    ),
    
    navbarPage(
        title="Impact of State Policies on COVID-19 in the United States",
        id="nav",
        position="fixed-top",
        collapsible=TRUE,

        ## Page 1: National Averages across states =====
        navbarMenu("National Averages", icon=icon('flag-usa'),
                   
        tabPanel("Cases by Policy", 
                 br(),
                 br(),
                 br(),
                 fluidRow(h1("National Averages by Policy")),
                 fluidRow(
                     column(3,
                            br(),
                            "Explore how a state policy impacts",
                            br(),
                            "COVID-19 outcomes for 30 days",
                            br(),
                            "between Feb and Oct, 2020.",
                            br()
                     )
                 ),
                 
                 fluidRow(
                     column(3,
                            br(),
                            selectInput(
                                inputId = "policy_name",
                                label = "Select a policy:",
                                choices = sort(unique(state.policy.list$policy_type))
                            ),
                            selectizeInput(
                                inputId = "policy_start_stop",
                                label = "Select an action:",
                                choices = sort(unique(state.policy.list$start_stop))
                            ),
                     ),
                     column(8,
                            plotlyOutput("Map_by_policy"),
                            fluidRow(
                                column(12,
                                       plotlyOutput("Counts_by_policy"),
                                       br(),
                                       fluidRow(
                                           column(12,
                                                  plotlyOutput("Rates_by_policy")
                                           )
                                       )
                                )
                            )
                            
                     )
                 ) 
        ), # end of Page 1 - Tab1 Nationwide Averages by Policy 
        
        tabPanel("Cases by Observations", 
                 br(),
                 br(),
                 br(),
                 fluidRow(h1("National Averages by Observation")),
                 fluidRow(
                     column(3,
                            br(),
                            "Explore how a state policy impacts",
                            br(),
                            "COVID-19 outcomes for 30 days",
                            br(),
                            "between Feb and Oct, 2020.",
                            br()
                     )
                 ),
                 
                 fluidRow(
                     column(3,
                            br(),
                            selectInput(
                                inputId = "observation",
                                label = "Select an observation:",
                                choices = list("Daily Confirmed Cases" = "confirmed.case",
                                               "Daily Deaths" = "new.deaths",
                                               "ICU Occupancy" = "ICU",
                                               "New ICU Admissions" = "newICU",
                                               "ICU Overflow" = "ICUover",
                                               "Ventilator Occupancy" = "Ventilator")
                            ),
                            checkboxGroupInput(
                                inputId = "policy_select",
                                label = "Select policies:",
                                choices = sort(paste0('[',toupper(state.policy.list$start_stop),'] ',state.policy.list$policy_type)),
                                selected = sort(paste0('[',toupper(state.policy.list$start_stop),'] ',state.policy.list$policy_type))[c(3,6,8,9,12)]
                            ),
                     ),
                     column(8,
                            plotlyOutput("Box_Dates_by_Obs"),
                            br(),
                            fluidRow(
                                plotlyOutput("Counts_by_Observation"),
                                br(),
                                fluidRow(
                                    column(12,
                                           plotlyOutput("Rates_by_Observation")
                                    )
                                )
                            )
                            
                            
                     )
                 ) 
        ) # end of Page 1 - Tab2 Nationwide Averages by Observations 
        ), # end of Page 1: Nationwide Averages
        
        ## Page 2: Compare state against Nationwide Averages =====
        tabPanel("State vs. National", icon=icon('drafting-compass'),
                 br(),
                 br(),
                 br(),
                 fluidRow(h1("State vs. National Averages")),
                 fluidRow(
                     column(3,
                            br(),
                            "Compare COVID-19 outcomes between",
                            br(),
                            "a state and the national averages.",
                            br()
                     ),
                     column(8,
                            br(),
                            wellPanel(h4(strong("National Average Cases")),
                                      h4(strong(htmlOutput("all_stat1"))),
                                      h4(strong(htmlOutput("all_stat12")))
                            ),
                     )
                 ),
                 
                 fluidRow(
                     column(3,
                            br(),
                            selectInput(
                                inputId = "state",
                                label = "Select a state:",
                                choices = state.names.list$Name
                            ),
                            br(),
                            selectInput(
                                inputId = "policy_name_state",
                                label = "Select a policy:",
                                choices = sort(unique(state.policy.list$policy_type))
                            ),
                            selectizeInput(
                                inputId = "policy_start_stop_state",
                                label = "Select an action:",
                                choices = sort(unique(state.policy.list$start_stop))
                            ),
                            selectizeInput(
                                inputId = "observation_state",
                                label = "Select an observation:",
                                choices = list("Daily Confirmed Cases" = "confirmed.case",
                                               "Daily Deaths" = "new.deaths",
                                               "ICU Occupancy" = "ICU",
                                               "New ICU Admissions" = "newICU",
                                               "ICU Overflow" = "ICUover",
                                               "Ventilator Occupancy" = "Ventilator")
                            ),
                     ),
                     column(8,

                                column(3,
                                       br(),
                                       wellPanel(h5(strong(htmlOutput("state_rank0"))),
                                                 h5(strong(htmlOutput("state_label0"))),
                                                 h5(strong(htmlOutput("state_stat0"))),
                                                 h5(strong(htmlOutput("state_stat02")))
                                       )
                                ),
                                column(3,
                                       br(),
                                       wellPanel(h5(strong("Rank 1")),
                                                 h5(strong(htmlOutput("state_label1"))),
                                                 h5(strong(htmlOutput("state_stat1"))),
                                                 h5(strong(htmlOutput("state_stat12")))
                                       )
                                ),
                                column(3,
                                       br(),
                                       wellPanel(h5(strong("Rank 2")),
                                                 h5(strong(htmlOutput("state_label2"))),
                                                 h5(strong(htmlOutput("state_stat2"))),
                                                 h5(strong(htmlOutput("state_stat22")))
                                       )
                                ),
                                column(3,
                                       br(),
                                       wellPanel(h5(strong("Rank 3")),
                                                 h5(strong(htmlOutput("state_label3"))),
                                                 h5(strong(htmlOutput("state_stat3"))),
                                                 h5(strong(htmlOutput("state_stat32")))
                                       )
                                ),
                            fluidRow(
                                br(),
                                column(12,
                                       plotlyOutput("Map_state")
                                ),
                                fluidRow(
                                    column(12,
                                           plotlyOutput("Box_Dates_state"),
                                           fluidRow(
                                           column(12,
                                                  br(),
                                                  plotlyOutput("Counts_state"),
                                                  br(),
                                                  fluidRow(
                                                      column(12,
                                                             plotlyOutput("Rates_state")
                                                      )
                                                  )
                                           )
                                       )
                                    )
                                )
                            )
                     )
                 )
        ), # End of Page 2: State compared to National Averages
        
        ## Page 3: Data tables and links =====
        navbarMenu("Data", icon=icon('table'),
                   
        tabPanel("Summary",
                 br(),
                 br(),
                 br(),
                 h3("Data courtesy of IHME and Healthdata.gov."),
                 "Individual links to the raw data are provided below.",
                 br(),
                 br(),
                 uiOutput("covid"),
                 uiOutput("policy"),
                 br(),
                 br(),
                 fluidRow(
                     column(12,
                            DT::dataTableOutput("table.covid.summary"))
                 ),
                 br()
        ),
        
        tabPanel("USA Covid Data",
                 br(),
                 fluidRow(
                     column(12,
                            DT::dataTableOutput("table.covid"))
                 ),
                 br()
        ),
        
        tabPanel("USA State Policy Data",
                 br(),
                 fluidRow(
                     column(12,
                            DT::dataTableOutput("table.policy"))
                 ),
                 br()
        )
        )#end of Page 3: DATA
    )
))
