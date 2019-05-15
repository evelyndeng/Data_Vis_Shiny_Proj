ui <- navbarPage("Global Displacement Since 1960",

    tabPanel("Overview",
             htmlOutput("text_overview1"),
             plotlyOutput("tot_displaced"),
             htmlOutput("text_overview2")
             ),
    navbarMenu("Mapping Displacement",
               tabPanel("Top Destinations",
                        htmlOutput("text_mapping_displacement1"),
                        leafletOutput("map_mapping_displacement1", height=500),
                        htmlOutput("text_mapping_displacement2")),
               tabPanel("Main Origins",
                        htmlOutput("text_mapping_displacement3"),
                        leafletOutput("map_mapping_displacement2", height=500),
                        htmlOutput("text_mapping_displacement4")),
               tabPanel("Internal Displacement",
                        htmlOutput("text_mapping_displacement5"),
                        leafletOutput("map_mapping_displacement3", height=500),
                        htmlOutput("text_mapping_displacement6"))
    ),
    navbarMenu("Phases of Refugee Migration",
               tabPanel("Introduction",
                        htmlOutput("text_phases_intro1")),
               tabPanel("First Phase: African Decolonization",
                        htmlOutput("text_phases_first1"),
                        visNetworkOutput("plot_phases_first1"),
                        htmlOutput("text_phases_first2"),
                        visNetworkOutput("plot_phases_first2"),
                        htmlOutput("blank_phases1")),
               tabPanel("Second Phase: Wars in Afghanistan and Central America",
                        htmlOutput("text_phases_second1"),
                        visNetworkOutput("plot_phases_second1"),
                        htmlOutput("blank_phases2")),
               tabPanel("Third Phase: Europe Joins the Fray",
                        htmlOutput("text_phases_third1"),
                        visNetworkOutput("plot_phases_third1"),
                        htmlOutput("blank_phases3")),
               tabPanel("Fourth Phase: Instability in the Middle East",
                        htmlOutput("text_phases_fourth1"),
                        visNetworkOutput("plot_phases_fourth1"),
                        htmlOutput("text_phases_fourth2"),
                        visNetworkOutput("plot_phases_fourth2"),
                        htmlOutput("blank_phases4"))
    ),
    navbarMenu("A Case Study",
               tabPanel("When do displaced people leave their countries?",
                        htmlOutput("text_case_study1"),
                        plotlyOutput("plot_case_study1")),
               tabPanel("Where do they go?",
                        htmlOutput("text_case_study2"),
                        leafletOutput("map_case_study1"),
                        htmlOutput("text_case_study3"),
                        leafletOutput("map_case_study2"),
                        htmlOutput("text_case_study4"))
             ),
    navbarMenu("Explore on Your Own",
               tabPanel("Displacement Over Time",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(inputId = "countries", label = "Choose countries:",
                                        choices = countries_vec, multiple = TRUE)),
                          mainPanel(
                            htmlOutput("text_disp1"),
                            plotOutput("displacement_perc"),
                            htmlOutput("text_disp2"),
                            plotOutput("displacement"),
                            htmlOutput("text_disp3"))
                        )
               ),
               tabPanel("Economics and Displacement",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(inputId = "regions", label = "Choose regions:", 
                                        choices = regions_vec, multiple = TRUE),
                            selectInput(inputId = "year", label = "Choose year:", 
                                        choices = unique(df_economic$year), multiple = FALSE)),
                          mainPanel(
                            htmlOutput("text_econ1"),
                            plotlyOutput("gdp"))
                        )
               )
             )

    )

