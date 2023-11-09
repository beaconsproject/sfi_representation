ui <- dashboardPage(skin="red",
  dashboardHeader(title = "SFI Representation"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Results - Ecozones", tabName = "ecozones", icon = icon("th")),
      menuItem("Results - Ecoprovinces", tabName = "ecoprovinces", icon = icon("th")),
      menuItem("Results - Ecoregions", tabName = "ecoregions", icon = icon("th")),
      menuItem("Methods", tabName = "methods", icon = icon("book"))
    ),
    #checkboxInput("sfi", "Add SFI map (10km grid)", FALSE),
    selectInput(inputId = "indicator", label = "Select indicator to map:", choices = x$indicator, selected = x$indicator[1]),
    sliderInput(inputId = "threshold", "Biophysical threshold:", min = 0, max = 1, value = 0.2, step=0.05),
    sliderInput(inputId = "threshold2", "Species threshold:", min = 0, max = 2, value = 1, step=0.05),
    p(style="padding:15px;","Biophysical indicators: Representation is measured using dissimilarity metrics (KS statistic for continuous maps and Bray-Curtis for categorical maps); values ranges from 0 to 1, with values approaching 0 indicating increasing representation."),
    p(style="padding:15px;","Songbirds & waterfowl: Representation is measured using the population ratio = proportion of population in SFI / proportion of SFI in stratification unit; values > 1 indicate that a greater proportion of population is in the SFI than expected.")
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName="overview",
        fluidRow(
          tabBox(
            id = "tabsetO", width="12",
            tabPanel("Welcome!", htmlOutput("help")),
            tabPanel("Stratification units", htmlOutput("strata")),
            tabPanel("Ecological datasets", htmlOutput("datasets"))
            #tabPanel("Datasets", dataTableOutput("datasets"))
          )
        )
      ),
      tabItem(tabName="ecozones",
        fluidRow(
          #tabBox(
          #  id = "tabsetZ", width="8",
          #  tabPanel("Click on a coloured polygon to view indicator barplots for that ecozone", leafletOutput("ecozmap", height=480))
          #),
          box(title="Click on a coloured polygon to view indicator barplots for that ecozone", leafletOutput("ecozmap", height=480),width=8),
          tabBox(
            id = "tabsetZ2", width="4",
            tabPanel("Biophysical",dataTableOutput("ecoztable1", height=480)),
            tabPanel("Songbirds",dataTableOutput("ecoztable2", height=480)),
            tabPanel("Core habitat",dataTableOutput("ecoztable4", height=480)),
            tabPanel("Waterfowl",dataTableOutput("ecoztable5", height=480)),
            tabPanel("Guilds",dataTableOutput("ecoztable3", height=480))
          )
        ),
        fluidRow(
          tabBox(
            id = "tabset1", width="4",
            tabPanel("Biophysical", plotOutput("ecoz_barplot"))
          ),
          tabBox(
            id = "tabset2", width="4",
            tabPanel("Songbirds",plotOutput("ecoz_barplot2")),
            tabPanel("Core habitat",plotOutput("ecoz_barplot4"))
          ),
          tabBox(
            id = "tabset3", width="4",
            tabPanel("Waterfowl",plotOutput("ecoz_barplot5")),
            tabPanel("Guilds",plotOutput("ecoz_barplot3"))
          )
        )
      ),
      tabItem(tabName = "ecoprovinces",
        fluidRow(
          #tabBox(
          #  id = "tabsetP", width="8",
          #  tabPanel("Click on a coloured polygon to view indicator barplots for that ecoprovince", leafletOutput("ecopmap", height=480))
        #),
          box(title="Click on a coloured polygon to view indicator barplots for that ecoprovince", leafletOutput("ecopmap", height=480),width=8),
          tabBox(
            id = "tabsetP2", width="4",
            tabPanel("Biophysical",dataTableOutput("ecoptable1", height=480)),
            tabPanel("Songbirds",dataTableOutput("ecoptable2", height=480)),
            tabPanel("Core habitat",dataTableOutput("ecoptable4", height=480)),
            tabPanel("Waterfowl",dataTableOutput("ecoptable5", height=480)),
            tabPanel("Guilds",dataTableOutput("ecoptable3", height=480))
          )
        ),
        fluidRow(
          tabBox(
            id = "tabset4", width="4",
            tabPanel("Biophysical", plotOutput("ecop_barplot"))
          ),
          tabBox(
            id = "tabset5", width="4",
            tabPanel("Songbirds",plotOutput("ecop_barplot2")),
            tabPanel("Core habitat",plotOutput("ecop_barplot4"))
          ),
          tabBox(
            id = "tabset6", width="4",
            tabPanel("Waterfowl",plotOutput("ecop_barplot5")),
            tabPanel("Guilds",plotOutput("ecop_barplot3"))
          )
        )
      ),
      tabItem(tabName = "ecoregions",
        fluidRow(
        #  tabBox(
        #    id = "tabsetR", width="8",
        #    tabPanel("Click on a coloured polygon to view indicator barplots for that ecoregion", leafletOutput("ecormap", height=480))
        #),
          box(title="Click on a coloured polygon to view indicator barplots for that ecoregion", leafletOutput("ecormap", height=480),width=8),
          tabBox(
            id = "tabsetR2", width="4",
            tabPanel("Biophysical",dataTableOutput("ecortable1", height=480)),
            tabPanel("Songbirds",dataTableOutput("ecortable2", height=480)),
            tabPanel("Core habitat",dataTableOutput("ecortable4", height=480)),
            tabPanel("Waterfowl",dataTableOutput("ecortable5", height=480)),
            tabPanel("Guilds",dataTableOutput("ecortable3", height=480))
          )
        ),
        fluidRow(
          tabBox(
            id = "tabset7", width="4",
            tabPanel("Biophysical", plotOutput("ecor_barplot"))
          ),
          tabBox(
            id = "tabset8", width="4",
            tabPanel("Songbirds",plotOutput("ecor_barplot2")),
            tabPanel("Core habitat",plotOutput("ecor_barplot4"))
          ),
          tabBox(
            id = "tabset9", width="4",
            tabPanel("Waterfowl",plotOutput("ecor_barplot5")),
            tabPanel("Guilds",plotOutput("ecor_barplot3"))
          )
        )
      ),    
      tabItem(tabName="methods",
       fluidRow(
          tabBox(
            id = "tabset10", width="12",
            tabPanel("Analysis", htmlOutput("methods"))
          )
        )
      )
    )
  )
)
