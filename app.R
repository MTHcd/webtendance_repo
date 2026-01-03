# on shiny: https://lrouviere.github.io/VISU/pres_shiny.pdf
# on shinydashboard: https://cran.r-project.org/web/packages/shinydashboard/shinydashboard.pdf

library(shiny) # basic shiny library
library(rsconnect) # connect to shinyio
library(tidyverse) # contains dplyr and readr
library(haven) # read_sas
library(lubridate) # dates
library(leaflet) # interactive map
library(plotly) # interactive plots
library(shinydashboard) # dashboardPage
library(shinyWidgets) # airDatePicker to restrain to months selection in date range for evolution graph
library(htmltools) # for interactive map with labels when adding polygons to leaflet
library(sf) # st_read geojson data
library(bslib) # page_navBar

achats <- read_delim("Achats_csv.csv", delim=';') # library(readr)
clients <- read_sas("clients.sas7bdat") # library(haven)
correspondance <- read_delim("Correspondance_sites.csv", delim=';') # library(readr)


achats <- achats %>% rename(NUM_SITE = `Num Site`,
                            DATE_ACHAT = `Date Achat`,
                            MNT_ACHAT = `Mnt Achat`,
                            ID_CLIENT = `Id Client`,
                            NB_ACHAT = `Nb Achat`) # renommer les colonnes pour homogénéiser + jointure

data <- achats %>% # jointure des 3 tables
  left_join(clients, by = "ID_CLIENT") %>%
  left_join(correspondance, by = "NUM_SITE")

data <- data %>% filter(NUM_SITE != 7) # deleting rows with numero de site 7

data$DATE_ACHAT <- as.Date(data$DATE_ACHAT, format = "%d/%m/%Y") # converting to date

data <- data %>% # creating a new variable Tranche d'Age
  mutate(TrancheAge = case_when(
    2025 - year(data$DATE_NAIS) < 30 ~ "Moins de 30",
    2025 - year(data$DATE_NAIS) >= 30 & 2025 - year(data$DATE_NAIS) <= 45 ~ "Entre 30 et 45",
    2025 - year(data$DATE_NAIS) > 45 ~ "Plus de 45"
  ))

data <- data %>% mutate(CODE_DEPT = substr(COD_POSTAL,1,2))
# transforming COD_POSTAL column to match france_dept postal code col

# https://france-geojson.gregoiredavid.fr/
# importing geojson data with st_read
# code region: https://www.data.gouv.fr/datasets/departements-de-france?utm_source=chatgpt.com

dept_reg <- read_csv(file="departements-france.csv", col_names=TRUE) # read_csv => ',' by default as sep with col_names=TRUE
dept_reg <- dept_reg %>% select(code_departement, code_region)

data <- data %>% left_join(dept_reg, by = c("CODE_DEPT"="code_departement"))

ui <- page_navbar( # navigation bar
  title="Suivi des ventes e-commerce - Groupe Webtendance",
  bg = "black",
  nav_panel(title="Représentation temporelle",
            p( # first dashboardPage with Evolution Plot
              dashboardPage(
                dashboardHeader(title="Graphique d'évolution"), # subtitle
                dashboardSidebar(disable=TRUE), # remove empty sidebar
                dashboardBody( # corps du nav_panel 1 contenant sidebarPanel+mainPanel
                  fluidRow(
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("indicateur", "Indicateur:",
                                    choices = c("Volume de ventes", "Chiffre d'affaire")),
                        selectInput("site", "Choisir un site:",
                                    choices = c("Tous", unique(data$NOM_SITE))),
                        airDatepickerInput( # 
                          inputId = "dates", # input name to server
                          label = "Plage de dates:", # input name to user
                          range = TRUE, # selecting a range of dates
                          view = "months",        # showing months
                          minView = "months",     # selecting by month
                          dateFormat = "MM yyyy", # selecting by months and year
                          value = c(min(data$DATE_ACHAT),max(data$DATE_ACHAT)), # initial values
                          minDate = min(data$DATE_ACHAT), # lower bound in selection
                          maxDate = max(data$DATE_ACHAT), # upper bound in selection
                          autoClose = TRUE # date range closing automatically after selecting
                        ),
                        checkboxInput("filter", "Filtrer", FALSE), # dynamic filter option
                        conditionalPanel(
                          condition = "input.filter == true",
                          selectInput("filter_choice",
                                      "Filtrer selon:",
                                      choices = c("Sexe", "Tranche d'âge"))
                        )
                        ,width=4
                      )
                      ,
                      mainPanel(
                        plotlyOutput("evolution_plot"), # graph d'évolution
                        width=8
                      ),
                      position = c("left")) # relative position of sidebarPanel to MainPanel
                  )
                )
              )
              )
            )
            ,
  nav_panel(title="Représentation spatiale",
            p( # second dashboardPage with Interactive Map
              dashboardPage(
                dashboardHeader(title="Carte intéractive"),
                dashboardSidebar(disable=TRUE),
                dashboardBody( # corps du nav_panel 2 contenant sidebarPanel+mainPanel
                  fluidRow(
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("level","Niveau:",
                                    choices = c("Département", "Région"), selected = "Département"),
                        selectInput("indicateur", "Indicateur:",
                                    choices = c("Volume de ventes", "Chiffre d'affaire")),
                        selectInput("site", "Choisir un site:",
                                    choices = c("Tous", unique(data$NOM_SITE))),
                        dateRangeInput("dates", "Plage de dates:",
                                       start = min(data$DATE_ACHAT),
                                       end = max(data$DATE_ACHAT),
                                    min = min(data$DATE_ACHAT),
                                    max = max(data$DATE_ACHAT)),
                        width=4),
                      mainPanel(
                        leafletOutput("map"), # map output
                        width=8
                      ),
                      position = c("left") # sidebarPanel left to mainPanel
                      )
                    )
                )
            )
        )
    )
  )

server <- function(input, output, session) {
  
  data_filtre <- reactive({ # df will change over time depending on selected input by user
    df <- data
    if (input$site != "Tous") {
      df <- df %>% filter(NOM_SITE == input$site) # applying user choice on website
    }
    df <- df %>%
      filter(DATE_ACHAT >= input$dates[1],
             DATE_ACHAT <= input$dates[2]) # applying user choice on date range
    return(df)
  })
  
  output$evolution_plot <- renderPlotly({ # plotly, tydiverse, lubridate; reactive; pre-processed data
    df <- data_filtre()
    df <- df %>% mutate(Mois = floor_date(DATE_ACHAT, "month"))
    validate(
      need(length(input$dates) == 2, "Veuillez sélectionner une plage de deux dates.")
    ) # pas d'affichage si deux plages non sélectionnées
    if (input$filter == TRUE && "Sexe" == input$filter_choice) {
      df <- df %>% 
        mutate(Sexe=recode(COD_SEXE, "1" = "Homme", "2"="Femme")) %>% 
        group_by(Mois, Sexe) %>% 
        summarise( # creating new df, grouping by month+sexe, apply choice of KPI to sum over
          Valeur = if (input$indicateur == "Volume de ventes")
            sum(NB_ACHAT) else sum(MNT_ACHAT/1000000) # changing unit
        )
      p <- ggplot(df, aes(x = Mois, y = Valeur, color = Sexe)) +
        geom_line(size = 0.6) +
        labs(title = if (input$indicateur == "Volume de ventes")
          paste("Évolution du", tolower(input$indicateur)) else paste("Évolution du", tolower(input$indicateur),"(M €)"),
             x = NULL, y = NULL) + # conditional title, no x or y legends
        theme_classic()
    } else if (input$filter == TRUE && "Tranche d'âge" == input$filter_choice) {
      df <- df %>%
        group_by(Mois, TrancheAge) %>% # filtering by age
        summarise( # creating new df, grouping by month+trancheAge, apply choice of KPI to sum over
          Valeur = if (input$indicateur == "Volume de ventes") sum(NB_ACHAT) else sum(MNT_ACHAT/1000000) # changing unit
        )
      
      p <- ggplot(df, aes(x = Mois, y = Valeur, color = TrancheAge)) +
        geom_line(size = 0.6) +
        labs(title = if (input$indicateur == "Volume de ventes") paste("Évolution du", tolower(input$indicateur))
        else paste("Évolution du", tolower(input$indicateur),"(M €)"),
        x = NULL, y = NULL) + # conditional title, no x or y legends
        theme_classic()
      p$labels$colour <- "Tranche d'âge"
    } else {
      df <- df %>%
        group_by(Mois) %>% #no filter
        summarise(
          Valeur = if (input$indicateur == "Volume de ventes") sum(NB_ACHAT) else sum(MNT_ACHAT/1000000) # changing unit
        ) # creating new df, grouping by month+trancheAge, apply choice of KPI to sum over
        p <- ggplot(df, aes(x = Mois, y = Valeur)) +
          geom_line(color = "black", size = 0.6) +
          labs(title = if (input$indicateur == "Volume de ventes") paste("Évolution du", tolower(input$indicateur))
               else paste("Évolution du", tolower(input$indicateur),"(M €)"),
               x = NULL, y = NULL) + # conditional title, no x or y legends
          theme_classic()
    }
    ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.1)) # plotly object + relocating the legend 
  })  
  
  output$map <- renderLeaflet({ # leaflet, htmltools, dplyr, pre-processed data, reactive
    df <- data_filtre()
    
    if (input$level == "Département") {
        ventes_dept <- df %>% 
        group_by(CODE_DEPT) %>% # grouping by dept
        summarise(
          NbAchats = sum(NB_ACHAT, na.rm = TRUE),
          MontantTotal = sum(MNT_ACHAT, na.rm = TRUE)
        ) # computing nb achats et mt total sans tenir compte des NA
      
        map_data <- st_read("departements.geojson") %>% 
          left_join(ventes_dept, by=c("code"="CODE_DEPT"))
    }
    else if (input$level == "Région") {
          ventes_reg <- df %>% 
            group_by(code_region) %>% 
            summarise(
              NbAchats = sum(NB_ACHAT, na.rm = TRUE),
              MontantTotal = sum(MNT_ACHAT, na.rm = TRUE)
            ) # computing nb achats et mt total sans tenir compte des NA
          
          map_data <- st_read("regions.geojson",
        ) %>% left_join(ventes_reg, by=c("code"="code_region"))
    }
    
    variable <- if(input$indicateur=="Volume de ventes") map_data$NbAchats else map_data$MontantTotal/1000000 # change of unit
    
    variable[is.na(variable)] <- 0 # substituting NA with 0
    
    legend_title <- if(input$indicateur=="Volume de ventes") "Volume de ventes" else "Montant (millions €)" # legend for map
    
    palette <- colorNumeric("YlGnBu", domain = variable, na.color = "gray")
    # palette of colors to show geographic disparity accross regions/dept
    
    labels <- sprintf( # for info bubbles
      "<strong>%s</strong><br/>%s : %g", # %s string; %g generic number
      map_data$nom, legend_title, variable
    ) %>% lapply(HTML) # library(htmltools) lapply > sapply to preserve output format
    
    leaflet(map_data) %>% # leaflet with input containing polygons
      addProviderTiles(providers$CartoDB.Positron) %>% # geographical context
      setView(lng=2.8, lat=46.2, zoom=4.5) %>% # initial position
      addPolygons( # adding dept/reg
        fillColor = ~palette(variable),
        weight=1, # standard delimiters related
        color="white", # standard delimiters related
        dashArray="3", # info bubbles related
        fillOpacity=0.7, # setting to focus on info bubble
        label=labels, # info bubbles related
        highlightOptions = highlightOptions(weight=3, color="black", fillOpacity=0.9, bringToFront=TRUE) # info bubbles related
      ) %>%
      addLegend(
        pal=palette, # colors to show different levels
        values=variable, # KPI
        opacity=0.7,
        title=legend_title,
        position="bottomright"
      )
  })
}

shinyApp(ui = ui, server = server)