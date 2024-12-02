library(tidyverse)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)
library(ggsci)
library(leaflet)
library(bslib)
library(plotly)

data <- readRDS("full_dat.RDS")
data
map_data <- read_csv("colony_coordinates.csv")
unique(data)
# UI ----
ui <- page_sidebar(
  # HTML
  tags$head(
    tags$style(HTML("
      .note-container { margin: 10px 0; }
      .note-text { color: #007bff; cursor: pointer; text-decoration: underline; }
      .note-content { display: none; padding: 10px; border: 1px solid #007bff; margin-top: 5px; }
    ")),
    tags$script(HTML("
      $(document).on('click', '#note_text', function() {
        $('#note_content').toggle();
      });
    "))
  ),
  title = "Populasjonsdata for utvalgte sjøfuglbestander i Norge - SEAPOP",
  
  sidebar = sidebar(
    bg = "white",
    accordion(
      open = TRUE,
      accordion_panel(
        HTML("<b>Velg art</b>"),
        selectInput(
          inputId = "species",
          label = "Art:",
          choices = unique(data$Species_norsk),
          multiple = T,
          selected = "Lunde"
        )
      ),
      accordion_panel(
        HTML("<b>Velg koloni</b>"),
        selectInput(
          inputId = "keysite",
          label = "Koloni:",
          choices = NULL,
          multiple = T,
          selected = ""
        )
      ),
      # Download button for data table
      downloadButton("download",
                     label = "Download data selection"
      ),
    )
  ),
  navset_tab(
    nav_panel(
      HTML("<u>Visualisering av data</u>"),
  accordion(
    open = F,
    accordion_panel(
      HTML("<b>Populasjonsstørrelse</b>"),
      div(
        htmlOutput(outputId = "pop_text2"),
        uiOutput("pop_plot2")
      )
    ),
    accordion_panel(
      HTML("<b>Årlig overlevelse</b>"),
      div(
        htmlOutput(outputId = "surv_text", container = div),  
        uiOutput("surv_plot")
        )
    ),
    accordion_panel(
      HTML("<b>Hekkesuksess</b>"),
      div(
        htmlOutput(outputId = "breed_text", container = div),
        uiOutput("breed_plot")
      )
    )
  )
  ),
  nav_panel("Data",
              dataTableOutput(outputId = "data_table")
            ),
  nav_panel("Kart",
              leafletOutput("birdmap",
                            width = "auto",
                            height = "1000px"
              )
            ))
)


# Server ----
server <- function(input, output) {
  bs_themer() # show theme panel
  
  # filter data on selected species
  species <- reactive({ 
    filter(data, Species_norsk %in% input$species) 
  })
  
  # filter data on selected colony
  observeEvent(species(), {
    choices <- unique(species()$Colony)
    updateSelectInput(inputId = "keysite", choices = choices)
  })
  
 
  
  ### Plot population size v2 ###
  output$pop_plot2 <- renderUI({
    req(input$species)  # Ensure species input is available
    
    # Generate a plot for each selected species in the accordion panel
    lapply(input$species, function(species) {
      plotlyOutput(outputId = paste0("pop_plot2_", gsub(" ", "_", species)))
    })
  })
  
  # Render individual popiulation size plots dynamically
  observe({
    req(input$species, input$keysite)
    
    # Filter data for the selected species and colonies
    pdata <- data %>%
      filter(Parameter == "Pop_size") %>%
      filter(Species_norsk %in% input$species) %>%
      filter(Colony %in% input$keysite) %>% 
      group_by(Species_norsk, Colony) %>% 
      arrange(desc(Year)) %>% 
      ungroup()
    
    # Define a color palette for colonies (same color for each colony across species)
    colony_colors <- RColorBrewer::brewer.pal(n = max(3, length(unique(pdata$Colony))), "Set2")
    colony_color_map <- setNames(colony_colors[seq_along(unique(pdata$Colony))], unique(pdata$Colony))
    
    # Dynamically create individual survival plots for each species
    lapply(input$species, function(species) {
      species_data <- pdata %>% filter(Species_norsk == species)
      
      output[[paste0("pop_plot2_", gsub(" ", "_", species))]] <- renderPlotly({
        plot_ly(species_data,
                x = ~Year,
                y = ~Parameter_value,
                marker = list(size = 12),
                type = "scatter",
                mode = "lines+markers",
                color = ~Colony,
                colors = colony_color_map
        ) %>%
          layout(
            title = list(text = paste(species), font = list(size = 20)),
            xaxis = list(title = "År", dtick = 1, 
                         tickfont = list(size = 16),
                         titlefont = list(size = 16)),
            yaxis = list(title = "Antall hekkende fugl", 
                         tickfont = list(size = 16),
                         titlefont = list(size = 16)),
            legend = list(
              x = 1.05,
              y = 0.5,
              xanchor = "left",
              yanchor = "middle"
            ),
            margin = list(r = 200),  # Space for the legend
            updatemenus = list(
              list(
                type = "buttons",
                direction = "right",
                x = 0.75, # Position the button horizontally in the middle of the plot
                y = 1.1, # Position the button slightly above the plot
                xanchor = "center", # Anchor horizontally in the center
                yanchor = "top", # Anchor vertically at the top
                buttons = list(
                  list(
                    label = "Linear Scale",
                    method = "relayout",
                    args = list(list(yaxis = list(type = "linear")))
                  ),
                  list(
                    label = "Log Scale",
                    method = "relayout",
                    args = list(list(yaxis = list(type = "log")))
                  )
                ), # Change font size and style here
                font = list(
                  size = 16, # Set the font size for the button label
                  color = "black", # Optional: Set font color
                  family = "Arial" # Optional: Change font family
                )
              )
            )
          )
      })
    })
  })
  
  output$pop_text2 <- renderUI({
    HTML('Data på populasjonsstørrelse er ikkje lagt inn per 29.11.24. Det som vises her er simulert data og samsvarer ikkje med reelle tall.')
  })
  
  ### Plot survival ###
  
  output$surv_plot <- renderUI({
    req(input$species)  # Ensure species input is available
    
    # Generate a plot for each selected species in the accordion panel
    lapply(input$species, function(species) {
      plotlyOutput(outputId = paste0("surv_plot_", gsub(" ", "_", species)))
    })
  })
  
  # Render individual plots dynamically
  observe({
    req(input$species, input$keysite)
    
    # Filter data for the selected species and colonies
    pdata <- data %>%
      filter(Parameter == "Survival") %>%
      filter(Species_norsk %in% input$species) %>%
      filter(Colony %in% input$keysite)%>% 
      group_by(Species_norsk, Colony) %>% 
      arrange(desc(Year)) %>% 
      ungroup()
    
    # Define a color palette for colonies (same color for each colony across species)
    colony_colors <- RColorBrewer::brewer.pal(n = max(3, length(unique(pdata$Colony))), "Set2")
    colony_color_map <- setNames(colony_colors[seq_along(unique(pdata$Colony))], unique(pdata$Colony))
    
    # Dynamically create individual survival plots for each species
    lapply(input$species, function(species) {
      species_data <- pdata %>% filter(Species_norsk == species)
      
      output[[paste0("surv_plot_", gsub(" ", "_", species))]] <- renderPlotly({
        plot_ly(species_data,
                x = ~Year,
                y = ~Parameter_value,
                marker = list(size = 10),
                type = "scatter",
                mode = "lines+markers",
                color = ~Colony,
                colors = colony_color_map,
                error_y = list(
                  array = ~ CI_high - Parameter_value,
                  arrayminus = ~ Parameter_value - CI_low
                )
        ) %>%
          layout(
            title = list(text = paste(species), font = list(size = 16)),
            xaxis = list(title = "År", dtick = 1, 
                         tickfont = list(size = 16),
                         titlefont = list(size = 16)),
            yaxis = list(title = "Overlevelse", 
                         tickfont = list(size = 16),
                         titlefont = list(size = 16)),
            legend = list(
              x = 1.05,
              y = 0.5,
              xanchor = "left",
              yanchor = "middle"
            ),
            margin = list(r = 200)  # Space for the legend
          )
      })
    })
  })
  
  ## Tekstboks med forklaring til overlevelse
  output$surv_text <- renderUI({
    req(input$species, input$keysite)
    
    # Create the dynamic description based on species and colonies
    surv_species <- paste0(
      "Grafen viser årlig overlevelse for ", paste(input$species, collapse = ", "), 
      " fra ", paste(input$keysite, collapse = ", "), "."
    )
    
    # Create the detailed HTML content
    html_content <- paste0(
      '<p>', surv_species, 
      ' Overlevelse og <span id="note_text" class="note-text">konfidensintervall</span> er oppgitt for hvert år. Overlevelse er basert på «fangst-gjenfangst data» som blir gjennomført på utvalgte nøkkellokaliteter hvert år. Et utvalg individuelle fugler blir merket med en fargering med en unik kode og basert på observasjoner av disse fra år til år kan en beregne gjennomsnittlig årlig overlevelse over samme tidsperiode.</p>',
      '<div class="note-container">',
      '<div id="note_content" class="note-content">',
      '<h3>Konfidensintervall</h3>',
      '<p>Konfidensintervall er et mål på usikkerheten til et estimat og gir en nedre og øvre grense som med en viss grad av sikkerhet inneholder den "reelle" verdien til estimatet (her overlevelse).</p>',
      '</div>',
      '</div>'
    )
    
    # Return the updated HTML content inside a div
    div(HTML(html_content))
  })
  

  
  ### Plot breeding success ###
  output$breed_plot <- renderUI({
    req(input$species)  # Ensure species input is available
    
    # Generate a plot for each selected species in the accordion panel
    lapply(input$species, function(species) {
      plotlyOutput(outputId = paste0("breed_plot_", gsub(" ", "_", species)))
    })
  })
  
  # Render individual breeding success plots dynamically
  observe({
    req(input$species, input$keysite)
    
    # Filter data for the selected species and colonies
    pdata <- data %>%
      filter(Parameter == "Breeding_success") %>%
      filter(Species_norsk %in% input$species) %>%
      filter(Colony %in% input$keysite)%>% 
      group_by(Species_norsk, Colony) %>% 
      arrange(desc(Year)) %>% 
      ungroup()
    
    # Define a color palette for colonies (same color for each colony across species)
    colony_colors <- RColorBrewer::brewer.pal(n = max(3, length(unique(pdata$Colony))), "Set2")
    colony_color_map <- setNames(colony_colors[seq_along(unique(pdata$Colony))], unique(pdata$Colony))
    
    # Dynamically create individual breeding plots for each species
    lapply(input$species, function(species) {
      species_data <- pdata %>% filter(Species_norsk == species)
      
      output[[paste0("breed_plot_", gsub(" ", "_", species))]] <- renderPlotly({
        plot_ly(species_data,
                x = ~Year,
                y = ~Parameter_value,
                marker = list(size = 10),
                type = "scatter",
                mode = "lines+markers",
                color = ~Colony,
                colors = colony_color_map
        ) %>%
          layout(
            title = list(text = paste(species), font = list(size = 16)),
            xaxis = list(title = "År", dtick = 1, 
                         tickfont = list(size = 16),
                         titlefont = list(size = 16)),
            yaxis = list(title = "Hekkesuksess", 
                         tickfont = list(size = 16),
                         titlefont = list(size = 16)),
            legend = list(
              x = 1.05,
              y = 0.5,
              xanchor = "left",
              yanchor = "middle"
            ),
            margin = list(r = 200)  # Space for the legend
          )
      })
    })
  })
  
  ## Tekstboks med forklaring til hekkesuksess
  output$breed_text <- renderUI({
    req(input$species, input$keysite)
    
    # Create the dynamic description based on species and colonies
    breed_species <- paste0(
      "Grafen viser årlig hekkesuksess for ", paste(input$species, collapse = ", "), 
      " fra ", paste(input$keysite, collapse = ", "), "."
    )
    
    # Create HTML content
    html_content_breed <- paste0(
      '<p>', breed_species, ' Hekkesuksess er definert på flere måter basert på utvalgte arters livshistorie og overvåkingsmetodikk for de ulike nøkkellokalitetene. 
      Alkefuglene (bortsett fra teist), i tillegg til havhest og havsule, legger kun ett egg, mens de resterende har en kullstørrelse fra to og oppover. 
      For de kolonihekkende artene, f.eks lunde og havsule, er hekkesuksess målt på et utvalg av populasjonen og de årlige estimatene har derfor et konfidensintervall som beskriver usikkerheten på estimatet.
      For noen arter/områder er hekkesuksess målt på samtlige individ i populasjonen og det er derfor ikke noe usikkerhet knyttet til de årlige verdiene av hekkesuksess.
      </p>'
    )
    
    
    # Return a div container with the updated content
    div(HTML(html_content_breed))
  })
  
  ### Print data table if checked ###
  output$data_table <- renderDataTable({
    req(input$species, input$keysite)
    data %>%
      filter(Species_norsk %in% input$species) %>%
      filter(Colony %in% input$keysite) %>%
      DT::datatable(
        options = list(
          pageLength = 50,
          digits = 3
        ),
        rownames = FALSE
      ) %>%
      formatRound(columns = c("Parameter_value", "CI_low", "CI_high", "SE"), digits = 3)
  })
  
  # Download data when pressing download button
  output$download <- downloadHandler(
    filename = function() {
      paste0("data.csv")
    },
    content = function(file) {
      write.csv(data %>%
                  filter(Species %in% input$species) %>%
                  filter(Colony %in% input$keysite), file)
    }
  )
  
  # Show map of selected populations
  
  output$birdmap <- renderLeaflet({
    req(input$keysite) ## Check if all inputs are selected
    map_data_filtered <- map_data %>% 
      filter(Colony %in% input$keysite) 
    
    map_data_filtered %>% 
      leaflet() %>%
        addTiles() %>%
        addMarkers(data = map_data_filtered[, 2:3])
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)