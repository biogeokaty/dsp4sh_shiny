# Shiny app for visualizing DSP4SH project information and data

# Load packages ------
library(shiny)
library(bslib)
library(DT)
library(leaflet)
library(tidyverse)

# Load data ----
treatment_table <- readRDS("treatment_table.rds")
project_annotate<- readRDS("project_annotate.rds")
surf_long <- readRDS("surf_long.rds")
project_labels <- readRDS("project_labels.rds")
indicator_labs_df <- readRDS("indicator_labs_df.rds")
dsp4sh_slab <- readRDS("dsp4sh_slab.rds")

# Define UI ----
ui <- page_fluid(
  titlePanel(title="Dynamic Soil Properties for Soil Health Data Explorer"),
  
  # Make page navigable by underlined tabs
  navset_card_underline(
    # Tab for site information
    nav_panel("Project overview",
              # Make description and map two cards side-by-side
              layout_columns(
                card(card_header("Dynamic Soil Properties for Soil Health (DSP4SH)"),
                     tags$p(
                       "DSP4SH is a cooperative effort organized by the USDA-Natural Resources Conservation Service Soil and Plant Science Division to collect standardized soil health metrics nationwide. DSP4SH projects investigate regionally-relevant agricultural systems and management strategies. In each project, soil health samples were collected from locations with a common soil series representing business as usual agriculture (BAU), soil health management agriculture (SHM), or perennial reference ecosystems (Ref). Soil health metrics examined include soil organic carbon, bulk density, aggregate stability, protein content, permanganate-oxidizable carbon, and enzyme activities."
                            ),
                     tags$p(
                       "The full DSP4SH dataset (version 1.0) is available at Ag Data Commons: Adeleke, Ekundayo; Wills, Skye; Carter, Tiffany (2024). Dynamic Soil Properties for Soil Health Database v.1.0.",
                            tags$a(
                              "https://doi.org/10.15482/USDA.ADC/25122323.v1.", href="https://doi.org/10.15482/USDA.ADC/25122323.v1")
                            )
                     ),
                
                # Card with map
                card(
                  card_header("Map of DSP4SH project locations"),
                  leafletOutput(outputId = "map"),
                  height=500
                )
              ),

      # Data table of site information - goes in a card below the project description and map
      card(
        card_header("DSP4SH project location and management details"),
        DT::dataTableOutput(outputId = "project_dt")
        )
      ),
    
    # Tab for surface soil indicator plots
    nav_panel("Soil health indicators - surface soils (0-10 cm)",
              layout_sidebar(
              # Sidebar
              sidebar = sidebar(
                
                # Select properties to display
                radioButtons(inputId="indicator_surface",
                             label="Choose a soil health indicator to display:",
                             choices=c("Bulk density" = "bulk_density",
                                       "Aggregate stability" = "kssl_wsa",
                                       "Aggregate MWD" = "yoder_agg_stab_mwd",
                                       "SOC %" = "soc_pct",
                                       "ACE protein" = "ace",
                                       "POX-C" = "pox_c",
                                       "Respiration" = "soil_respiration",
                                       "Betaglucosidase activity" = "bglucosidase",
                                       "NAG activity" = "bglucosaminidase",
                                       "AcidP activity" = "acid_phosphatase",
                                       "AlkP activity" = "alkaline_phosphatase",
                                       "AS activity" = "arylsulfatase")
                )
              ),
              
              # Pooled data- tab for boxplot and summary data
              navset_card_tab(
                title=textOutput(outputId="boxplot_title"),
                nav_panel("Boxplot", plotOutput(outputId = "indicator_boxplot")),
                nav_panel("Summary data", DT::dataTableOutput(outputId="indicator_summary_surface")),
                ),
              
              # Density plots - tab for each project
            navset_card_tab(
              title="Density plot",
               nav_menu("Select a project",
                 nav_panel("University of Illinois", plotOutput(outputId = "indicator_density_plot1"), textOutput("pedon_n_density1")),
                 nav_panel("Kansas State", plotOutput(outputId = "indicator_density_plot2"), textOutput("pedon_n_density2")),
                 nav_panel("North Carolina State", plotOutput(outputId = "indicator_density_plot3"), textOutput("pedon_n_density3")),
                 nav_panel("Oregon State", plotOutput(outputId = "indicator_density_plot4"), textOutput("pedon_n_density4")),
                 nav_panel("Texas A&M - 1", plotOutput(outputId = "indicator_density_plot5"), textOutput("pedon_n_density5")),
                 nav_panel("Texas A&M - 2", plotOutput(outputId = "indicator_density_plot6"), textOutput("pedon_n_density6")),
                 nav_panel("University of Connecticut", plotOutput(outputId = "indicator_density_plot7"), textOutput("pedon_n_density7")),
                 nav_panel("University of Texas RGV", plotOutput(outputId = "indicator_density_plot8"), textOutput("pedon_n_density8")),
                 nav_panel("University of Minnesota", plotOutput(outputId = "indicator_density_plot9"), textOutput("pedon_n_density9")),
                 nav_panel("Washington State", plotOutput(outputId = "indicator_density_plot10"), textOutput("pedon_n_density10"))
                 ),
               id="density_tab")
    )
    ),
    
    nav_panel("Soil health indicators - full soil profile",
              layout_sidebar(
                # Sidebar
                sidebar = sidebar(
                  
                  # Select properties to display
                  radioButtons(inputId="indicator_depth",
                               label="Choose a soil health indicator to display:",
                               choices=c("Bulk density" = "bulk_density",
                                         "Aggregate stability" = "kssl_wsa",
                                         "Aggregate MWD" = "yoder_agg_stab_mwd",
                                         "SOC %" = "soc_pct",
                                         "ACE protein" = "ace",
                                         "POX-C" = "pox_c",
                                         "Respiration" = "soil_respiration",
                                         "Betaglucosidase activity" = "bglucosidase",
                                         "NAG activity" = "bglucosaminidase",
                                         "AcidP activity" = "acid_phsophatase",
                                         "AlkP activity" = "alkaline_phosphatase",
                                         "AS activity" = "arylsulfatase")
                  )
                ),
              navset_card_tab(
                title=textOutput(outputId="depth_plot_title"),
                nav_menu("Select a project",
                         nav_panel("University of Illinois", plotOutput(outputId = "indicator_depth_plot1"), textOutput("pedon_n_depth1")),
                         nav_panel("Kansas State", plotOutput(outputId = "indicator_depth_plot2"), textOutput("pedon_n_depth2")),
                         nav_panel("North Carolina State", plotOutput(outputId = "indicator_depth_plot3"), textOutput("pedon_n_depth3")),
                         nav_panel("Oregon State", plotOutput(outputId = "indicator_depth_plot4"), textOutput("pedon_n_depth4")),
                         nav_panel("Texas A&M - 1", plotOutput(outputId = "indicator_depth_plot5"), textOutput("pedon_n_depth5")),
                         nav_panel("Texas A&M - 2", plotOutput(outputId = "indicator_depth_plot6"), textOutput("pedon_n_depth6")),
                         nav_panel("University of Connecticut", plotOutput(outputId = "indicator_depth_plot7"), textOutput("pedon_n_depth7")),
                         nav_panel("University of Texas RGV", plotOutput(outputId = "indicator_depth_plot8"), textOutput("pedon_n_depth8")),
                         nav_panel("University of Minnesota", plotOutput(outputId = "indicator_depth_plot9"), textOutput("pedon_n_depth9")),
                         nav_panel("Washington State", plotOutput(outputId = "indicator_depth_plot10"), textOutput("pedon_n_depth10"))
                ),
                id="depth_tab"),
              
              card(
                card_header("Summary data"),
                DT::dataTableOutput(outputId = "indicator_summary_depth")
              )
    )
  )
  )
)


# Define server logic ----
server <- function(input, output) {
  
  # Build data inputs ----
  
  # Reactive outputs to create labels indicator boxplots
  data_indicator_labs <- reactive({
    indicator_labs_df %>%
      filter(indicator %in% as.vector(input$indicator_surface))
    })
  
  # Reactive outputs to create labels for indicator depth plots
  data_indicator_labs_depth <- reactive({
    indicator_labs_df %>%
      filter(indicator %in% as.vector(input$indicator_depth))
  })
  
  # Lookup table for selected project under density plot dropdown
  density_lookup <- reactive({
    project_annotate %>%
      filter(project_label %in% as.vector(input$density_tab))
  })
  
  # Lookup table for selected project under depth plot dropdown
  depth_lookup <- reactive({
    project_annotate %>%
      filter(project_label %in% as.vector(input$depth_tab))
  })
  
  # Reactive output to create boxplot and summary table for surface depth only
  data_surf <- reactive({
    surf_long %>%
      filter(indicator %in% as.vector(input$indicator_surface)) 
  })
  
  # Reactive outputs to create density plot for surface depth only
  data_density_plot <- reactive({
    surf_long %>%
      filter(indicator %in% as.vector(input$indicator_surface),
             project %in% as.vector(density_lookup()$project)) 
  })
  
  # Reactive outputs to create depth plots
  data_profile <- reactive({
    dsp4sh_slab %>%
      filter(indicator %in% as.vector(input$indicator_depth),
             project %in% as.vector(depth_lookup()$project))
  })
  
  # Reactive outputs for pedon n below density plots
  data_pedon_n_density <- reactive({
    surf_long %>%
      filter(project %in% as.vector(density_lookup()$project),
             indicator %in% as.vector(input$indicator_surface)) %>%
      na.omit() %>%
      count(label)
  })
  
  # Reactive outputs for pedon n below depth plots
  data_pedon_n_depth <- reactive({
    surf_long %>%
      filter(project %in% as.vector(depth_lookup()$project),
             indicator %in% as.vector(input$indicator_depth)) %>%
      na.omit() %>%
      count(label)
  })
  
  # Render plots and tables ----
  
  # Card titles for indicator boxplots
  output$boxplot_title <- renderText({
    paste(data_indicator_labs()$indicator_label, " in top 10 cm - all projects") 
  })

  # Card titles for indicator depth plots
  output$depth_plot_title <- renderText({
    paste(data_indicator_labs_depth()$indicator_label, " across soil profile") 
  })
  
  # Map of project locations
  output$map <- renderLeaflet({

    # Icons for map 
    mapIcons <- iconList(BAU = makeIcon("dot_bau.svg", iconWidth = 24, iconHeight =32),
                         SHM = makeIcon("dot_shm.svg", iconWidth = 24, iconHeight =32),
                         Ref = makeIcon("dot_ref.svg", iconWidth = 24, iconHeight =32))

    leaflet() %>%
      addTiles()  %>%
      setView(lng = -96.25, lat = 39.50, zoom = 4) %>%
      addMarkers(data=project_annotate, 
                 lng=~avg_long,
                 lat=~avg_lat, 
                 icon=~mapIcons[label],
                 popup=paste0(project_annotate$project_label, "<hr>", 
                              project_annotate$management, "<br>", "Number of pedons: ", project_annotate$pedon_n),
                 label=~project_label)
    
  })
  
  # Table of project treatment data
  output$project_dt <- DT::renderDataTable({
    
    treatment_table %>%
      dplyr::select(-lookup) %>%
      datatable(colnames = c("Project",
                             "Soil Series",
                             "Taxonomy",
                             "MAT",
                             "MAP",
                             "Treatment",
                             "Description",
                             "Number of pedons"))
    
  })
  
  # Surface indicator values boxplot
  output$indicator_boxplot <- renderPlot({
    
    ggplot(data_surf(),
             aes(x = project, y = value, fill=label)) +
      geom_boxplot() +
      labs(x="Project", y=paste(data_indicator_labs()$units_long)) +
      scale_x_discrete(labels=project_labels) +
      scale_fill_manual(values=c("#FED789FF","#72874EFF","#476F84FF"),
                        name="Management") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            text = element_text(size = 14))
     
  })
  
  output$indicator_density_plot1 <- output$indicator_density_plot2 <- output$indicator_density_plot3 <- output$indicator_density_plot4 <- output$indicator_density_plot5 <- output$indicator_density_plot6 <- output$indicator_density_plot7 <- output$indicator_density_plot8 <- output$indicator_density_plot9 <- output$indicator_density_plot10 <- renderPlot({

      ggplot(data_density_plot(), aes(x=value, fill=label)) +
      geom_density(alpha=0.75) +
      labs(title=paste(density_lookup()$project_label),
           x=paste(data_indicator_labs()$units_long), y="Density") +
      scale_fill_manual(values=c("#FED789FF","#72874EFF","#476F84FF"),
                        name="Management") +
      theme_classic() +
      theme(text = element_text(size = 14))

  })
  
  output$pedon_n_density1 <- output$pedon_n_density2 <- output$pedon_n_density3 <- output$pedon_n_density4 <- output$pedon_n_density5 <- output$pedon_n_density6 <- output$pedon_n_density7 <- output$pedon_n_density8 <- output$pedon_n_density9 <- output$pedon_n_density10 <- renderText({
    paste("BAU: ", {filter(data_pedon_n_density(), label=="BAU")$n}, " pedons, ",
          "SHM: ", {filter(data_pedon_n_density(), label=="SHM")$n}, " pedons, ",
          "Ref: ", {filter(data_pedon_n_density(), label=="Ref")$n}, " pedons")
  })
  
  output$indicator_summary_surface <- DT::renderDataTable({
      
      data_surf() %>%
      group_by(project, label, indicator_label, units_only) %>%
      na.omit() %>%
      summarize(mean = mean(value, na.rm=TRUE),
                sd=sd(value, na.rm=TRUE),
                n=n()) %>%
      relocate(indicator_label, .before=project) %>%
      relocate(units_only, .before=project) %>%
      arrange(project, label) %>%
      mutate(across(where(is.numeric), ~round(.x, 2))) %>%
      datatable(colnames = c('Project' = 'project',
                             "Management" = "label",
                             "Indicator" = "indicator_label",
                             "Units" = "units_only",
                             "Mean" = "mean",
                             "Standard Deviation" = "sd"))
  })
  
  output$indicator_summary_depth <- DT::renderDataTable({
      data_profile() %>%
      select(-contributing_fraction, -p.q5, -p.q95, -indicator, -units_long) %>%
      unite("depth_increment", top:bottom, sep="-") %>%
      relocate(depth_increment, .after=label) %>%
      relocate(indicator_label, .before=project) %>%
      relocate(units_only, .after=indicator_label) %>%
      arrange(project, label, depth_increment) %>%
      mutate(across(where(is.numeric), ~round(.x, 2))) %>%
      datatable(colnames = c('Project' = 'project',
                             "Management" = "label",
                             "Indicator" = "indicator_label",
                             "Units" = "units_only",
                             "Soil Depth (cm)" = "depth_increment",
                             "1st Quartile" = "p.q25",
                             "Median" = "p.q50",
                             "3rd Quartile" = "p.q75"))
  })
  
  output$indicator_depth_plot1 <- output$indicator_depth_plot2 <- output$indicator_depth_plot3 <- output$indicator_depth_plot4 <- output$indicator_depth_plot5 <- output$indicator_depth_plot6 <- output$indicator_depth_plot7 <- output$indicator_depth_plot8 <- output$indicator_depth_plot9 <- output$indicator_depth_plot10 <- renderPlot({
  ggplot(data_profile(),
         aes(x=top, y=p.q50)) +
    geom_line(aes(color=label), linewidth=2) +
    geom_ribbon(aes(ymin=p.q25, ymax=p.q75, x=top, fill=label), alpha=0.4) +
    xlim(c(100,0)) +
    coord_flip() +
    labs(x="Depth (cm)", y=paste(data_indicator_labs_depth()$units_long),
         title=paste(depth_lookup()$project_label)) +
    scale_fill_manual(values=c("#FED789FF","#72874EFF","#476F84FF"),
                      breaks=c("BAU", "SHM", "Ref"), 
                      guide="none") +
    scale_color_manual(values=c("#FED789FF","#72874EFF","#476F84FF"),
                       breaks=c("BAU", "SHM", "Ref"), 
                       name="Management") +
    theme_classic() +
    theme(text = element_text(size = 14))
  })
  
  output$pedon_n_depth1 <- output$pedon_n_depth2 <- output$pedon_n_depth3 <- output$pedon_n_depth4 <- output$pedon_n_depth5 <- output$pedon_n_depth6 <- output$pedon_n_depth7 <- output$pedon_n_depth8 <- output$pedon_n_depth9 <- output$pedon_n_depth10 <- renderText({
    paste("BAU: ", {filter(data_pedon_n_depth(), label=="BAU")$n}, " pedons, ",
          "SHM: ", {filter(data_pedon_n_depth(), label=="SHM")$n}, " pedons, ",
          "Ref: ", {filter(data_pedon_n_depth(), label=="Ref")$n}, " pedons")
  })
      
}

# Run the application ----
shinyApp(ui, server)
