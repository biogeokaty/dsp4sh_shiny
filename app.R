# Shiny app for visualizing DSP4SH project information and data

# Load packages ------
library(shiny)
library(bslib)
library(DT)
library(leaflet)
library(plotly)
library(paletteer)
library(ggpubr)
library(Hmisc)
library(tidyverse)

# Load data ----
surf <- readRDS("surf.rds")
treatment_table <- readRDS("treatment_table.rds")
treat_n <- readRDS("treat_n.rds")
project_annotate<- readRDS("project_annotate.rds")
surf_long <- readRDS("surf_long.rds")
dsp4sh_dice_in <- readRDS("dsp4sh_dice_in.rds")

# Make choice lists ----
project_list <- list("University of Connecticut" = "UConn",
                     "University of Illinois" = "Illinois",
                     "Kansas State" = "KansasState",
                     "University of Minnesota" = "UnivOfMinnesota",
                     "North Carolina State" = "NCState",
                     "Oregon State" = "OregonState",
                     "Texas A&M - 1" = "TexasA&MPt-1",
                     "Texas A&M - 2" = "TexasA&MPt-2",
                     "University of Texas RGV" = "UTRGV",
                     "Washington State" = "WashingtonState")

indicator_list <- list("Bulk density" = "bulk_density",
                       "Aggregate stability" = "kssl_wsa",
                       "Aggregate MWD" = "yoder_agg_stab_mwd",
                       "SOC %" = "soc_pct",
                       "ACE protein" = "ace",
                       "POX-C" = "pox_c",
                       "Soil respiration" = "soil_respiration",
                       "Betaglucosidase activity" = "bglucosidase",
                       "NAG activity" = "bglucosaminidase",
                       "AcidP activity" = "acid_phsophatase",
                       "AlkP activity" = "alkaline_phosphatase",
                       "AS activity" = "arylsulfatase")

# Project labels
project_labels <- c("UnivOfMinnesota" = "University of Minnesota",
                    "UTRGV" = "University of Texas RGV",
                    "WashingtonState" = "Washington State",
                    "TexasA&MPt-1" = "Texas A&M - 1",
                    "TexasA&MPt-2" = "Texas A&M - 2",
                    "NCState" = "North Carolina State",
                    "Illinois" = "University of Illinois",
                    "OregonState" = "Oregon State",
                    "KansasState" = "Kansas State",
                    "UConn" = "University of Connecticut")

# Make vector of indicator labels so they will print nicely
indicator_labs <- c("soc_pct" = "SOC",
                    "soc_stock_100cm" = "SOC stock (100 cm depth)",
                    "soc_stock_0_30cm" = "SOC stock (30 cm depth)",
                    "bglucosaminidase" = "NAG activity",
                    "ace" = "ACE protein",
                    "bglucosidase" = "BG activity",
                    "kssl_wsa" = "Aggregate stability",
                    "yoder_agg_stab_mwd" = "Aggregate MWD",
                    "arylsulfatase" = "AS activity",
                    "pox_c" = "POX-C",
                    "tn_pct" = "Total N%",
                    "bulk_density" = "Bulk density",
                    "soil_respiration" = "Soil respiration",
                    "phosphodiesterase" = "Phosphodiesterase",
                    "alkaline_phosphatase" = "AlkP activity",
                    "acid_phosphatase" = "AcidP activity",
                    "p_h" = "pH")

# Also make into dataframe with units (need this to use labels within map functions)
indicator_labs_df <- data.frame(indicator_labs) %>%
  rownames_to_column() %>%
  rename(indicator = rowname,
         indicator_label = indicator_labs) %>%
  mutate(units_long = case_when(indicator=="soc_pct" ~ "SOC (%)",
                                indicator=="bglucosaminidase" ~ "NAG activity (mg kg-1 hr-1)",
                                indicator=="ace" ~ "ACE protein (g kg-1)",
                                indicator=="bglucosidase" ~ "BG activity (mg kg-1 hr-1)",
                                indicator=="kssl_wsa" ~ "Aggregate stability (%)",
                                indicator=="yoder_agg_stab_mwd" ~ "Aggregate MWD (mm)",
                                indicator=="arylsulfatase" ~ "AS activity (mg kg-1 hr-1)",
                                indicator=="pox_c" ~ "POX-C (mg kg-1)",
                                indicator=="bulk_density" ~ "Bulk density (g cm-3)",
                                indicator=="soil_respiration" ~ "Soil respiration (mg per 4 days)",
                                indicator=="alkaline_phosphatase" ~ "AlkP activity (mg kg-1 hr-1)",
                                indicator=="acid_phosphatase" ~ "AcidP activity (mg kg-1 hr-1)"),
         units_only = case_when(indicator=="soc_pct" ~ "%",
                                indicator=="bglucosaminidase" ~ "mg kg-1 hr-1",
                                indicator=="ace" ~ "g kg-1",
                                indicator=="bglucosidase" ~ "mg kg-1 hr-1",
                                indicator=="kssl_wsa" ~ "%",
                                indicator=="yoder_agg_stab_mwd" ~ "mm",
                                indicator=="arylsulfatase" ~ "mg kg-1 hr-1",
                                indicator=="pox_c" ~ "mg kg-1",
                                indicator=="bulk_density" ~ "g cm-3",
                                indicator=="soil_respiration" ~ "mg per 4 days",
                                indicator=="alkaline_phosphatase" ~ "mg kg-1 hr-1",
                                indicator=="acid_phosphatase" ~ "mg kg-1 hr-1"))
  

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
                            ),
                     textOutput(outputId="treat_count")
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
    nav_menu(
      "Explore indicator data in surface soils (0-10 cm)",
      
      nav_panel("Within-project data distribution",
                layout_sidebar(
                  sidebar=sidebar(
                    # Select properties to display
                    selectInput(inputId="density_indicator",
                                label="Choose a soil health indicator to display:",
                                choices=indicator_list
                    ),
                    # Select project to display
                    selectInput(inputId="density_project",
                                label="Choose a project to display:",
                                choices=project_list
                    ),
                    selectInput(inputId="density_trt",
                                label="Choose a grouping variable",
                                choices=c("Management group" = "label",
                                          "Land use" = "lu",
                                          "Agriculture vs non-agriculture" = "agriculture",
                                          "Perennial" = "perennial",
                                          "Tillage" = "till",
                                          "Cover crop use" = "cover_crops",
                                          "Crop" = "crop"))
                    
                  ),
                  
                  card(
                    plotlyOutput(outputId="density_plot"),
                    textOutput("density_n"),
                    id="density",
                    height=600)
                  
                )
      ),

      nav_panel("Compare between projects",
                layout_sidebar(
                  # Sidebar
                  sidebar = sidebar(
                    
                    # Select properties to display
                    selectInput(inputId="trt_indicator",
                                label="Choose a soil health indicator to display:",
                                choices=indicator_list
                    ),
                    
                    selectInput(inputId="trt_trt",
                                label="Choose a primary grouping variable:",
                                choices=c("Project" = "project",
                                          "Soil suborder" = "suborder",
                                          "Management group" = "label",
                                          "Land use" = "lu",
                                          "Agriculture vs non-agriculture" = "agriculture",
                                          "Perennial" = "perennial",
                                          "Tillage" = "till",
                                          "Cover crop use" = "cover_crops",
                                          "Crop" = "crop"),
                                selected="project"
                    ),
                    
                    selectInput(inputId="trt_group2",
                                label="Choose a secondary grouping variable:",
                                choices=c("Project" = "project",
                                          "Soil suborder" = "suborder",
                                          "Management group" = "label",
                                          "Land use" = "lu",
                                          "Agriculture vs non-agriculture" = "agriculture",
                                          "Perennial" = "perennial",
                                          "Tillage" = "till",
                                          "Cover crop use" = "cover_crops",
                                          "Crop" = "crop"),
                                selected="label"
                    ),
                    
                    checkboxGroupInput(inputId="trt_project",
                                       label="Choose projects to include:",
                                       choices=project_list,
                                       selected=project_list
                    )
                    
                  ),
                  
                  navset_card_tab(
                    nav_panel("Boxplot",
                              plotlyOutput(outputId = "trt_plot")),
                    nav_panel("Summary data",
                              DT::dataTableOutput(outputId="trt_summary")),
                    height=500
                  )
                )
      ),
      
      nav_panel("Compare soil health indicators",
                layout_sidebar(
                  # Sidebar
                  sidebar = sidebar(
                    
                    # Select properties to display
                    selectInput(inputId="indicator_compare_y",
                                label="Choose a soil health indicator to display on y-axis:",
                                choices=indicator_list,
                                selected="soc_pct"
                    ),
                    selectInput(inputId="indicator_compare_x",
                                label="Choose a soil health indicator to display on x-axis:",
                                choices=indicator_list,
                                selected="ace"
                    ),
                    selectInput(inputId="indicator_compare_trt",
                                label="Choose a primary grouping variable (color):",
                                choices=c("Project" = "project",
                                          "Soil suborder" = "suborder",
                                          "Management group" = "label",
                                          "Land use" = "lu",
                                          "Agriculture vs non-agriculture" = "agriculture",
                                          "Perennial" = "perennial",
                                          "Tillage" = "till",
                                          "Cover crop use" = "cover_crops",
                                          "Crop" = "crop"),
                                selected="project"
                    ),
                    
                    selectInput(inputId="indicator_compare_group2",
                                label="Choose a secondary grouping variable (shape):",
                                choices=c("Project" = "project",
                                          "Soil suborder" = "suborder",
                                          "Management group" = "label",
                                          "Land use" = "lu",
                                          "Agriculture vs non-agriculture" = "agriculture",
                                          "Perennial" = "perennial",
                                          "Tillage" = "till",
                                          "Cover crop use" = "cover_crops",
                                          "Crop" = "crop"),
                                selected="label"
                    ),
                    checkboxGroupInput(inputId="indicator_compare_project",
                                       label="Choose projects to include:",
                                       choices=project_list,
                                       selected=project_list
                    )
                  ),
                  
                  card(
                   plotlyOutput(outputId = "compare_plot")
                  )
                )
      )
    ),
    
   nav_panel("Explore indicator data in full soil profile",
              layout_sidebar(
                # Sidebar
                sidebar = sidebar(
                  
                  # Select properties to display
                  selectInput(inputId="depth_indicator",
                               label="Choose a soil health indicator to display:",
                               choices=indicator_list
                  ),
                  
                  # Select project to display
                  selectInput(inputId="depth_project",
                              label="Choose a project to display:",
                              choices=project_list
                  ),
                  
                  selectInput(inputId="depth_trt",
                              label="Choose a grouping variable:",
                              choices=c("Management group" = "label",
                                        "Land use" = "lu",
                                        "Agriculture vs non-agriculture" = "agriculture",
                                        "Perennial" = "perennial",
                                        "Tillage" = "till",
                                        "Cover crop use" = "cover_crops",
                                        "Crop" = "crop")
                  )
                ),
                navset_card_tab(
                  nav_panel("Depth plot",
                    plotlyOutput(outputId = "depth_plot"),
                    textOutput("depth_n")),
                  nav_panel("Summary data",
                  DT::dataTableOutput(outputId="depth_table")),
                  height=800
                )
              )
    )
  )
  )


# Define server logic ----
server <- function(input, output) {
  
  # Build data inputs ----
  
  # Reactive outputs to create density plot for surface depth only
  data_density_plot <- reactive({
    surf_long %>%
      filter(indicator %in% as.vector(input$density_indicator),
             project %in% as.vector(input$density_project)) 
  })
  
  # Indicator labels for density plots
  density_indicator_labs <- reactive({
    indicator_labs_df %>%
      filter(indicator %in% as.vector(input$density_indicator))
    })
  
  # Lookup table for selected project under density plot dropdown
  density_lookup <- reactive({
    project_annotate %>%
      ungroup() %>%
      filter(project %in% as.vector(input$density_project)) %>%
      distinct(project, project_label)
  })
  
  # Reactive outputs for pedon n below density plots
  data_density_n <- reactive({
    surf_long %>%
      filter(project %in% as.vector(input$density_project),
             indicator %in% as.vector(input$density_indicator)) %>%
      drop_na(value) %>%
      distinct(dsp_pedon_id, !!as.name(input$density_trt)) %>%
      count(!!as.name(input$density_trt)) %>%
      mutate(print = paste(!!as.name(input$density_trt), "- ", n, " pedons"))
  })
  
  # Treatment project selection
  data_trt <- reactive({
    surf_long %>%
      filter(project %in% as.vector(input$trt_project),
             indicator %in% as.vector(input$trt_indicator))
  })
  
  # Treatment labels
  labels_trt <- reactive({
    indicator_labs_df %>%
      filter(indicator %in% as.vector(input$trt_indicator))
  })
  
  # Comparison plot data
  data_compare <- reactive({
    surf %>%
      filter(project %in% as.vector(input$indicator_compare_project)) 
  })
  
  # Comparison plot labels
  compare_x_label <- reactive({
    indicator_labs_df %>%
      filter(indicator %in% as.vector(input$indicator_compare_x))
  })
  
  compare_y_label <- reactive({
    indicator_labs_df %>%
      filter(indicator %in% as.vector(input$indicator_compare_y))
  })
  
  # Reactive outputs to create labels for indicator depth plots
  depth_indicator_labs <- reactive({
    indicator_labs_df %>%
      filter(indicator %in% as.vector(input$depth_indicator))
  })
  
  # Lookup table for selected project under depth plot dropdown
  depth_lookup <- reactive({
    project_annotate %>%
      ungroup() %>%
      filter(project %in% as.vector(input$depth_project)) %>%
      distinct(project, project_label)
  })
  
  data_depth <- reactive({
    dsp4sh_dice_in %>%
      filter(project %in% as.vector(input$depth_project),
             indicator %in% as.vector(input$depth_indicator)) %>%
      group_by(project, indicator, indicator_label, units_only, 
               depth_cat, top, bottom, !!as.name(input$depth_trt)) %>%
      drop_na(value) %>%
      nest() %>%
      mutate(quantiles = purrr::map(data, ~hdquantile(.x$value, probs=c(.25, .5, .75))),
             quantiles2 = map(quantiles, ~bind_rows(.x) %>% rename(p.q25="0.25", p.q50="0.50", p.q75="0.75"))) %>%
      unnest(cols=quantiles2) 
  })
  
  data_depth_n <- reactive({
    dsp4sh_dice_in %>%
      filter(project %in% as.vector(input$depth_project),
             indicator %in% as.vector(input$depth_indicator)) %>%
      drop_na(value) %>%
      distinct(dsp_pedon_id, !!as.name(input$depth_trt)) %>%
      count(!!as.name(input$depth_trt)) %>%
      mutate(print = paste(!!as.name(input$depth_trt), "- ", n, " pedons"))
  })
  
  # Render plots and tables ----
  # Count of projects, soil suborders, and pedons
  output$treat_count <- renderText({
    paste("The DSP4SH database currently includes ", 
          print(treat_n$dsp_pedon_id), 
          "pedons representing ",
          print(treat_n$suborder),
          "soil suborders collected from ",
          print(treat_n$project),
          "projects.")
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
  
  output$density_plot <- renderPlotly({

      density_ggplot <- ggplot(data_density_plot(), 
                               aes(x=value, fill=!!as.name(input$density_trt))) +
        geom_density(alpha=0.75) +
        labs(title=paste(density_lookup()$project_label),
           x=paste(density_indicator_labs()$units_long), y="Density") +
        scale_fill_paletteer_d("PNWColors::Sunset2") +
        facet_wrap(~ suborder) +
        theme_classic() +
        theme(text = element_text(size = 14))
      
      ggplotly(density_ggplot)

  })
  
  output$density_n <- renderText({
    paste(print(data_density_n()$print), collapse = ", ")
  })
  
  output$depth_plot  <- renderPlotly({
    
    depth_ggplot <- ggplot(data_depth(),
                            aes(x=top, y=p.q50)) +
      geom_line(aes(color=!!as.name(input$depth_trt)), linewidth=2) +
      geom_ribbon(aes(ymin=p.q25, ymax=p.q75, x=top, fill=!!as.name(input$depth_trt)), alpha=0.4) +
      xlim(c(100,0)) +
      coord_flip() +
      labs(x="Depth (cm)", y=paste(depth_indicator_labs()$units_long),
           title=paste(depth_lookup()$project_label)) +
      scale_fill_paletteer_d("PNWColors::Sunset2") +
      scale_color_paletteer_d("PNWColors::Sunset2") +
      theme_classic() +
      theme(text = element_text(size = 14))
    
    ggplotly(depth_ggplot)
    
  })
  
  output$depth_n <- renderText({
    paste(print(data_depth_n()$print), collapse = ", ")
  })
  
  output$depth_table <- DT::renderDataTable({
    data_depth() %>%
      ungroup() %>%
      select(-top,-bottom,-data,-quantiles,-indicator) %>%
      arrange(project, !!as.name(input$depth_trt), depth_cat) %>%
      mutate(across(where(is.numeric), ~round(.x, 2))) %>%
      datatable(colnames = c('Project' = 'project',
                             "Indicator" = "indicator_label",
                             "Units" = "units_only",
                             "Soil Depth" = "depth_cat",
                             "1st Quartile" = "p.q25",
                             "Median" = "p.q50",
                             "3rd Quartile" = "p.q75"))
  })
  
  # Indicator comparison plot
  output$compare_plot <- renderPlotly({
    
    compare_ggplot <- ggplot(data_compare(),
           aes(x=.data[[input$indicator_compare_x]], 
               y=.data[[input$indicator_compare_y]])) +
      geom_point(aes(color=!!as.name(input$indicator_compare_trt),
                 shape=!!as.name(input$indicator_compare_group2))) +
      geom_smooth(method="lm", formula=y~x, color="gray10") +
      ggpubr::stat_cor(output.type="text", size=4, label.x.npc = 0.5, label.y.npc = 1,
               aes(label = paste(after_stat(rr.label)))) +
      ggpubr::stat_cor(output.type="text", size=4, label.x.npc = 0.5, label.y.npc=0.9,
                       aes(label = paste(after_stat(p.label)))) +
      scale_color_paletteer_d("PNWColors::Sunset2") +
      labs(x=paste(compare_x_label()$units_long), y=paste(compare_y_label()$units_long)) +
      theme_classic() +
      theme(text = element_text(size = 14))
    
    ggplotly(compare_ggplot)
    
  })
  
  # Compare by treatment
  output$trt_plot <- renderPlotly({
    
    trt_ggplot <- ggplot(data_trt(),
                         aes(x=.data[[input$trt_trt]],
                             y=value,
                             fill=.data[[input$trt_group2]])) +
      geom_boxplot() +
      scale_fill_paletteer_d("PNWColors::Sunset2") +
      labs(x=paste(input$trt_trt), y=paste(labels_trt()$units_long)) +
      theme_classic()
    
    ggplotly(trt_ggplot) %>%
      layout(boxmode = "group")
    
  })
  
  output$trt_summary <- DT::renderDataTable({
    
    data_trt() %>%
      group_by(!!as.name(input$trt_trt), !!as.name(input$trt_group2), indicator_label, units_only) %>%
      na.omit() %>%
      summarize(mean = mean(value, na.rm=TRUE),
                sd=sd(value, na.rm=TRUE),
                n=n()) %>%
      mutate(across(where(is.numeric), ~round(.x, 2))) %>%
      datatable(colnames = c("Indicator" = "indicator_label",
                             "Units" = "units_only",
                             "Mean" = "mean",
                             "Standard Deviation" = "sd"))
  })
      
}

# Run the application ----
shinyApp(ui, server)
