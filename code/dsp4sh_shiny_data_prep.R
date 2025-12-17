# Prep data needed for DSP4SH Shiny app

# Load packages ----
library(here)
library(janitor)
library(aqp)
library(soilDB)
library(readxl)
library(DBI)
library(RSQLite)
library(sf)
library(terra)
library(tidyverse)

# Load data ----
dsp4sh6 <- dbConnect(SQLite(), here("data", "dsp4sh6.db"))
treatment <- read_xlsx(here("data", "DSP4SH Standardized Treatments.xlsx"))

# Make label vectors ----

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
                    "bglucosaminidase" = "NAG activity",
                    "ace" = "ACE protein",
                    "bglucosidase" = "BG activity",
                    "kssl_wsa" = "Aggregate stability",
                    "yoder_agg_stab_mwd" = "Aggregate MWD",
                    "arylsulfatase" = "AS activity",
                    "pox_c" = "POX-C",
                    "tn_pct" = "Total N",
                    "bulk_density" = "Bulk density",
                    "soil_respiration" = "Soil respiration",
                    "alkaline_phosphatase" = "AlkP activity",
                    "acid_phosphatase" = "AcidP activity",
                    "ph" = "pH - cooperator lab",
                    "sand_pct" = "Sand content - cooperator lab",
                    "silt_pct" = "Silt content - cooperator lab",
                    "clay_pct" = "Clay content - cooperator lab",
                    "ph_kssl" = "pH - KSSL",
                    "sand_kssl" = "Sand content - KSSL",
                    "silt_kssl" = "Silt content - KSSL",
                    "clay_kssl" = "Clay content - KSSL")

# Also make into dataframe with units (need this to use labels within map functions)
indicator_labs_df <- data.frame(indicator_labs) %>%
  rownames_to_column() %>%
  rename(indicator = rowname,
         indicator_label = indicator_labs) %>%
  mutate(units_long = case_when(indicator=="soc_pct" ~ "SOC (%)",
                                indicator=="tn_pct" ~ "Total nitrogen (%)",
                                indicator=="bglucosaminidase" ~ "NAG activity (mg kg-1 hr-1)",
                                indicator=="ace" ~ "ACE protein (g kg-1)",
                                indicator=="bglucosidase" ~ "BG activity (mg kg-1 hr-1)",
                                indicator=="kssl_wsa" ~ "Aggregate stability (%)",
                                indicator=="yoder_agg_stab_mwd" ~ "Aggregate MWD (mm)",
                                indicator=="arylsulfatase" ~ "AS activity (mg kg-1 hr-1)",
                                indicator=="pox_c" ~ "POX-C (mg kg-1)",
                                indicator=="bulk_density" ~ "Bulk density (g cm-3)",
                                indicator=="soil_respiration" ~ "Soil respiration (mg CO2 per 4 days)",
                                indicator=="alkaline_phosphatase" ~ "AlkP activity (mg kg-1 hr-1)",
                                indicator=="acid_phosphatase" ~ "AcidP activity (mg kg-1 hr-1)",
                                indicator=="ph" ~ "pH - cooperator lab",
                                indicator=="sand_pct" ~ "Sand content - cooperator lab (%)",
                                indicator=="silt_pct" ~ "Silt content - cooperator lab (%)",
                                indicator=="clay_pct" ~ "Clay content - cooperator lab (%)",
                                indicator=="ph_kssl" ~ "pH - KSSL",
                                indicator=="sand_kssl" ~ "Sand content - KSSL (%)",
                                indicator=="silt_kssl" ~ "Silt content - KSSL (%)",
                                indicator=="clay_kssl" ~ "Clay content - KSSL (%)"),
         units_only = case_when(indicator=="soc_pct" ~ "%",
                                indicator=="tn_pct" ~ "%",
                                indicator=="bglucosaminidase" ~ "mg kg-1 hr-1",
                                indicator=="ace" ~ "g kg-1",
                                indicator=="bglucosidase" ~ "mg kg-1 hr-1",
                                indicator=="kssl_wsa" ~ "%",
                                indicator=="yoder_agg_stab_mwd" ~ "mm",
                                indicator=="arylsulfatase" ~ "mg kg-1 hr-1",
                                indicator=="pox_c" ~ "mg kg-1",
                                indicator=="bulk_density" ~ "g cm-3",
                                indicator=="soil_respiration" ~ "mg CO2 per 4 days",
                                indicator=="alkaline_phosphatase" ~ "mg kg-1 hr-1",
                                indicator=="acid_phosphatase" ~ "mg kg-1 hr-1",
                                indicator=="ph" ~ "pH",
                                indicator=="sand_pct" ~ "%",
                                indicator=="silt_pct" ~ "%",
                                indicator=="clay_pct" ~ "%",
                                indicator=="ph_kssl" ~ "pH",
                                indicator=="sand_kssl" ~ "%",
                                indicator=="silt_kssl" ~ "%",
                                indicator=="clay_kssl" ~ "%"))

# Join database tables ----
# Convert tables into dataframes
db <- lapply(setNames(nm = dbListTables(dsp4sh6)), dbReadTable, conn = dsp4sh6)

dspplotmgt <- db$dspplotmgt %>% 
  clean_names() # contains information about the sampling plots and treatments
kssl_lab <- db$kssllabmst %>% 
  clean_names() # lab data analyzed at KSSL
coop_lab <- db$cooplabmst %>% 
  clean_names() # lab data analyzed at cooperator labs, this is the data we will focus on
pedon <- db$pedon %>% 
  clean_names() # information about the each pedon sampled (location, date, etc)
layer_descr <- db$layerdescription %>% 
  clean_names() # field data for each profile sampled 
layer_desig <- db$layerdesignation %>% 
  clean_names() # designation of the layers in each soil profile sampled - this is where depths and horizon names come from
project_design <- db$projectdesign %>%
  clean_names() # information about the overall project design for individual cooperator projects

dbDisconnect(conn = dsp4sh6)

# join data
layer_join <- coop_lab %>%
  dplyr::select(-cooplabmst_id, -dsp_pedon, -water_content, -weon, -weoc, -yoder_agg_stab_total_ws, -yoder_ws_2_8,
                -yoder_ws_pt5_2, -yoder_ws_250_500, -yoder_ws_53_250, -ec, -effervescence, -plfa_id) %>%
  left_join(layer_desig, by=c("dsp_sample_id", "dsp_pedon_id", "layer_no")) %>%
  left_join(select(layer_descr, dsp_sample_id, coarse_frag_volume), 
            by=c("dsp_sample_id")) %>%
  left_join(select(pedon, dsp_pedon_id, dsp_plot_id, pedon_x, pedon_y), by="dsp_pedon_id") %>%
  left_join(select(dspplotmgt, dsp_plot_id, label, lu, till, trt, explanation, project, soil), by="dsp_plot_id") %>%
  left_join(select(kssl_lab, natural_key, clay_tot_psa, silt_tot_psa, sand_tot_psa, tex_psda, ph_h2o),
            by=c("kssl_labsampnum" = "natural_key")) %>%
  mutate(label = case_when(project=="UConn" & lu =="FOREST" ~ "Ref",
                           .default=label), # correct one plot in UConn data where forest treatment is incorrectly labeled as SHM, should be Ref
         label=factor(label, levels=c("BAU", "SHM", "Ref"))) 

# Make treatment dataframe ----
# Filter big join df
project <- layer_join %>%
  select(project, dsp_pedon_id, dsp_plot_id, soil, label, lu, till, trt, pedon_x, pedon_y) %>%
  distinct()

# Pull soil taxonomy from OSD
soils <- project %>%
  distinct(soil) %>%
  pull()
osd <- fetchOSD(soils)
taxonomy <- site(osd) %>%
  select(id, suborder, subgroup) %>%
  mutate(id = str_to_title(id),
         suborder = str_to_title(suborder),
         subgroup = str_to_title(subgroup)) 

project_soil <- project %>%
  left_join(taxonomy, by=c("soil" = "id")) %>%
  relocate(suborder, subgroup, .after=soil)

# Pull in PRISM data
mat <- rast(here("PRISM_tmean_30yr_normal_800mM5_annual_bil", 
                        "PRISM_tmean_30yr_normal_800mM5_annual_bil.bil"))
ppt <- rast(here("PRISM_ppt_30yr_normal_800mM4_annual_bil", 
                       "PRISM_ppt_30yr_normal_800mM4_annual_bil.bil"))
prism_stack <- rast(list(mat, ppt))

# make pedon location points into sf
pedon_pts <- project %>%
  st_as_sf(coords = c("pedon_x", "pedon_y"), crs=4326)

# extract prism data
prism_extract <- terra::extract(prism_stack, pedon_pts)

project_prism <- project %>%
  bind_cols(dplyr::select(prism_extract, -ID)) %>%
  rename(mat = PRISM_tmean_30yr_normal_800mM5_annual_bil,
         ppt = PRISM_ppt_30yr_normal_800mM4_annual_bil) %>%
  mutate(mat = round(mat, 1),
         ppt = round(ppt, 0)) %>%
  group_by(project) %>%
  mutate(avg_mat = round(mean(mat), 1),
         avg_ppt = round(mean(ppt), 0))

# Join in treatment description from publication, add project labels
project_soil_prism_trt <- project_soil %>%
  left_join(select(treatment, dsp_pedon_id, description), by="dsp_pedon_id") %>%
  left_join(select(project_prism, dsp_pedon_id, project, avg_mat, avg_ppt), by=c("project", "dsp_pedon_id")) %>%
  mutate(project_label = str_replace_all(project, project_labels))

# Make condensed treatment table with count of pedons in each management condition, add nice labels for project
treatment_table <- project_soil_prism_trt %>%
  group_by(project, project_label, soil, subgroup, avg_mat, avg_ppt, label, description) %>%
  dplyr::summarize(pedon_n = n()) %>%
  ungroup()

# Count projects, soil suborders, and pedons for text
trt_n_total <- project_soil_prism_trt %>%
  dplyr::summarize(across(c("project", "suborder", "dsp_pedon_id"), n_distinct))

# Make dataframe of soil horizons ----
horizon_shiny <- layer_join %>%
  select(project, dsp_sample_id, dsp_pedon_id, dsp_plot_id, pedon_x, pedon_y, soil,
         label, trt, lu, till, 
         layer_no, hzdesg, hrzdep_t, hrzdep_b, 
         soc_pct, bulk_density, tn_pct, kssl_wsa, 
         yoder_agg_stab_mwd, p_h:ace, coarse_frag_volume, 
         sand_pct, silt_pct, clay_pct, texture, 
         sand_tot_psa, silt_tot_psa, clay_tot_psa, tex_psda, ph_h2o) %>%
  rename_with(~str_replace(., "tot_psa", "kssl")) %>%
  rename(ph = p_h,
         ph_kssl = ph_h2o,
         texture_kssl = tex_psda) %>%
  left_join(select(treatment, dsp_pedon_id, agriculture, perennial, cover_crops, crop), by="dsp_pedon_id") %>%
  left_join(taxonomy, by=c("soil" = "id")) %>%
  relocate(suborder, subgroup, .after=soil) %>% 
  arrange(project, label)

# Calculate indicator values in 0-10 cm soils ----
surf_shiny <- horizon_shiny %>%
  filter(hrzdep_b == "5" | hrzdep_b=="10") %>%
  group_by(dsp_pedon_id) %>%
  dplyr::summarize(across(c(soc_pct:ace, sand_pct:clay_pct, sand_kssl:clay_kssl, ph_kssl), 
                          ~round(mean(.x, na.rm=TRUE), 2))) %>%
  mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .))) %>%
  left_join(project_soil_prism_trt, by="dsp_pedon_id") %>%
  left_join(select(treatment, dsp_pedon_id, agriculture, perennial, cover_crops, crop), by="dsp_pedon_id") %>%
  relocate(project:crop, .after=dsp_pedon_id) %>% 
  arrange(project_label, label)

# Make a version of surface indicator data that is pivoted longer
surf_shiny_long <- surf_shiny %>%
  pivot_longer(soc_pct:ph_kssl, names_to="indicator", values_to="value") %>%
  left_join(indicator_labs_df, by="indicator") %>% # add indicator labels and units
  arrange(project_label, label)

# Site location table for mapping ----
# Make dataframe for adding project data annotation to map, with just one point for each project label combination, text label describing project

map_data <- project_soil_prism_trt %>% 
  unite("management", c("label", "description"), sep=" - ", remove=FALSE) %>%
  group_by(project) %>%
  mutate(avg_lat = mean(pedon_y, na.rm=TRUE),
         avg_long = mean(pedon_x, na.rm=TRUE)) %>%
  ungroup() %>%
  group_by(project, label) %>%
  mutate(pedon_n = n()) %>%
  select(project, project_label, label, avg_lat, avg_long, management, pedon_n) %>%
  distinct() %>%
  pivot_wider(names_from=label, values_from=c(management, pedon_n))

# Depth plot dataframe ----
# Promote horizon data to spc
dsp4sh_spc <- horizon_shiny 
depths(dsp4sh_spc) <- dsp_pedon_id ~ hrzdep_t + hrzdep_b
hzdesgnname(dsp4sh_spc) <- 'hzdesg'

# Dice profile into 1 cm increments
dsp4sh_dice_spc <- dice(dsp4sh_spc, fm = ~ bulk_density + kssl_wsa + 
                          yoder_agg_stab_mwd + soc_pct + tn_pct + ace + pox_c + soil_respiration +
                          bglucosidase + bglucosaminidase + acid_phosphatase + alkaline_phosphatase + arylsulfatase
                        + ph + sand_pct + silt_pct + clay_pct + ph_kssl + sand_kssl + silt_kssl + clay_kssl,
                        byhz=FALSE) 

# prep data frame for calculating quantiles in shiny app
dsp4sh_dice_in <- horizons(dsp4sh_dice_spc) %>%
  left_join(select(treatment, dsp_pedon_id, project, lu, label, trt, 
                   till, agriculture, perennial, cover_crops, crop), by="dsp_pedon_id") %>%
  left_join(select(project_soil_prism_trt, dsp_pedon_id, project_label, soil, suborder, subgroup), by="dsp_pedon_id") %>%
  filter(hrzdep_t < 100) %>%
  mutate(depth_cat = case_when(hrzdep_t <= 10 ~ "0-10 cm",
                               hrzdep_t >10 & hrzdep_t <= 20 ~ "10-20 cm",
                               hrzdep_t >20 & hrzdep_t <= 30 ~ "20-30 cm",
                               hrzdep_t >30 & hrzdep_t <= 40 ~ "30-40 cm",
                               hrzdep_t >40 & hrzdep_t <= 50 ~ "40-50 cm",
                               hrzdep_t >50 & hrzdep_t <= 60 ~ "50-60 cm",
                               hrzdep_t >60 & hrzdep_t <= 70 ~ "60-70 cm",
                               hrzdep_t >70 & hrzdep_t <= 80 ~ "70-80 cm",
                               hrzdep_t >80 & hrzdep_t <= 90 ~ "80-90 cm",
                               hrzdep_t >90 & hrzdep_t <= 100 ~ "90-100 cm")) %>%
  mutate(top = str_extract(depth_cat, "\\d{1,}(?=-)") %>% as.integer(),
         bottom = str_extract(depth_cat, "(?<=-)\\d{2,}") %>% as.integer()) %>%
  pivot_longer(bulk_density:clay_kssl, names_to="indicator", values_to="value") %>%
  left_join(indicator_labs_df, by="indicator") %>%
  mutate(label=factor(label, levels=c("BAU", "SHM", "Ref"))) %>%
  relocate(project:bottom, .after=dsp_pedon_id)

# SHAPE Scores ----
# Load scoring script
source(here("code", "shapecurve_function.R"))

# Make purrr-safe 'possibly' version that will run for all samples that can be run, and skip invalid samples
poss_shape_latlon <- possibly(.f=shape_latlon, otherwise=NA)

# Wrangle input data
shape_in <- surf_shiny_long %>%
  filter(indicator=="soc_pct" | indicator=="pox_c" | indicator=="soil_respiration" | indicator=="ace") %>%
  group_by(dsp_pedon_id, dsp_plot_id, project, suborder, label, lu, agriculture, perennial, till, cover_crops, crop,
           units_long, indicator_label) %>%
  nest()

# Run scoring algorithm
shape_nest <- shape_in %>%
  mutate(shape_results = purrr::map(data, 
                                    ~poss_shape_latlon(lat=.x$pedon_y,
                                                       long = .x$pedon_x,
                                                       model_type =case_when(.x$indicator=="soc_pct" ~ "SOC",
                                                                             .x$indicator=="pox_c" ~ "AC",
                                                                             .x$indicator=="soil_respiration" ~ "RESP",
                                                                             .x$indicator=="ace" ~ "ACE"),
                                                       covariate = .x$value)))

shape_df <- shape_nest %>%
  mutate(score = purrr::map(shape_results, ~pluck(.x, 1)),
         posterior = purrr::map(shape_results, ~pluck(.x, 2)),
         scalar = purrr::map(shape_results, ~pluck(.x, 3)),
         model_type = purrr::map(shape_results, ~pluck(.x, 4)),
         indicator = purrr::map(data, ~pluck(.x, "indicator")))

shape_curve_df <- shape_df %>%
  select(-data, -shape_results, -score) %>%
  unnest(c(posterior, scalar, model_type))

shape_score_df <- shape_df %>%
  select(-data, -shape_results, -posterior, -scalar, -model_type) %>%
  unnest(cols=score)

# Save all data ----
saveRDS(indicator_labs_df, "indicator_labs_df.rds")
saveRDS(treatment_table, "treatment_table.rds")
saveRDS(trt_n_total, "trt_n_total.rds")
saveRDS(horizon_shiny, "horizon_shiny.rds")
saveRDS(surf_shiny, "surf_shiny.rds")
saveRDS(surf_shiny_long, "surf_shiny_long.rds")
saveRDS(map_data, "map_data.rds")
saveRDS(dsp4sh_dice_in, "dsp4sh_dice_in.rds")
saveRDS(shape_curve_df, "shape_curve_df.rds")
saveRDS(shape_score_df, "shape_score_df.rds")
