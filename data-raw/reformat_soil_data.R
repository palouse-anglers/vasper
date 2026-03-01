# data-raw/reformat_soil_data.R
# Convert long-format soil_data3.RData → wide-format soil_data.csv
# compatible with the `soils` package (soils::washi_data schema).
#
# Source data is downloaded from GitHub at build time.

library(tidyverse)

# 1. Download & load source data ----
tmp <- tempfile(fileext = ".Rdata")
download.file(
  "https://github.com/palouse-anglers/soil-app/raw/refs/heads/main/data/soil_data3.RData",
  destfile = tmp
)
load(tmp)
unlink(tmp)
soil_data <- new_soil_data_long_coords
rm(new_soil_data_long_coords)

# 2. Parameter → column name mapping ----
# Keys match unique(soil_data$parameter); values follow soils conventions.
param_map <- c(
  # --- Matches to soils::data_dictionary ---
  "pH 1:1" = "ph",
  "OM (%)" = "om_percent",
  "CEC (meq/100g)" = "cec_meq_100g",
  "NO3N (ppm)" = "no3_n_mg_kg",
  "NH4N (ppm)" = "nh4_n_mg_kg",

  "Olsen P (ppm)" = "p_olsen_mg_kg",
  "Olsen K (ppm)" = "k_mg_kg",
  "Ec(1:1) (dS/m)" = "ec_mmhos_cm",
  "B (ppm)" = "b_mg_kg",
  "Cu (ppm)" = "cu_mg_kg",
  "Fe (ppm)" = "fe_mg_kg",
  "Mn (ppm)" = "mn_mg_kg",
  "Zn (ppm)" = "zn_mg_kg",
  "SO4S (ppm)" = "s_mg_kg",
  "Soil Texture" = "texture",
  "Exch Ca (meq/100g)" = "ca_mg_kg",
  "Exch Mg (meq/100g)" = "mg_mg_kg",
  "Exch Na (meq/100g)" = "na_mg_kg",

  # --- Extra parameters (no soils equivalent) ---
  "pH (A-E)" = "ph_ae",
  "Bray P(1:10) (ppm)" = "bray_p_mg_kg",
  "Exch K (meq/100g)" = "exch_k_meq_100g",
  "Est CEC (meq/100g)" = "est_cec_meq_100g",
  "Total Bases (meq/100g)" = "total_bases_meq_100g",
  "Cl (ppm)" = "cl_mg_kg",
  "Al (ppm)" = "al_mg_kg",
  "Al(KCl) (ppm)" = "al_kcl_mg_kg",
  "Ca (KCl) (ppm)" = "ca_kcl_mg_kg",
  "Mg (KCl) (ppm)" = "mg_kcl_mg_kg",
  "Na (KCl) (ppm)" = "na_kcl_mg_kg",
  "Density (g/ml)" = "density_g_ml",
  "Density 2 (mill-lbs/ac-depth)" = "density2_mlb_ac",
  "Efferves (Scale 0-7)" = "effervescence",
  "Est. SS (dS/m)" = "est_ss_ds_m",
  "NO3N# (lbs/ac-depth)" = "no3_n_lb_ac",
  "NH4N# (lbs/ac-depth)" = "nh4_n_lb_ac",
  "SO4S# (lbs/ac-depth)" = "s_lb_ac",
  "Wilting Point" = "wilting_point",
  "Field Capacity" = "field_capacity",
  "H2O wet/dry" = "h2o_wet_dry",
  "Avl H2O (inches)" = "avl_h2o_in",
  "Avl H2O % (%)" = "avl_h2o_percent"
)

# 3. Remap parameter names ----
soil_long <- soil_data |>
  filter(parameter %in% names(param_map)) |>
  mutate(column_name = param_map[parameter])

# texture results are all missing.
stopifnot(
  all(is.na(soil_long$result[soil_long$column_name == "texture"]))
)

# Separate texture (character) from numeric measurements
texture_wide <- soil_long |>
  filter(column_name == "texture") |>
  distinct(latitude, longitude, sample_date, depth, notes1, .keep_all = TRUE) |>
  select(latitude, longitude, sample_date, depth, notes1)

numeric_long <- soil_long |>
  filter(column_name != "texture") |>
  # Fill missing start/end depth from the depth string (e.g. "0-3" → 0, 3)
  mutate(
    start_depth_inches = coalesce(
      start_depth_inches,
      as.numeric(str_extract(depth, "^\\d+"))
    ),
    end_depth_inches = coalesce(
      end_depth_inches,
      as.numeric(str_extract(depth, "\\d+$"))
    )
  ) |>
  filter(!is.na(result)) |>
  select(where(~ any(!is.na(.x)) | any(as.character(.x) != "")))

# 4. Pivot to wide ----
# Unique sample key: latitude, longitude, sample_date, depth
soil_wide <- numeric_long |>
  select(
    year,
    sample_date,
    depth,
    start_depth_inches,
    end_depth_inches,
    latitude,
    longitude,
    grower,
    field_name,
    field_id,
    huc8_name,
    hc12_name,
    huc12,
    column_name,
    result
  ) |>
  pivot_wider(
    id_cols = c(
      year,
      sample_date,
      depth,
      start_depth_inches,
      end_depth_inches,
      latitude,
      longitude,
      grower,
      field_name,
      field_id,
      huc8_name,
      hc12_name,
      huc12
    ),
    names_from = column_name,
    values_from = result,
    values_fn = mean # resolve any duplicates by averaging
  )

# Include variables as missing if not already defined.
if (!"crop" %in% names(soil_wide)) {
  soil_wide$crop <- NA_character_
}

if (!"farm_name" %in% names(soil_wide)) {
  soil_wide$farm_name <- NA_character_
}

# 5. Derive IDs ----
# producer_id: from grower name
# field_id:    existing field_id
# sample_id:   producer + field + lat + long + date + depth
soil_wide <- soil_wide |>
  mutate(
    # IDs do not need to be numeric
    producer_id = grower
  ) |>
  mutate(
    sample_id = paste(
      producer_id,
      field_id,
      round(latitude, 5),
      round(longitude, 5),
      sample_date,
      depth,
      sep = "-"
    ),
    county = "Columbia"
  )

# 6. Select & order columns to match soils::washi_data layout ----
# Required metadata first, then measurement columns
metadata_cols <- c(
  "year",
  "sample_id",
  "farm_name",
  "producer_id",
  "field_name",
  "field_id",
  "county",
  "crop",
  "longitude",
  "latitude",
  "sample_date",
  "depth",
  "start_depth_inches",
  "end_depth_inches",
  "huc8_name",
  "hc12_name",
  "huc12"
)

# Measurement columns present in this data (ordered: physical, bio, chem, macro, micro, other)
measurement_cols <- intersect(
  c(
    # Physical
    "texture",
    "density_g_ml",
    "density2_mlb_ac",
    # Biological
    "om_percent",
    # Chemical
    "ph",
    "ph_ae",
    "ec_mmhos_cm",
    "est_ss_ds_m",
    "effervescence",
    "cec_meq_100g",
    "est_cec_meq_100g",
    "total_bases_meq_100g",
    # Macro nutrients
    "no3_n_mg_kg",
    "no3_n_lb_ac",
    "nh4_n_mg_kg",
    "nh4_n_lb_ac",
    "p_olsen_mg_kg",
    "bray_p_mg_kg",
    "k_mg_kg",
    "exch_k_meq_100g",
    "ca_mg_kg",
    "mg_mg_kg",
    "na_mg_kg",
    "s_mg_kg",
    "s_lb_ac",
    # Micro nutrients
    "b_mg_kg",
    "cl_mg_kg",
    "cu_mg_kg",
    "fe_mg_kg",
    "mn_mg_kg",
    "zn_mg_kg",
    "al_mg_kg",
    "al_kcl_mg_kg",
    # KCl extracts
    "ca_kcl_mg_kg",
    "mg_kcl_mg_kg",
    "na_kcl_mg_kg",
    # Moisture
    "wilting_point",
    "field_capacity",
    "h2o_wet_dry",
    "avl_h2o_in",
    "avl_h2o_percent"
  ),
  names(soil_wide)
)

soil_data_out <- soil_wide |>
  select(all_of(metadata_cols), all_of(measurement_cols))

# 7. Write soil_data.csv ----
write_csv(soil_data_out, "data/soil_data.csv", na = "")
message(
  "Wrote data/soil_data.csv: ",
  nrow(soil_data_out),
  " rows x ",
  ncol(soil_data_out),
  " cols"
)

# 8. Build data dictionary ----
# measurement_group assignment for every measurement column
group_map <- tribble(
  ~column_name           , ~measurement_group                , ~abbr                 , ~unit         ,
  "density_g_ml"         , "Physical"                        , "Density"             , "g/ml"        ,
  "density2_mlb_ac"      , "Physical"                        , "Density 2"           , "mill-lbs/ac" ,
  "om_percent"           , "Biological"                      , "OM"                  , "%"           ,
  "ph"                   , "Chemical"                        , "pH"                  , ""            ,
  "ph_ae"                , "Chemical"                        , "pH (A-E)"            , ""            ,
  "ec_mmhos_cm"          , "Chemical"                        , "EC"                  , "mmhos/cm"    ,
  "est_ss_ds_m"          , "Chemical"                        , "Est. SS"             , "dS/m"        ,
  "effervescence"        , "Chemical"                        , "Efferves."           , "0-7"         ,
  "cec_meq_100g"         , "Chemical"                        , "CEC"                 , "cmolc/kg"    ,
  "est_cec_meq_100g"     , "Chemical"                        , "Est CEC"             , "cmolc/kg"    ,
  "total_bases_meq_100g" , "Chemical"                        , "Total Bases"         , "cmolc/kg"    ,
  "no3_n_mg_kg"          , "Plant Essential Macro Nutrients" , "NO\u2083-N"          , "ppm"         ,
  "no3_n_lb_ac"          , "Plant Essential Macro Nutrients" , "NO\u2083-N mass"     , "lb/ac"       ,
  "nh4_n_mg_kg"          , "Plant Essential Macro Nutrients" , "NH\u2084-N"          , "ppm"         ,
  "nh4_n_lb_ac"          , "Plant Essential Macro Nutrients" , "NH\u2084-N mass"     , "lb/ac"       ,
  "p_olsen_mg_kg"        , "Plant Essential Macro Nutrients" , "P (Olsen)"           , "ppm"         ,
  "bray_p_mg_kg"         , "Plant Essential Macro Nutrients" , "P (Bray)"            , "ppm"         ,
  "k_mg_kg"              , "Plant Essential Macro Nutrients" , "K"                   , "ppm"         ,
  "exch_k_meq_100g"      , "Plant Essential Macro Nutrients" , "Exch K"              , "cmolc/kg"    ,
  "ca_mg_kg"             , "Plant Essential Macro Nutrients" , "Ca"                  , "ppm"         ,
  "mg_mg_kg"             , "Plant Essential Macro Nutrients" , "Mg"                  , "ppm"         ,
  "na_mg_kg"             , "Plant Essential Macro Nutrients" , "Na"                  , "ppm"         ,
  "s_mg_kg"              , "Plant Essential Macro Nutrients" , "S"                   , "ppm"         ,
  "s_lb_ac"              , "Plant Essential Macro Nutrients" , "S mass"              , "lb/ac"       ,
  "b_mg_kg"              , "Plant Essential Micro Nutrients" , "B"                   , "ppm"         ,
  "cl_mg_kg"             , "Plant Essential Micro Nutrients" , "Cl"                  , "ppm"         ,
  "cu_mg_kg"             , "Plant Essential Micro Nutrients" , "Cu"                  , "ppm"         ,
  "fe_mg_kg"             , "Plant Essential Micro Nutrients" , "Fe"                  , "ppm"         ,
  "mn_mg_kg"             , "Plant Essential Micro Nutrients" , "Mn"                  , "ppm"         ,
  "zn_mg_kg"             , "Plant Essential Micro Nutrients" , "Zn"                  , "ppm"         ,
  "al_mg_kg"             , "Plant Essential Micro Nutrients" , "Al"                  , "ppm"         ,
  "al_kcl_mg_kg"         , "Plant Essential Micro Nutrients" , "Al (KCl)"            , "ppm"         ,
  "ca_kcl_mg_kg"         , "Plant Essential Macro Nutrients" , "Ca (KCl)"            , "ppm"         ,
  "mg_kcl_mg_kg"         , "Plant Essential Macro Nutrients" , "Mg (KCl)"            , "ppm"         ,
  "na_kcl_mg_kg"         , "Plant Essential Macro Nutrients" , "Na (KCl)"            , "ppm"         ,
  "wilting_point"        , "Physical"                        , "Wilting Pt"          , ""            ,
  "field_capacity"       , "Physical"                        , "Field Cap"           , ""            ,
  "h2o_wet_dry"          , "Physical"                        , "H\u2082O w/d"        , ""            ,
  "avl_h2o_in"           , "Physical"                        , "Avl H\u2082O length" , "in"          ,
  "avl_h2o_percent"      , "Physical"                        , "Avl H\u2082O"        , "%"
)

# Keep only columns that actually appear in the output data
data_dictionary <- group_map |>
  filter(column_name %in% measurement_cols) |>
  mutate(
    order = row_number(),
    abbr_unit = if_else(unit == "", abbr, paste0(abbr, " (", unit, ")"))
  ) |>
  select(measurement_group, column_name, order, abbr, unit, abbr_unit)

write_csv(data_dictionary, "data/data_dictionary.csv", na = "")
message("Wrote data/data_dictionary.csv: ", nrow(data_dictionary), " rows")

message("Done.")
