# Download external icon assets for app/tool UI ----

download_icon_assets <- function(force = FALSE) {
  assets <- tibble::tribble(
    ~asset_key           , ~asset_url                                                      , ~dest_rel_path                     ,
    "reports_soils_logo" , "https://wa-department-of-agriculture.github.io/soils/logo.png" , "www/icons/soils-logo.png"         ,
    "open_meteo_favicon" , "https://open-meteo.com/favicon.ico"                            , "www/icons/open-meteo-favicon.ico" ,
    "weatherlink_logo"   , "https://avatars.githubusercontent.com/u/50184229?s=200&v=4"    , "www/icons/weatherlink-logo.png"   ,
    "usda_nass_logo"     , "https://www.nass.usda.gov/images/masthead/nass_logo_bw-1.png"  , "www/icons/usda-nass-logo.png"
  )

  download_one <- function(asset_key, asset_url, dest_rel_path) {
    if (!isTRUE(force) && file.exists(dest_rel_path)) {
      return(tibble::tibble(
        asset_key = asset_key,
        asset_url = asset_url,
        dest_rel_path = dest_rel_path,
        status = "skipped_existing"
      ))
    }

    dir.create(dirname(dest_rel_path), recursive = TRUE, showWarnings = FALSE)

    req <- httr2::request(asset_url) |>
      httr2::req_timeout(30)

    resp <- tryCatch(
      httr2::req_perform(req),
      error = function(e) e
    )

    if (inherits(resp, "error")) {
      return(tibble::tibble(
        asset_key = asset_key,
        asset_url = asset_url,
        dest_rel_path = dest_rel_path,
        status = paste0("error: ", conditionMessage(resp))
      ))
    }

    raw_body <- tryCatch(
      httr2::resp_body_raw(resp),
      error = function(e) e
    )

    if (inherits(raw_body, "error")) {
      return(tibble::tibble(
        asset_key = asset_key,
        asset_url = asset_url,
        dest_rel_path = dest_rel_path,
        status = paste0("error: ", conditionMessage(raw_body))
      ))
    }

    writeBin(raw_body, dest_rel_path)

    tibble::tibble(
      asset_key = asset_key,
      asset_url = asset_url,
      dest_rel_path = dest_rel_path,
      status = "downloaded"
    )
  }

  purrr::pmap_dfr(assets, download_one)
}

download_icon_assets()
