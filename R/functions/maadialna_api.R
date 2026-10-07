# maadialna.ma dam levels
# The "get-bassin" endpoint returns one element per river basin; each one embeds an HTML modal
# listing its dams (fill rate today, fill rate a year ago, volume).
# Dam names are in Arabic, so dams are matched to our GeoJSON by capacity (see eau_match_by_capacity).

library(httr2)
library(jsonlite)
library(xml2)
library(rvest)
library(purrr)
library(stringr)
library(tibble)

maadialna_url <- "https://maadialna.ma/ar/get-bassin"
maadialna_cache <- "data/sidattes/barrages_maroc/maadialna_cache.rds"

# One row per dam: bassin, barrage, taux_today, taux_last_year, volume_mm3
parse_maadialna_modal <- function(html, bassin) {
  page <- read_html(html, encoding = "UTF-8")

  # Each dam = a <p><strong>name</strong></p> followed by a <ul> with 3 <li>
  uls <- html_elements(page, "ul.list-style-modal")
  if (length(uls) == 0) return(NULL)

  map_dfr(uls, function(ul) {
    name <- xml_find_first(ul, "preceding-sibling::p[.//strong][1]") |>
      html_text2() |>
      str_squish()

    items <- ul |> html_elements("li") |> html_text2() |> str_squish()

    tibble(
      bassin = bassin,
      barrage = name,
      taux_today = str_extract(items[1], "[0-9.]+(?=%)") |> as.numeric(),
      taux_last_year = str_extract(items[2], "[0-9.]+(?=%)") |> as.numeric(),  # NA when "--%"
      volume_mm3 = str_extract(items[3], "[0-9.]+") |> as.numeric()
    )
  })
}

fetch_maadialna_dams <- function() {
  bassins <- request(maadialna_url) |>
    req_user_agent("Mozilla/5.0") |>
    req_timeout(15) |>
    req_perform() |>
    resp_body_string() |>
    fromJSON(simplifyDataFrame = TRUE)

  map2_dfr(
    bassins$field_modal_content_barrage,
    str_squish(bassins$title),
    parse_maadialna_modal
  )
}

# Live API first (and refresh the cache); if it fails, fall back on the last cached response.
# Returns list(dams, fetched_at, live) or NULL when there is neither.
load_maadialna_dams <- function() {
  tryCatch({
    res <- list(dams = fetch_maadialna_dams(), fetched_at = Sys.time(), live = TRUE)
    saveRDS(res, maadialna_cache)
    res
  }, error = function(e) {
    message("maadialna API unavailable (", conditionMessage(e), ")")
    if (!file.exists(maadialna_cache)) return(NULL)
    res <- readRDS(maadialna_cache)
    res$live <- FALSE
    res
  })
}

# Match our dams to the API dams using the capacity.
# The API's volume_mm3 is the dam capacity in Mm³, like capacite_i in our GeoJSON; each of our
# dams takes the closest API capacity (within `tol`), and one API dam is never used twice.
# `targets` needs OBJECTID and capacite_i; returns OBJECTID, api_name, bassin, taux_today, taux_last_year.
eau_match_by_capacity <- function(api, targets, tol = 0.02) {
  # Relative gap between every dam capacity and every API capacity
  gap <- outer(targets$capacite_i, api$volume_mm3, function(a, b) abs(a - b) / a)
  gap[is.na(gap)] <- Inf

  out <- list()
  repeat {
    best <- which(gap == min(gap), arr.ind = TRUE)[1, ]
    if (!is.finite(gap[best[1], best[2]]) || gap[best[1], best[2]] > tol) break
    out[[length(out) + 1]] <- data.frame(
      OBJECTID = targets$OBJECTID[best[1]],
      api_name = api$barrage[best[2]],
      bassin = api$bassin[best[2]],
      taux_today = api$taux_today[best[2]],
      taux_last_year = api$taux_last_year[best[2]],
      stringsAsFactors = FALSE
    )
    gap[best[1], ] <- Inf
    gap[, best[2]] <- Inf
  }
  if (length(out) == 0) {
    return(data.frame(OBJECTID = integer(), api_name = character(), bassin = character(),
                      taux_today = numeric(), taux_last_year = numeric()))
  }
  do.call(rbind, out)
}
