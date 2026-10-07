# Discover & test the maadialna.ma "get-bassin" API
# Endpoint returns a JSON array: one element per river basin (bassin hydraulique)

library(httr2)
library(jsonlite)
library(xml2)
library(rvest)
library(dplyr)
library(purrr)
library(stringr)

url <- "https://maadialna.ma/ar/get-bassin"

# ---- 1. Raw request: status, headers, content type ---------------------------
resp <- request(url) |>
  req_user_agent("Mozilla/5.0") |>
  req_timeout(30) |>
  req_perform()

cat("Status      :", resp_status(resp), "\n")
cat("Content-Type:", resp_content_type(resp), "\n")
cat("Last-Modified:", resp_header(resp, "Last-Modified"), "\n")
cat("Size (bytes):", length(resp_body_raw(resp)), "\n\n")

raw <- resp_body_json(resp, simplifyVector = FALSE)


# ---- 2. Parse JSON (keep strings, convert numbers later) -----------------------
bassins <- resp |>
  resp_body_string() |>
  fromJSON(simplifyDataFrame = TRUE)

# ---- 3. Basin-level table ------------------------------------------------------
basins_df <- bassins |>
  transmute(
    bassin        = str_squish(title),
    region_id     = field_id_region_hydraulique,
    volume_mm3    = as.numeric(field_volume),
    taux_remplissage = as.numeric(field_taux_de_remplissage),
    trend         = as.integer(field_indicateu_up_down)
  )

# ---- 4. Parse the modal HTML into one row per dam -----------------------------
parse_modal <- function(html, bassin) {
  page <- read_html(html, encoding = "UTF-8")

  # Each dam = a <p><strong>name</strong></p> followed by a <ul> with 3 <li>
  uls <- html_elements(page, "ul.list-style-modal")
  if (length(uls) == 0) return(NULL)

  map_dfr(uls, function(ul) {
    # the dam name is in the closest preceding <p> that contains <strong>
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

dams_df <- map2_dfr(
  bassins$field_modal_content_barrage,
  str_squish(bassins$title),
  parse_modal
)

print(dams_df, n = Inf)
