# src/utils/district_name_helpers.R
# Shared district name cleaning/abbreviation helper for static maps and web export.

clean_single_name <- function(names) {
  names |>
    stringr::str_replace_all("(?i)\\s+in\\s+(the\\s+)?(count(y|ies)|cit(y|ies)).*$", "") |>
    stringr::str_replace_all("(?i)\\s+and\\s+state(\\s+of.*)?$", "") |>
    stringr::str_to_title() |>
    stringr::str_replace_all("(?i)\\b(No|Number)\\b\\.?\\s*:?\\s*\\d*", "") |>
    stringr::str_replace_all("#\\s*\\d*", "") |>
    stringr::str_replace_all("(?i)\\b(Re|Rj|Jt|R|C|J)[\\s\\-_/]*\\d+[a-z]?\\b", "") |>
    stringr::str_replace_all("(?i)\\b\\d+[a-z]\\b", "") |>
    stringr::str_replace_all("\\b\\d{1,4}\\b", "") |>
    stringr::str_replace_all(
      "(?i)\\b(Public|Community|Consolidated|Local|Joint|Jt|Unified|Union|Elementary|High|Secondary|Exempted Village|Special|Independent|Charter)\\b",
      ""
    ) |>
    stringr::str_replace_all(
      "(?i)\\b(School\\s+District|District|Dist|Schools?|Sch\\s+Dist|SD|CUSD|USD|ISD|CSD|SSD)\\b",
      ""
    ) |>
    stringr::str_replace_all("(?i)\\b(County|Counties|City|Co|Borough|Municipal)\\b", "") |>
    stringr::str_replace_all("[\\.\\,\\(\\)\\[\\]\\-_/\\:\\;\\#]", " ") |>
    stringr::str_replace_all("(?i)\\b(Re|Rj|Jt|No|J|R|C)\\b", "") |>
    stringr::str_replace_all("(?i)\\b(In\\s+The\\s+Of|In\\s+The|Of\\s+The|In|Of|The|And|At|For)\\b", "") |>
    stringr::str_replace_all("\\b(Ii|Iii|Iv|Vi|Vii|Viii)\\b", \(m) stringr::str_to_upper(m)) |>
    stringr::str_squish() |>
    stringr::str_remove("^[\\.\\,\\-\\s]+") |>
    stringr::str_remove("[\\.\\,\\-\\s]+$")
}

clean_district_name <- function(lea_name, district_name = lea_name) {
  c_lea  <- clean_single_name(dplyr::coalesce(lea_name, ""))
  c_dist <- clean_single_name(dplyr::coalesce(district_name, ""))
  
  # 1. Use cleaned lea_name if non-empty; otherwise fall back to cleaned TIGER district_name
  res <- dplyr::if_else(c_lea != "", c_lea, c_dist)
  
  # 2. For purely numbered districts (e.g., "District 49", "School District 27J", or
  #    "School District No. 1 in the County of Denver"), extract place name or compact number
  raw_fallback <- dplyr::coalesce(district_name, lea_name, "")
  place_match  <- stringr::str_match(
    raw_fallback,
    "(?i)\\b(?:count(?:y|ies)|cit(?:y|ies))\\s+of\\s+([a-z\\s\\-]+?)(?:\\s+and\\s+state|\\s*$)"
  )[, 2] |>
    stringr::str_squish() |>
    stringr::str_to_title()
  
  compact_fallback <- raw_fallback |>
    stringr::str_replace_all("(?i)\\s+in\\s+(the\\s+)?(count(y|ies)|cit(y|ies)).*$", "") |>
    stringr::str_replace_all("(?i)\\bSchool\\s+District\\b", "Dist.") |>
    stringr::str_replace_all("(?i)\\bDistrict\\b", "Dist.") |>
    stringr::str_replace_all("(?i)\\bNo\\.?\\s*", "") |>
    stringr::str_squish() |>
    stringr::str_to_title()
  
  dplyr::case_when(
    res != "" ~ res,
    !is.na(place_match) & place_match != "" ~ place_match,
    compact_fallback != "" ~ compact_fallback,
    TRUE ~ stringr::word(raw_fallback, 1)
  )
}