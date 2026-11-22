# src/utils/district_name_helpers.R
# Shared district name cleaning/abbreviation helper for static maps and web export.

clean_district_name <- function(names) {
  cleaned <- names |>
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

  dplyr::if_else(cleaned == "" | is.na(cleaned), stringr::word(names, 1), cleaned)
}