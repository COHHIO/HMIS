
#' @title Living Situation Reference Number Translation`r lifecycle::badge("deprecated")`
#' @description Deprecated in favor of `hud_translations`.  Return a human-readable living situation character vector provided with an integer reference number
#' @param ReferenceNo \code{(integer)} Reference number for living situation type
#' @return \code{(character)} Human-readable living situation type
#' @export

living_situation <- function(ReferenceNo) {
  dplyr::case_when(
    ReferenceNo == 1 ~ "Emergency shelter/ h/motel paid for by a third party/Host Home shelter",
    ReferenceNo == 2 ~ "Transitional housing",
    ReferenceNo == 3 ~ "Permanent housing (other than RRH) for formerly homeless persons",
    ReferenceNo == 4 ~ "Psychiatric hospital/ other psychiatric facility",
    ReferenceNo == 5 ~ "Substance abuse treatment facility or detox center",
    ReferenceNo == 6 ~ "Hospital or other residential non-psychiatric medical facility",
    ReferenceNo == 7 ~ "Jail/prison/juvenile detention",
    ReferenceNo == 8 ~ "Client doesn't know",
    ReferenceNo == 9 ~ "Client refused",
    ReferenceNo == 32 ~ "Host Home (non-crisis)",
    ReferenceNo == 13 ~ "Staying or living with friends, temporary tenure",
    ReferenceNo == 36 ~ "Staying or living in a friend's room, apartment or house",
    ReferenceNo == 18 ~ "Safe Haven",
    ReferenceNo == 15 ~ "Foster care home of foster care group home",
    ReferenceNo == 12 ~ "Staying or living with family, temporary tenure",
    ReferenceNo == 25 ~ "Long-term care facility or nursing home",
    ReferenceNo == 22 ~ "Staying or living with family, permanent tenure",
    ReferenceNo == 35 ~ "Staying or living in a family member's room, apartment, or house",
    ReferenceNo == 16 ~ "Place not meant for habitation",
    ReferenceNo == 23 ~ "Staying or living with friends, permanent tenure",
    ReferenceNo == 29 ~ "Residential project or halfway house with no homeless criteria",
    ReferenceNo == 14 ~ "H/Motel paid for by household",
    ReferenceNo == 26 ~ "Moved from one HOPWA funded project to HOPWA PH",
    ReferenceNo == 27 ~ "Moved from HOPWA funded project to HOPWA TH",
    ReferenceNo == 28 ~ "Rental by client, with GPD TIP housing subsidy",
    ReferenceNo == 19 ~ "Rental by client, with VASH housing subsidy",
    ReferenceNo == 31 ~ "Rental by client, with RRH or equivalent subsidy",
    ReferenceNo == 33 ~ "Rental by client, with HCV voucher",
    ReferenceNo == 34 ~ "Rental by client in a public housing unit",
    ReferenceNo == 10 ~ "Rental by client, no ongoing housing subsidy",
    ReferenceNo == 20 ~ "Rental by client, with other ongoing housing subsidy",
    ReferenceNo == 21 ~ "Owned by client, with ongoing housing subsidy",
    ReferenceNo == 11 ~ "Owned by client, no ongoing housing subsidy",
    ReferenceNo == 30 ~ "No exit interview completed",
    ReferenceNo == 17 ~ "Other",
    ReferenceNo == 24 ~ "Deceased",
    ReferenceNo == 37 ~ "Worker unable to determine",
    ReferenceNo == 99 ~ "Data not collected"
  )
}

#' @title Project Type Reference Number Translation `r lifecycle::badge("deprecated")`
#' @description Deprecated in favor of `hud_translations`. Return a human-readable project type provided a reference number
#' @param ReferenceNo \code{(integer)} Reference number for project type
#' @return \code{(character)} Human-readable project type
#' @export

project_type <- function(ReferenceNo){
  dplyr::case_when(
    ReferenceNo == 1 ~ "Emergency Shelter",
    ReferenceNo == 2 ~ "Transitional Housing",
    ReferenceNo == 3 ~ "Permanent Supportive Housing",
    ReferenceNo == 4 ~ "Street Outreach",
    ReferenceNo == 6 ~ "Services Only",
    ReferenceNo == 8 ~ "Safe Haven",
    ReferenceNo == 12 ~ "Prevention",
    ReferenceNo == 13 ~ "Rapid Rehousing",
    ReferenceNo == 14 ~ "Coordinated Entry"
  )
}

#' @title Yes/No to numeric binary `1/0` 
#' @description Change a binary character `"No"` or `NA` to `0` and everything else to `1`
#' @param column_name \code{(character)} Column vector. Quasiquoted name if used in \link[dplyr]{mutate}.
#' @return \code{(Integer)} Binary encoded integer vector
#' @export


replace_yes_no <- function(column_name) {
  dplyr::if_else(column_name == "No" | is.na(column_name), 0, 1)
}

#' @title Translate HUD reference numbers `r lifecycle::badge("deprecated")`
#' @description Deprecated in favor of `hud_translations`. Translate HUD reference numbers
#' @param ReferenceNo \code{(integer)} vector. Quasiquoted column name if used in \link[dplyr]{mutate}.
#' @return \code{(character)} Human-readable corresponding character vector.
#' @export

enhanced_yes_no_translator <- function(ReferenceNo) {
  dplyr::case_when(
    ReferenceNo == 0 ~ "No",
    ReferenceNo == 1 ~ "Yes",
    ReferenceNo == 8 ~ "Client doesn't know",
    ReferenceNo == 9 ~ "Client refused",
    ReferenceNo == 99 ~ "Data not collected"
  )
}

#' @title Translate HUD reference numbers `r lifecycle::badge("deprecated")`
#' @description Deprecated in favor of `hud_translations` This function translates the HUD csv 1.7 and 1.8 lists and returns yes, no, or unknown as appropriate. 
#' @param column_name \code{(integer)} Column vector. Quasiquoted column name if used in \link[dplyr]{mutate}.
#' @return \code{(character)} Human-readable corresponding character vector.
#' @export

translate_HUD_yes_no <- function(column_name){
  dplyr::case_when(
    column_name == 1 ~ "Yes",
    column_name == 0 ~ "No",
    column_name %in% c(8, 9, 99) ~ "Unknown"
  )
}

    
#' @title Translate Numeric/Character from a Hash Table with columns Value/Text
#'
#' @param x \code{(character/numeric)} vector to translate
#' @param hash \code{(data.frame)} with Value/Text numeric/character corresponding to the value to translate
#'
#' @return \code{(numeric/character)} Whichever class is opposite the input vector
#' @export

hud_translate <- function(x, hash) {
  UseMethod("hud_translate")
}
#' @title S3 Method for hud_translate
#' @export
hud_translate.numeric <- function(x, hash) {
  out <- rep(NA_character_, length(x))
  na <- is.na(x)
  out[!na] <- purrr::map_chr(x[!na], ~hash[[2]][.x == hash[[1]]])
  out
}
#' @title S3 Method for hud_translate
#' @export
hud_translate.character <- function(x, hash) {
  out <- rep(NA_real_, length(x))
  na <- is.na(x)
  out[!na] <- purrr::map_dbl(x[!na], ~hash[[1]][.x == hash[[2]]])
  out
}

#' @title Translate HUD Coding of Data Elements
#' @description Translate values from HUD Data elements between values or text
#' @param .x \code{(character/numeric)} The values to translate
#' @return \code{(character/numeric)} equivalent, depending on the input
#' @export
hud_translations <- list.files(full.names = TRUE, file.path("inst", "export_text_translations", "2022")) |>
  {\(x) {rlang::set_names(x, stringr::str_remove(basename(x), "\\.feather"))}}() |>
  purrr::map(~
               rlang::new_function(args = rlang::pairlist2(.x = , table = FALSE), body = rlang::expr({
                 hash <- feather::read_feather(system.file("export_text_translations", !!file.path("2022", basename(.x)), package = "HMIS", mustWork = TRUE))
                 if (!"Value" %in% names(hash) || !(is.character(hash[[2]]) && is.numeric(hash[[1]]))) {
                   rlang::warn("Translation table is irregular and isn't supported for translation. Returning table as-is")
                   return(hash)
                 }
                 
                 if (table) {
                   out <- hash
                 } else {
                   out <- hud_translate(.x, hash)
                 }
                 out
               })
               )
  ) |>
  {\(x) {rlang::list2(
    !!!x
  )}}()