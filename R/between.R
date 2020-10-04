ReportStart <- NULL
ReportEnd <- NULL



#CHANGED New Between function
#' @title between_df
#' @name between_df
#' @family _between
#' @keywords Internal
#' @description Context sensitive quick filtering or output of logical based on `start` and `end` Dates. 
#' @param . \code{(data.frame/tibble)} Input to be filtered. In a `magrittr` pipe this will always be the first object
#' @param status \code{(character)} One of:
#' \itemize{
#'   \item{\code{"served"/"se"}}{ Equivalent of \code{served_between}}
#'   \item{\code{"stayed"/"st"}}{ Equivalent of \code{stayed_between}}
#'   \item{\code{"entered"/"en"}}{ Equivalent of \code{entered_between}}
#'   \item{\code{"exited"/"ex"}}{ Equivalent of \code{exited_between}}
#'   \item{\code{"operating"/"op"}}{ Equivalent of \code{operating_between}}
#'   \item{\code{"beds_available"/"be"/"ba"}}{ Equivalent of \code{beds_available_between}}
#' }
#' that specifies the type of function to be performed
#' @param start \code{character/Date} of the end of the timeframe. Characters in format mdY, Ymd, dmY acceptable.  **Default `ReportStart`** will be automatically retrieved from parent environments if not specified. If end is named other than ReportStart, it must be specified. 
#' @param end \code{character/Date} of the end of the timeframe. Characters in format mdY, Ymd, dmY acceptable.  **Default `ReportEnd`** will be automatically retrieved from parent environments if not specified. If end is named other than ReportEnd, it must be specified. 
#' @param lgl \code{logical} Flag to force logical vector output. **Default `FALSE`**
#' @details Context-sensitive: Automatically detects if nested inside of \link[dplyr]{filter} call, if so returns `logical` instead of `data.frame`
#' @return \code{data.frame/logical} after filtering/applying conditional to the appropriate columns
#' @examples 
#' \dontrun{
#' library(dplyr)
#' ReportStart = Sys.Date() - lubridate::weeks(4)
#' ReportEnd = Sys.Date()
#' identical(
#' served_between(qpr_leavers),
#' qpr_leavers %>% filter(served_between(.))
#' )
#' }
#' @importFrom rlang abort sym `!!` expr eval_tidy
#' @importFrom stringr str_detect
#' @importFrom purrr map_lgl
#' @importFrom dplyr filter
#' @importFrom utils tail


between_df <- function(., status, start = ReportStart, end = ReportEnd, lgl = FALSE) {
  ExitDate <- NULL
  #Check date format and coerce if need be
  dates <- check_dates(start, end)
  
  # if no status supplied, throw error
  if (missing(status)) {
    rlang::abort("Please supply a status. See ?between_df for details.")
  } 
  # Check calling context - if inside of a filter call, return the logical
  .lgl <- purrr::map_lgl(tail(sys.calls(),5), ~{any(grepl("eval_all_filter", as.character(.x)))})
  .lgl <- sum(.lgl) > 0
  # Convert that to a character for regex parsing
  .cn_chr <- tolower(substr(status, 0, 2))
  # If it's one of served of stayed
  if (stringr::str_detect(.cn_chr, "se|st")) {
    if (stringr::str_detect(.cn_chr, "se")) {
      # if served use entrydate
      .col <- rlang::sym("EntryDate")
    } else if (stringr::str_detect(.cn_chr, "st")) {
      # if stayed used entryadjust
      .col <- rlang::sym("EntryAdjust")
    }
    .cond <- rlang::expr(!!.col <= dates["end"] & (is.na(ExitDate) | ExitDate >= dates["start"]))
    if (.lgl || lgl) {
      .out <- rlang::eval_tidy(.cond, data = .)
    } else {
      #filter the appropriate columns
      .out <- dplyr::filter(., !!.cond)
    }
  } else if (stringr::str_detect(.cn_chr, "en|ex")) {
    # if its entered or exited
    if (stringr::str_detect(.cn_chr, "en")) {
      # if entered use entrydate
      .col <- rlang::sym("EntryDate")
    } else if (stringr::str_detect(.cn_chr, "ex")) {
      #if exited use exit date
      .col <- rlang::sym("ExitDate")
    }
    .cond <- rlang::expr(!!.col >= dates["start"] & !!.col <= dates["end"])
    if (.lgl || lgl) {
      .out <- rlang::eval_tidy(.cond, data = .)
    } else {
      #filter the appropriate columns
      .out <- dplyr::filter(., !!.cond)
    }
  } else if (stringr::str_detect(.cn_chr, "ba|be|op")) {
    if (stringr::str_detect(.cn_chr, "op")) {
      .prefix <- "Operating"
    } else if (stringr::str_detect(.cn_chr, "be|ba")) {
      .prefix <- "Inventory"
    }
    # Construct column names from prefixes
    .cols <- paste0(.prefix, c("StartDate", "EndDate"))
    # Extract the appropriate columns
    .cols <- purrr::map(.cols, rlang::sym)
    .cond <- rlang::expr(!!.cols[[1]] <= dates["end"] &
                           (is.na(!!.cols[[2]]) | !!.cols[[2]] >= dates["start"]))
    if (.lgl || lgl) {
      .out <- rlang::eval_tidy(.cond, data = .)
    } else {
      #filter the appropriate columns
      .out <- dplyr::filter(., !!.cond)
    }
  }
  .out
}


# CHANGED Check and coerce dates as sub-function to split between_ into two logical segments, one which outputs a data.frame (for use in Rminor) and one which outputs a logical for use in COHHIO_HMIS
#' @title check_dates
#' @name check_dates
#' @description coerce start/end input values to Date's
#' @inheritParams between_df
#' @keywords Internal
#' @importFrom purrr map_lgl imap
#' @importFrom lubridate parse_date_time as_date
#' @importFrom rlang abort


check_dates <- function(start, end) {
  # Add input dates to list
  .dates <- list(start = start, end = end)
  # Check if inputs are all Date or POSIXct
  .test_date <- purrr::map_lgl(.dates, ~{inherits(.x, c("Date"))})
  # If not
  if (!all(.test_date)) {
    # map over the one's that arent
    .dates <- purrr::imap(.dates, ~{
      if (inherits(.x, c("POSIXct", "POSIXlt", "numeric"))) {
        .out <- lubridate::as_date(.x)  
      } else if (inherits(.x, "character")) {
        # try these formats
        .out <- lubridate::parse_date_time(.x, c("Ymd", "mdY", "dmY"))
        .out <- as.Date(.out)  
      } 
      if (!inherits(.out, c("Date"))) {
        # if none of those formats worked throw error and inform user which argument was not able to be parsed
        rlang::abort(paste0(.y, " could not be parsed to a Datetime, please check argument."))
      }
      .out
    })
    # bind the coerced Date/Datetimes to the environment, overwriting the existing values
  } 
  do.call(c,.dates)
}


# Client Entry Exits Between Date Range Functions -------------------------------------
#' @title served_between
#' @name served_between
#' @family _between
#' @inherit between_df
#' @inheritParams between_df
#' @export

served_between <- function(., start = ReportStart, end = ReportEnd, lgl = FALSE) {
  between_df(., "served", start, end, lgl)
}

#' @title entered_between
#' @name entered_between
#' @inherit between_df
#' @family _between
#' @export
entered_between <- function(., start = ReportStart, end = ReportEnd, lgl = FALSE) {
  between_df(., "entered", start, end, lgl)
}

#' @title exited_between
#' @name exited_between
#' @inherit between_df
#' @family _between
#' @export
exited_between <- function(., start = ReportStart, end = ReportEnd, lgl = FALSE){
  between_df(., "exited", start, end, lgl)
}

#' @title stayed_between
#' @name stayed_between
#' @inherit between_df
#' @family _between
#' @export
stayed_between <- function(., start = ReportStart, end = ReportEnd, lgl = FALSE){
  between_df(., "stayed", start, end, lgl)
}

#' @title operating_between
#' @name operating_between
#' @inherit between_df
#' @family _between
#' @export
operating_between <- function(., start = ReportStart, end = ReportEnd, lgl = FALSE){
  between_df(., "op", start, end, lgl)
}

#' @title beds_available_between
#' @name beds_available_between
#' @inherit between_df
#' @family _between
#' @export
beds_available_between <- function(., start = ReportStart, end = ReportEnd, lgl = FALSE){
  between_df(., "ba", start, end, lgl)
}