ReportStart <- NULL
ReportEnd <- NULL

#CHANGED New Between function
#' @title between_df
#'
#' @name between_df
#' @family _between
#'
#' @keywords Internal
#'
#' @description Context-sensitive quick filtering or output of logical based on
#'  a date range. All assumptions about the data are based on the most recent 
#'  HUD HMIS Data Standards. \url{https://www.hudexchange.info/resource/3824/hmis-data-dictionary}
#'  The family of *_between functions helps HMIS data analysts easily shape
#'  enrollment or project descriptor data based on a single date range, avoiding
#'  errors and repeated code.
#'
#' @param . data.frame/tibble. In a `magrittr`, pipe this will always be the
#' first object
#'
#' @param status character, One of "served", "stayed", "entered", "exited",
#' "operating", or "beds_available".
#' \itemize{
#'   \item{\code{"served"}}{ Enrollments that overlap the date range specified,
#'   based on Entry and Exit Dates. Equivalent of \code{served_between()}}
#'   \item{\code{"stayed"}}{ Enrollments that overlap the date range specified,
#'   based on Exit and the Entry Dates for ES, SH, TH projects and Move In Dates
#'   for RRH and PSH projects. Requires an EntryAdjust variable in the
#'   dataframe. EntryAdjust is based on the ProjectType of the ProjectID
#'   associated with the Enrollment and a valid MoveInDate. (Future version will
#'   not require EntryAdjust as long as ProjectType and a valid MoveInDate are
#'   present in the dataset.) Equivalent of \code{stayed_between()}}
#'   \item{\code{"entered"}}{ Enrollments where the Entry Date is in the date
#'   range specified. Used to find households who entered during date range.
#'   Equivalent of \code{entered_between()}}
#'   \item{\code{"exited"}}{ Enrollments where the Exit Date is in the date range
#'   specified. Used to find households who exited during date range.
#'   Equivalent of \code{exited_between()}}
#'   \item{\code{"operating"}}{ Projects where the Operating Start and End dates
#'   overlap the date range specified, including those with null Operating End
#'   Dates. Equivalent of \code{operating_between()}}
#'   \item{\code{"beds_available"}}{ Projects with active beds during the date
#'   range specified. Equivalent of \code{beds_available_between()}}
#' }
#'
#' @param start Default = ReportStart, character/date of the start of the date 
#'  range. Characters in format mdY, Ymd, dmY acceptable. Will be automatically
#'  retrieved from parent environments if not specified. If start is named other 
#'  than ReportStart, it must be specified.
#'
#' @param end Default = ReportEnd, character/date of the end of the date range. 
#'  Characters in format mdY, Ymd, dmY acceptable. Will be automatically 
#'  retrieved from parent environments if not specified. If end is named other
#'  than ReportEnd, it must be specified.
#'
#' @param lgl Default = FALSE, logical, flag to force logical vector output. 
#'
#' @details Context-sensitive: Automatically detects if nested inside
#'  of \link[dplyr]{filter} call, if so returns `logical` instead of `data.frame`.
#'  Enrollment-related functions require Enrollment data elements such as Entry
#'  Date, Exit Date, Move In Date, and Project Type (of the Enrollment's Project).
#'  Project Descriptor-related functions require Project data elements such as
#'  Operating Start/End dates and Bed Inventory Start/End dates.
#'
#' @return \code{data.frame/logical} after filtering/applying conditional to the
#'  appropriate columns
#'  
#' @examples
#' \dontrun{
#' library(dplyr)
#' ReportStart <- Sys.Date() - lubridate::weeks(4)
#' ReportEnd <- Sys.Date()
#' identical(
#' served_between(qpr_leavers),
#' qpr_leavers %>% filter(served_between(.))
#' )
#' }
#'
#' @importFrom rlang abort sym `!!` expr eval_tidy
#' @importFrom stringr str_detect
#' @importFrom purrr map_lgl
#' @importFrom dplyr filter
#' @importFrom utils tail


between_df <-
  function(.,
           status,
           start = ReportStart,
           end = ReportEnd,
           lgl = FALSE) {
    ExitDate <- NULL
    #Check date format and coerce if need be
    dates <- check_dates(start, end)
    
    # if no status supplied, throw error
    if (missing(status)) {
      rlang::abort("Please supply a status. See ?between_df for details.")
    }
    # Check calling context - if inside of a filter call, return the logical
    .lgl <- purrr::map_lgl(tail(sys.calls(), 5),
                           ~ {
                             any(grepl("eval_all_filter", as.character(.x)))
                           })
    .lgl <- sum(.lgl) > 0
    # Convert that to a character for regex parsing
    .cn_chr <- tolower(status)
    # If it's one of served of stayed
    if (stringr::str_detect(.cn_chr, "served|stayed")) {
      if (stringr::str_detect(.cn_chr, "served")) {
        # if served use entrydate
        .col <- rlang::sym("EntryDate")
      } else if (stringr::str_detect(.cn_chr, "stayed")) {
        # if stayed used entryadjust
        .col <- rlang::sym("EntryAdjust")
      }
      .cond <- rlang::expr(!!.col <= dates["end"] &
                             (is.na(ExitDate) |
                                ExitDate >= dates["start"]))
      if (.lgl || lgl) {
        .out <- rlang::eval_tidy(.cond, data = .)
      } else {
        #filter the appropriate columns
        .out <- dplyr::filter(.,!!.cond)
      }
    } else if (stringr::str_detect(.cn_chr, "entered|exited")) {
      # if its entered or exited
      if (stringr::str_detect(.cn_chr, "entered")) {
        # if entered use entrydate
        .col <- rlang::sym("EntryDate")
      } else if (stringr::str_detect(.cn_chr, "exited")) {
        #if exited use exit date
        .col <- rlang::sym("ExitDate")
      }
      .cond <-
        rlang::expr(!!.col >= dates["start"] & !!.col <= dates["end"])
      if (.lgl || lgl) {
        .out <- rlang::eval_tidy(.cond, data = .)
      } else {
        #filter the appropriate columns
        .out <- dplyr::filter(.,!!.cond)
      }
    } else if (stringr::str_detect(.cn_chr, "beds_available|operating")) {
      if (stringr::str_detect(.cn_chr, "operating")) {
        .prefix <- "Operating"
      } else if (stringr::str_detect(.cn_chr, "beds_available")) {
        .prefix <- "Inventory"
      }
      # Construct column names from prefixes
      .cols <- paste0(.prefix, c("StartDate", "EndDate"))
      # Extract the appropriate columns
      .cols <- purrr::map(.cols, rlang::sym)
      .cond <- rlang::expr(!!.cols[[1]] <= dates["end"] &
                             (is.na(!!.cols[[2]]) |
                                !!.cols[[2]] >= dates["start"]))
      if (.lgl || lgl) {
        .out <- rlang::eval_tidy(.cond, data = .)
      } else {
        #filter the appropriate columns
        .out <- dplyr::filter(.,!!.cond)
      }
    }
    .out
  }


# CHANGED Check and coerce dates as sub-function to split between_ into two
#  logical segments, one which outputs a data.frame (for use in Rminor) and one
#  which outputs a logical for use in COHHIO_HMIS
#' @title check_dates
#' @name check_dates
#' @description coerce start/end input values to dates
#' @inheritParams between_df
#' @keywords Internal
#' @importFrom purrr map_lgl imap
#' @importFrom lubridate parse_date_time as_date
#' @importFrom rlang abort


check_dates <- function(start, end) {
  # Add input dates to list
  .dates <- list(start = start, end = end)
  # Check if inputs are all Date or POSIXct
  .test_date <- purrr::map_lgl(.dates, ~ {
    inherits(.x, c("Date"))
  })
  # If not
  if (!all(.test_date)) {
    # map over the ones that aren't
    .dates <- purrr::imap(.dates, ~ {
      if (inherits(.x, c("POSIXct", "POSIXlt", "numeric"))) {
        .out <- lubridate::as_date(.x)
      } else if (inherits(.x, "character")) {
        # try these formats
        .out <-
          lubridate::parse_date_time(.x, c("Ymd", "mdY", "dmY"))
        .out <- as.Date(.out)
      }
      if (!inherits(.out, c("Date"))) {
        # if none of those formats worked throw error and inform user which
        # argument was not able to be parsed
        rlang::abort(paste(
          .y,
          "Could not be parsed to a Datetime, please check argument."
        ))
      }
      .out
    })
    # bind the coerced Date/Datetimes to the environment, overwriting the
    # existing values
  }
  do.call(c, .dates)
}


# Client Entry Exits Between Date Range Functions -------------------------

#' @title served_between
#' @name served_between
#' @family _between
#' @inherit between_df
#' @inheritParams between_df
#' @description Filters a dataframe of Enrollments that overlap the ReportStart
#'  to ReportEnd date range. Most commonly used in reporting project stays in a 
#'  date range
#' @seealso stayed_between(), entered_between(), exited_between()
#' @export

served_between <-
  function(.,
           start = ReportStart,
           end = ReportEnd,
           lgl = FALSE) {
    between_df(., "served", start, end, lgl)
  }

#' @title entered_between
#' @name entered_between
#' @description Filters a dataframe of Enrollments where the Entry Date falls
#'  within the date range specified. Used to report on households entering
#'  your system in a date range. Useful for measuring intake policies or
#'  practices and adherence to Housing First principles.
#' @inherit between_df
#' @family _between
#' @export
#' 

entered_between <-
  function(.,
           start = ReportStart,
           end = ReportEnd,
           lgl = FALSE) {
    between_df(., "entered", start, end, lgl)
  }

#' @title exited_between
#' @name exited_between
#' @description Filters a dataframe of Enrollments where the Exit Date falls
#'  within the date range specified. Used to report on households leaving a
#'  project. Useful for measuring outcomes.
#' @inherit between_df
#' @family _between
#' @export

exited_between <-
  function(.,
           start = ReportStart,
           end = ReportEnd,
           lgl = FALSE) {
    between_df(., "exited", start, end, lgl)
  }

#' @title stayed_between
#' @name stayed_between
#' @description Much like served_between(), but for Rapid Rehousing and 
#'  Permanent Supportive Housing projects, it measures from Move In Date to Exit
#'  Date instead of Entry Date to Exit Date. Useful for when you want to only
#'  count households who moved into housing or measure the time that household
#'  occupied bed inventory.
#' @inherit between_df
#' @family _between
#' @export

stayed_between <-
  function(.,
           start = ReportStart,
           end = ReportEnd,
           lgl = FALSE) {
    between_df(., "stayed", start, end, lgl)
  }

#' @title operating_between
#' @name operating_between
#' @description Filters a dataframe of projects active within the date range 
#'  specified. Used to exclude inactive projects from reporting. Requires
#'  OperatingStartDate and OperatingEndDate in the dataframe.
#' @inherit between_df
#' @family _between
#' @export

operating_between <-
  function(.,
           start = ReportStart,
           end = ReportEnd,
           lgl = FALSE) {
    between_df(., "operating", start, end, lgl)
  }

#' @title beds_available_between
#' @name beds_available_between
#' @description Filters a dataframe of Bed Inventory data where the 
#'  InventoryStartDate and InventoryEndDate overlap the date range specified. 
#'  Useful for excluding providers from Utilization reporting who do not have
#'  active bed/unit inventory and bed/unit inventory data not active during the
#'  specified date range.
#' @inherit between_df
#' @family _between
#' @export

beds_available_between <- function(.,
                                   start = ReportStart,
                                   end = ReportEnd,
                                   lgl = FALSE) {
  between_df(., "beds_available", start, end, lgl)
}
