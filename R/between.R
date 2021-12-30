ReportStart <- NULL
ReportEnd <- NULL

#' @title check_names
#' @keywords Internal
#' @description Ensure the appropriate names required for filtering are present in the data and if not, give informative error.
#' @param . \code{(data.frame/tibble)} data input
#' @param nms \code{(character)} Vector of column names that must be present

check_names <- function(x, nms) {
  .cn <- colnames(x)
  .lgl <- sapply(nms, `%in%`,  table = .cn)
  if (!all(.lgl)) {
    stop("Columns ", paste0(nms, collapse = ", ")," must be present in data.\n",
         "* Missing: ", paste0(nms[!.lgl], collapse = ", "))
  }
}



#CHANGED New Between function
#' @title between_df
#'
#' @name between_df
#' 
#' @family _between
#'
#' @description Context-sensitive quick filtering or output of logical based on
#'   a date range. All assumptions about the data are based on the most recent 
#'   \href{https://www.hudexchange.info/resource/3824/hmis-data-dictionary}{HUD HMIS Data Standards}.
#'   The family of *_between functions helps HMIS data analysts easily shape
#'   enrollment or project descriptor data based on a single date range, avoiding
#'   errors and repeated code.
#'
#' @param x \code{(data.frame)} to filter.
#'
#' @param status character, One of "served", "stayed", "entered", "exited",
#'   "operating", or "beds_available".
#' \describe{
#'   \item{"served"}{ Enrollments that overlap the date range specified,
#'   based on Entry and Exit Dates. Equivalent of \code{served_between()}}
#'   \item{"stayed"}{ Enrollments that overlap the date range specified,
#'   based on Exit and the Entry Dates for ES, SH, TH projects and Move In Dates
#'   for RRH and PSH projects. Requires an EntryAdjust variable in the
#'   dataframe. EntryAdjust is based on the ProjectType of the ProjectID
#'   associated with the Enrollment and a valid MoveInDate. (Future version will
#'   not require EntryAdjust as long as ProjectType and a valid MoveInDate are
#'   present in the dataset.) Equivalent of \code{stayed_between()}}
#'   \item{"entered"}{ Enrollments where the Entry Date is in the date
#'   range specified. Used to find households who entered during date range.
#'   Equivalent of \code{entered_between()}}
#'   \item{"exited"}{ Enrollments where the Exit Date is in the date range
#'   specified. Used to find households who exited during date range.
#'   Equivalent of \code{exited_between()}}
#'   \item{"operating"}{ Projects where the Operating Start and End dates
#'   overlap the date range specified, including those with null Operating End
#'   Dates. Equivalent of \code{operating_between()}}
#'   \item{"beds_available"}{ Projects with active beds during the date
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
#' qpr_leavers %>% filter(served_between(., lgl = TRUE))
#' )
#' }
#'
#' @importFrom rlang abort sym `!!` expr eval_tidy
#' @importFrom stringr str_detect
#' @importFrom purrr map_lgl
#' @importFrom dplyr filter
#' @importFrom utils tail
#' @export

between_df <-
  function(x,
           status,
           start = NULL,
           end = NULL,
           lgl = FALSE) {
    ExitDate <- NULL
    #Check date format and coerce if need be
    dates <- check_dates(start, end)
    
    # if no status supplied, throw error
    if (missing(status))
      rlang::abort("Please supply a status. See ?between_df for details.")
    
    # Check calling context - if inside of a filter call, return the logical
    .lgl <- any(purrr::map_lgl(tail(sys.calls(), 5),
                               ~ {
                                 any(grepl("eval_all_filter", as.character(.x)))
                               }))
    
    # Convert that to a character for regex parsing
    .cn_chr <- tolower(status)
    .cols <- switch(
      .cn_chr,
      entered = ,
      served = c("EntryDate", "ExitDate"),
      stayed = c("EntryAdjust", "ExitDate"),
      exited = rep("ExitDate", 2),
      operating = paste("Operating", c("StartDate", "EndDate")),
      beds_available = paste("Inventory", c("StartDate", "EndDate"))
    )
    check_names(x, .cols)
    .cols <- purrr::map(.cols, rlang::sym)
    .exp <- rlang::exprs(
      lte_end = !!.cols[[1]] <= dates["end"],
      na_date = is.na(!!.cols[[2]]),
      gte_st = !!.cols[[2]] >= dates["start"]
    )
    .cond <- switch(
      .cn_chr,
      entered = ,
      exited = rlang::expr(!!.exp$gte_st & !!.exp$lte_end),
      served = ,
      stayed = ,
      operating = ,
      beds_available = rlang::expr((!!.exp$gte_st |
                                      !!.exp$na_date) & !!.exp$lte_end)
    )
    if (lgl || .lgl)
      out <- rlang::eval_tidy(.cond, data = x)
    else
      out <- dplyr::filter(x,!!.cond)
    
    out
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
  # If not all dates
  if (!all(purrr::map_lgl(.dates, inherits, what = "Date"))) {
    # map over the ones that aren't
    .dates <- purrr::imap(.dates, make_date)
  }
  rlang::set_names(do.call(c, .dates), c("start", "end"))
}
#' @title Coerce various inputs to \code{(Date)}
#'
#' @param x \code{(character/numeric/POSIXct/lt)}
#'
#' @return \code{(Date)}
#' @export

make_date <- function(x) {
  UseMethod("make_date")
}

#' @export
#' @inherit make_date
make_date.POSIXct <- make_date.POSIXlt <- function(x) {
  lubridate::as_date(x)
}
#' @export
#' @inherit make_date
make_date.numeric <- function(x) {
  if (all(x < 100000)) {
    # Handle Dates
    .out <- lubridate::as_date(x, origin = lubridate::origin)
  } else {
    # Handle datetimes
    .out <- lubridate::as_datetime(signif(x / 10 ^ ceiling(log10(x)), 10) * 10 ^ 10, origin = lubridate::origin, tz = tz)
    .out <- lubridate::as_date(.out)
  }
  .out
}
#' @export
#' @inherit make_date
make_date.character <- function(x) {
  lubridate::as_date(lubridate::parse_date_time(.x, c("Ymd", "mdY", "dmY")))
}
#' @export
#' @inherit make_date
make_date.Date <- function(x) {
  x
}
#' @export
#' @inherit make_date
make_date.default <- function(x) {
  rlang::abort(paste0(x, " is of unknown type"), trace = rlang::trace_back())
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
           start = NULL,
           end = NULL,
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
           start = NULL,
           end = NULL,
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
           start = NULL,
           end = NULL,
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
           start = NULL,
           end = NULL,
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
           start = NULL,
           end = NULL,
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
                                   start = NULL,
                                   end = NULL,
                                   lgl = FALSE) {
  between_df(., "beds_available", start, end, lgl)
}
