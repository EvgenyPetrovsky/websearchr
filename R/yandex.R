#' Initiate yandex search
#'
#' Function takes more or less stable parameters and returns another
#' unified search function that takes 3 parameters:
#' query, numner of results on page and page number
#'
#' @export
#'
#' @param user_name Yandex user
#' @param api_key API key given to Yandex user
#' @param sort_by Sort mode: \code{rlv} - relevance, \code{tm} time
#' @param filter filter mode: \code{none}, \code{moderate} or \code{strict}
init_yandex_search <- function(
  user_name,
  api_key,
  sort_by = c("rlv", "tm"),
  filter = c("none", "moderate", "strict")
) {
  # function that defines stble search parameters and takes only
  fun <- function(query, n = 10, page = 0) {
    yandex_search(
      user = user_name,
      key = api_key,
      query = query,
      lr = "",
      l10n = "en",
      sortby = sort_by,
      filter = filter,
      maxpassages = "",
      groupby = yandex_group_by(mode = "flat", attr = " ", groups_on_page = n),
      page = page,
      showmecaptcha = NULL
    )
  }

  return(fun)

}

#' Create group_by parameter value for init_yandex_search function
#'
#' please see Yandex.XML documentation \link{https://tech.yandex.com/xml/doc/dg/concepts/about-docpage/}
#'
#' @export
#'
#' @param mode Grouping method. Possible values:
#'   \code{flat} — Flat grouping. Each group contains a single document.
#'   \code{deep} — Grouping by domain. Each group contains documents from a single domain.
#' @param attr Utility attribute. Depends on the value of the mode attribute.
#' @param groups_on_page Maximum number of groups that can be returned per page of search results.
#'   Acceptable values - from 1 to 100.
#' @param docs_in_group — Maximum number of documents that can be returned per group.
#'   Acceptable values — from 1 to 3.
yandex_group_by <- function(
  mode = c("flat", "deep"),
  attr = c(" ", "d"),
  groups_on_page = 10,
  docs_in_group = 1
) {
  paste(
    "attr%3D", attr,
    ".mode%3D", mode,
    ".groups-on-page%3D", groups_on_page,
    ".docs-in-group%3D", docs_in_group,
    sep = "")
}

#' Low-level yandex search function
yandex_search <- function(
  user, key, query,
  lr, l10n, sortby, filter, maxpassages, groupby, page,
  showmecaptcha
) {
  r <-
    httr::GET(
      url = "https://yandex.com/search/xml",
      query = list(
        user          = user,
        key           = key,
        query         = query,
        lr            = lr,
        l10n          = l10n,
        sortby        = sortby,
        filter        = filter,
        maxpassages   = maxpassages,
        groupby       = groupby,
        page          = page,
        showmecaptcha = showmecaptcha
      )
    ) %>%
    httr::stop_for_status()

  content <- r %>%
    httr::content(as = "parsed", type = "text/xml")

  error <- content %>%
    xml2::xml_find_first(xpath = "/yandexsearch/response/error")

  if (!is.na(error)) {
    # read error messga from response
    err_message <- paste(
      "Yandex search engine responded with error:",
      xml2::xml_find_chr(error, ".")
    )
    stop(err_message)
  }

  xpath_path = "/yandexsearch/response/results/grouping/group"
  xpath_id = paste(xpath_path, "doc/@id", sep = "/")
  xpath_url = paste(xpath_path, "doc/url", sep = "/")
  xpath_title = paste(xpath_path, "doc/title", sep = "/")

  df_result <- data.frame(
    id = xml2::xml_find_all(content, xpath_id) %>% xml2::xml_text(),
    url = xml2::xml_find_all(content, xpath_url) %>% xml2::xml_text(),
    title = xml2::xml_find_all(content, xpath_title) %>% xml2::xml_text(),
    stringsAsFactors = F
  )

  df_result
}

#' Describe error using code
#'
#' Documentation is available on \link{https://tech.yandex.com/xml/doc/dg/concepts/about-docpage/}
yandex_error <- function(code) {
  errors <- list(
    `1` = "The query text (the value passed in the query element) contains a syntactical error. For example, a query was sent that contained only two slash symbols in a row (\"//\").",
    `2`	  = "An empty search query was defined (an empty value was passed in the query element).",
    `15`  = "There are no search results for the specified search query.",
    `18`  = "The XML file cannot be validated, or invalid request parameters are set. Possible reasons:
    Incorrect tags or tag values were passed.
    The request body contains non-escaped special characters. For example, the ampersand symbol (\"&\"), and so on.
    The request page contains search results with more than 1000 entries. For example, if each page contains 10 results, this error will be returned when attempting to request page 101 and further in results.",
    `19`  = "The search query contains incompatible parameters (for example, incompatible values for the groupings element).",
    `20`  = "The reason for the error is unknown. If the error persists, contact the support service.",
    `31`  = "The user is not registered on the service.",
    `32`  = "Limit exceeded for the number of queries allowed per day. Review the information about restrictions and choose a suitable method for increasing your daily quota.",
    `33`  = "The IP address that the search request was sent from does not match the one(s) set during registration.",
    `34`  = "The user is not registered on the Yandex.Passport service.",
    `37`  = "Error in request parameters. Maybe mandatory parameters were omitted, or mutually exclusive parameters were defined.",
    `42`  = "The key that was issued during registration contains an error. Check whether the correct address is used for sending requests.",
    `43`  = "The version of the key that was issued during registration contains an error. Check whether the correct address is used for sending requests.",
    `44`  = "The address that requests are sent to is no longer supported. Correct the value to match the address that was given during registration.",
    `48`  = "The search type that was specified during registration does not match the search type that is being used for requesting data. Reset the domain that is being used to the correct domain. For corrections, use the URL for sending requests.",
    `55`  = "The number of requests sent during a second (RPS) exceeded the allowed value.",
    `100` = "The request was most likely sent by a robot. When this error appears, a CAPTCHA must be returned to the user."
  )
  errors$code
}

handle_captcha <- function() {
  NULL
}
