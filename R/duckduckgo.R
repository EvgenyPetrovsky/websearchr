#' Initiate duckduckgo search
#'
#' @export
#'
init_duck_search <- function() {
  fun <- function(query, n, page) {duck_search(q = query)}

  return(fun)
}

#' DuckDuckGo low-lewel search function
#'
#'
duck_search <- function(
  q
) {
  duck_url <- "https://api.duckduckgo.com"
  name_of_app <- "websearchr"

  r <-
    httr::GET(
      url = duck_url,
      query = list(
        # no_html - to remove html markup; t - to provide name of application
        q = q,
        no_html = 1, format = "json", pretty = 1, no_redirect = 1,
        t = name_of_app
      )
    ) %>%
    httr::stop_for_status()

  content <- r %>%
    httr::content(as = "parsed", type = "application/json")

  related_topics <- content$RelatedTopics

  related_topics %>%
    Map(
      f = function(x) {
        data.frame(
          id = x$Result, url = x$FirstURL, title = x$Text,
          stringsAsFactors = F
        )
      }
    ) %>%
    Reduce(f = rbind)

}
