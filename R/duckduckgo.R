
duck_url <- "https://api.duckduckgo.com"
name_of_app <- "websearchr"

duck_header <- function() {
  httr::add_headers(httr::accept_json())
}

duck_call <- function(
  query
) {
  httr::GET(
    url = duck_url,
    httr::accept_json(),
    query = list(
      # no_html - to remove html markup; t - to provide name of application
      no_html = 1, t = name_of_app))
}
