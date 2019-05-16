bing_url <- "https://api.cognitive.microsoft.com/bing/v7.0/search"

bing_header <- function() {
  httr::add_headers(httr::accept_json())
}

bing_call <- function(
  query
) {
  httr::GET(url = bing_url, httr::accept_json(), query = list(answerCount = 5, safeSearch = "Strict"))
}
