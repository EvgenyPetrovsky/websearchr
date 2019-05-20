# websearchr

Tool that provides unified functional access to search engines.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
install.packages("devtools")
devtools::install_github(repo = "EvgenyPetrovsky/websearchr")

## create search function initialize search functionality
search_fun <- init_yandex_search(
  user_name = "<user_name>", 
  api_key = "<api_key>",
  sort_by = "rlv", 
  filter = "strict"
)

## fire search and receive responce
search_result <- my_search(query = "github", n = 3, page = 0)

```

In an example ebove we install package and initialize Yandex searcher. In order to do it we need to register first and obtain user_name and API key from [yandex](https://xml.yandex.com/).

During search execution server may return response on http request (communication went well) but it may contain error (search query couldn't been executed). In this case function stops its work and returns error description.

