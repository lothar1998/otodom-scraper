#' @importFrom dplyr "%>%"

root_url <- 'https://www.otodom.pl'

#' Get offer paths
#'
#' Fetch paths from otodom.pl search site.
#'
#' @param path Path to the search site.
#' @return Paths to offers from search site.
#' @export
get_offer_paths <- function(path) {
  url <- paste0(root_url, path)
  paths <- rvest::read_html(url) %>%
    rvest::html_nodes(css = 'div[role="main"] > div[data-cy="search.listing"] > ul > li > a') %>%
    rvest::html_attr('href')
  return(paths)
}

#' Get data from offer path
#'
#' Scrap house estate data from given path on otodom.pl
#'
#' @param path Path to the offer site.
#' @return Tibble with scraped data.
#' @export
get_data_from_offer_path <- function(path) {
  url <- paste0(root_url, path)
  html <- rvest::read_html(url)

  name <- html %>%
    rvest::html_node(css = '[data-cy="adPageAdTitle"]') %>%
    rvest::html_text()

  get_param <- function(html, name) {
    value <- html %>%
      rvest::html_node(css = sprintf('[aria-label="%s"]', name)) %>%
      rvest::html_text(trim = TRUE)

    if (identical(value, character(0))) {
      return()
    }

    return(value)
  }

  get_param_nested <- function(html, name) {
    value <- html %>%
      rvest::html_node(css = sprintf('[aria-label="%s"]', name)) %>%
      rvest::html_nodes(css = 'div:nth-last-child(1)') %>%
      rvest::html_text(trim = TRUE)

    if (identical(value, character(0))) {
      return()
    }

    return(value)
  }

  result <- dplyr::tibble("Nazwa" = name,
                   "Cena" = get_param(html, "Cena"),
                   "Cena za metr kwadratowy" = get_param(html, "Cena za metr kwadratowy"),
                   "Adres" = get_param(html, "Adres"),
                   "Powierzchnia" = get_param_nested(html, "Powierzchnia"),
                   "Liczba pokoi" = get_param_nested(html, "Liczba pokoi"),
                   "Rynek" = get_param_nested(html, "Rynek"),
                   "Piętro" = get_param_nested(html, "Piętro"),
                   "Rok budowy" = get_param_nested(html, "Rok budowy"),
                   "Rodzaj zabudowy" = get_param_nested(html, "Rodzaj zabudowy"),
                   "Liczba pięter" = get_param_nested(html, "Liczba pięter"),
                   "Okna" = get_param_nested(html, "Okna"),
                   "Stan wykończenia" = get_param_nested(html, "Stan wykończenia"),
                   "Materiał budynku" = get_param_nested(html, "Materiał budynku"),
                   "Ogrzewanie" = get_param_nested(html, "Ogrzewanie"),
                   "Forma własności" = get_param_nested(html, "Forma własności"),
                   "Czynsz" = get_param_nested(html, "Czynsz"),
                   "Obsługa zdalna" = get_param_nested(html, "Obsługa zdalna"),
                   "Dostępne od" = get_param_nested(html, "Dostępne od"))

  return(result)
}

#' Get data
#'
#' Combines get_offer_paths and get_data_from_offer_path to facilitate
#' loading of many offers from search site.
#'
#' @param path Path to search site.
#' @return Tibble with scraped data for all offers from search site.
#' @export
get_data <- function(path) {
  offer_paths <- get_offer_paths(path)

  data <- dplyr::tibble()
  for (offer_path in offer_paths) {
    result <- get_data_from_offer_path(offer_path)
    data <- dplyr::bind_rows(data, result)
  }

  return(data)
}