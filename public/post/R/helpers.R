library(readxl)
library(writexl)
library(rvest)
library(tidyverse)
library(DT)

Sys.setlocale("LC_TIME", "us")

get_play_store <- function(url) {
  
  # create an empty list container
  res <- list()
  
  # create an rvest object using url
  html_obj <- read_html(url)
  
  res$code <- names(url)
  
  details <- html_obj %>%
    html_nodes(".hAyfc") %>%
    html_children() %>%
    html_text()
  
  # create a namelist of details of the app
  details <- setNames(
    details[seq(2, length(details), 2)], 
    details[seq(1, length(details), 2)])
  
  
  res$img <- html_obj %>%
    html_nodes("img") %>%
    `[[`(1) %>% 
    html_attr("src")
  res$img <- glue::glue("<img src = {res$img}>") # make it html tag
  
  res$name <- html_obj %>%
    html_nodes(".AHFaub") %>%
    html_text()
  
  res$org <- details[["Offered By"]]
  res$date_update <- details[["Updated"]] %>% 
    str_replace(",", "") %>%
    lubridate::as_date(format = "%B %d %Y")
  res$n_downloads <- details[["Installs"]]

  res$desc <- html_obj %>%
    html_nodes(".DWPxHb") %>%
    html_text() %>%
    `[[`(1)

  return(data.frame(res))
}

datatable_th <- function(df, ...) {
  url_dt_thai <- '//cdn.datatables.net/plug-ins/1.10.11/i18n/Thai.json'
  
  datatable(
    df, 
    options = list(
      language = list(url = url_dt_thai)
    ),
    ...
  )
}

DT_app <- function(df, ...) {
  url_dt_thai <- '//cdn.datatables.net/plug-ins/1.10.11/i18n/Thai.json'
  datatable(
    df, 
    escape = FALSE,
    options = list(
      language = list(url = url_dt_thai),
      pageLength = 5,
      columnDefs = list(
        list(
          targets = 6,
          render = JS("function(data, type, row, meta) {", "return type === 'display' && data.length > 300?", "'<span title=\"' + data + '\">' + data.substr(0, 300) + '...</span>' : data;", "}")
        )
      )
    ),
    callback = JS('table.page(3).draw(false);'),
    ...
  )
}