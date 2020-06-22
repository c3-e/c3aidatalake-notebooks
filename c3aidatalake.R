library(tidyverse)
library(httr)
library(jsonlite)

# read_data_json directly accesses the C3.ai COVID-19 Data Lake APIs using the requests library, 
# and returns the response as a JSON, raising an error if the call fails for any reason.
# ------
# typename: The type you want to access, i.e. 'OutbreakLocation', 'LineListRecord', 'BiblioEntry', etc.
# api: The API you want to access, either 'fetch' or 'evalmetrics'.
# body: The spec you want to pass. For examples, see the API documentation.
read_data_json <- function(typename, api, body) {
  
  # get the JSON response
  resp <- POST(
    str_c("https://api.c3.ai/covid/api/1/", typename, "/", api),
    body = body %>% toJSON(auto_unbox = TRUE),
    accept("application/json")
  )
  
  # if there was an error, report it
  if (http_error(resp)) {
    stop(
      sprintf(
        "API request failed [%s]\n%s", 
        status_code(resp),
        content(resp)$message
      ),
      call. = FALSE
    )
  }
  
  resp
}

# fetch accesses the C3.ai COVID-19 Data Lake using read_data_json, and converts the response into a Tibble.
# fetch is used for all non-timeseries data in the C3.ai COVID-19 Data Lake, and will call read_data as many times 
# as required to access all of the relevant data for a given typename and body.
# ------
# typename: The type you want to access, i.e. 'OutbreakLocation', 'LineListRecord', 'BiblioEntry', etc.
# body: The spec you want to pass. For examples, see the API documentation.
# get_all: If TRUE, get all records and ignore any limit argument passed in the body. If FALSE, use the limit argument passed in the body. The default is FALSE.
# remove_meta: If TRUE, remove metadata about each record. If FALSE, include it. The default is TRUE.
fetch <- function(typename, body, get_all = FALSE, remove_meta = TRUE) {
  
  # if get_all is TRUE, call fetch until all the data has been accessed
  if(get_all) {
    has_more <- TRUE
    offset <- 0
    df <- NULL
    limit <- 2000
    
    while (has_more) {
      body[["spec"]][["limit"]] <- limit
      body[["spec"]][["offset"]] <- offset
      
      resp <- read_data_json(typename, "fetch", body)
      has_more <- content(resp)$hasMore
      offset <- offset + limit
      df <- bind_rows(df, content(resp)$objs %>% enframe())
    }
  } 
  
  # if get_all is FALSE, just call fetch once
  else {
    resp <- read_data_json(typename, "fetch", body)
    df <- content(resp)$objs %>% 
      enframe()
  }
  
  # reformat the data for easy access
  df <- 
    df %>% 
    dplyr::select(-name) %>% 
    unnest_wider(col = c(value), names_repair = "unique") %>% 
    mutate_at(vars(contains("Date")), parse_datetime)
  
  # remove meta and version if remove_meta is TRUE
  if(remove_meta) {
    df %>% dplyr::select(-meta, -version)
  } else {
    df
  }
}

# evalmetrics accesses the C3.ai COVID-19 Data Lake using read_data_json, and converts the response into a Tibble.
# evalmetrics is used for all timeseries data in the C3.ai COVID-19 Data Lake.
# ------
# typename: The C3.ai Type you want to access, i.e. 'OutbreakLocation', 'LineListRecord', 'BiblioEntry', etc.
# body: The spec you want to pass. For examples, see the API documentation.
# get_all: If TRUE, get all metrics and ignore limits on number of expressions and ids. If FALSE, consider expressions and ids limits. The default is FALSE.
# remove_meta: If TRUE, remove metadata about each record. If FALSE, include it. The default is TRUE.
evalmetrics <- function(typename, body, get_all = FALSE, remove_meta = TRUE) {
  
  # if get_all is TRUE, call EvalMetrics until all the data has been accessed
  # use a loop to bypass limit of 10 IDs and 4 Expressions per call
  if (get_all && ((length(body$spec$ids) > 10) || (length(body$spec$expressions) > 4))) {
    ids_raw <- unlist(body$spec$ids)
    id_groups <- split(ids_raw, ceiling(seq_along(ids_raw)/10))
    expressions_raw <- unlist(body$spec$expressions)
    expression_groups <- split(expressions_raw, ceiling(seq_along(expressions_raw)/4))
    
    df <- NULL
    
    # for each set of IDs and Expressions, get the JSON EvalMetrics response and convert to dataframe
    for (ids in id_groups) {
      for (expressions in expression_groups) {
        body$spec$ids <- as.list(ids)
        body$spec$expressions <- as.list(expressions)
        resp <- read_data_json(typename, "evalmetrics", body)
        df <- bind_rows(df, content(resp)$result %>% enframe())
      }
    }
  } 
  
  # get the JSON EvalMetrics response and convert to dataframe
  else {
    resp <- read_data_json(typename, "evalmetrics", body)
    df <- content(resp)$result %>% enframe()
  }
  
  # reformat the data for easy access
  df <- 
    df %>% 
    unnest_longer(value) %>% 
    unnest_wider(value) %>% 
    unnest(cols = everything()) %>% 
    select_at(vars(!matches("unit"))) %>% 
    mutate_all(unlist) %>% 
    mutate(dates = as.POSIXct(dates))
  
  if(remove_meta) {
    df %>% dplyr::select(-type, -count, -timeZone, -interval, -start, -end)
  } else {
    df
  }
}

# getprojectionhistory accesses the C3.ai COVID-19 Data Lake using read_data_json, and converts the response into a Tibble.
# ------
# body: The API request body you want to pass. For examples, see the API documentation.
# remove_meta: If TRUE, remove metadata about each record. If FALSE, include it. The default is TRUE.
getprojectionhistory <- function(body, remove_meta = TRUE) {
  
  # get the JSON GetProjectionHistory response
  resp <- read_data_json(
    "outbreaklocation", 
    "getprojectionhistory", 
    body
  )
  
  df <- 
    content(resp) %>% 
    enframe() %>% 
    dplyr::select(-name) %>% 
    unnest_wider(value) %>% 
    dplyr::select(-type) %>% 
    unnest_wider(value) %>% 
    pivot_longer(everything()) %>% 
    unnest_wider(value) %>% 
    dplyr::select(-type) %>% 
    unnest_longer(value) %>% 
    unnest_wider(value) %>% 
    unnest(cols = everything()) %>% 
    mutate_all(unlist) %>% 
    mutate(dates = as.POSIXct(dates)) %>% 
    mutate(projection_date = str_remove(expr, ".* ") %>% as.POSIXct())
  
  if(remove_meta) {
    df %>% dplyr::select(-type, -count, -timeZone, -interval, -start, -end)
  } else {
    df
  }
}