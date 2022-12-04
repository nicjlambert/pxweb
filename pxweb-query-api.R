#' Author: nicholas.lambert
#' Description: Download and tidy data from IRENA API
#' (1) The for loop enables multiple API calls. And uses 'Technology' for the
#'     pagination of API requests (see note 1)
#' (2) The data from each call is written to an outfile
#' (3) The data returned for each iteration is unioned and written to csv

#' Notes:
#' 1. The API is limited to 100,000 values per call
#' 2. API documentation: https://www.scb.se/api_en/
#' 3. Year columns (2000, 2001,...,n) are unpivoted into
#'    separate row values with the column title 'Year'
#' 4. The data a tidyied such that each column is a variable and each row an
#'    obersation.

packages <- c('httr'    # tools for working with URLs and HTTP
            , 'tidyr'   # tidy messy data
            , 'dplyr')  # a grammar of data manipulation

packages.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE, repos = "https://cran.csiro.au")
      library(x, character.only = TRUE)
    }
  }
)

#' get_data
#'
#' @param curYear a chr of the current year
#' @param baseURL a chr of the URL used for download
#' @param tbl a chr of the shortened IRENA database table name
#'
#' @returns a data.frame containing the data or NULL when the data cannot be found
#'          a csv containing the data or NULL when the data cannot be found
#' @export
#'
#' @examples
#'
#' get_data(
#'  curYear = substr(Sys.Date(), 0, 4),
#'  baseURL = 'https://pxweb.irena.org:443/api/v1/en/IRENASTAT/Power Capacity and Generation/',
#'  tbl = 'RECAP'
#'  )
#'

curYear <- substr(Sys.Date(), 0, 4)
tbl <- 'RECAP'
baseURL <-  'https://pxweb.irena.org:443/api/v1/en/IRENASTAT/Power Capacity and Generation/'

get_data <- function(curYear, baseURL, tbl, cycle_nbr) {

  # Specify the url to the API endpoint
  url <-
    paste0(baseURL, tbl, '_', curYear, '_cycle', cycle_nbr, '.px')
  
  message(paste0(
    'URL: ',
    url,
    '\n\nPOST API query sent as JSON towards the URL:\n\n'
  ))

  # Create a vector of values for the "technology" attribute
  technologies <- 0:18

  outfiles <- list()
  
  for (technology in technologies) {

    # Create a query object with the "technology" attribute and its value
    q <-
      paste0(
        '{"query":[{"code":"Technology","selection":{"filter":"item","values":["',
        i ,
        '"]}}],"response":{"format":"csv"}}'
      )
    
    message(q)

    # POST the query to the API endpoint
    r <- httr::POST(URLencode(url), body = q )

    out <- read.csv(textConnection(content(r, 'text')))
    
    Sys.sleep(5)
    
    names(out) <- sub("^X", "", names(out))
    
    out <- out %>%

      dplyr::mutate_all(as.character) %>%
      
      tidyr::pivot_longer(
        !c(Region.country.area, Technology),
        names_to = "Year",
        values_to = "Observation_Value"
      ) %>%
      dplyr::mutate(
        Observation_Value = na_if(Observation_Value, ".."),
        Publication_Cycle = paste0('cycle', cycle_nbr),
        Publication_Year = curYear
      ) %>%
      dplyr::rename(Region_Country_Area = Region.country.area) %>%
      dplyr::select(
        Region_Country_Area,
        Technology,
        Year,
        Publication_Cycle,
        Publication_Year,
        Observation_Value
      )
    
    outfiles[[q]] <- out
    
  }

  # Convert list of dataframes to a single dataframe
  df <- as.data.frame(do.call(rbind, outfiles), row.names = FALSE)
  df$Region_Country_Area <- gsub("[/*]", "", df$Region_Country_Area)

  # Write the combined results dataframe to a csv file
  write.csv(
    df,
    paste0(
      'IRENA_RECAP_',
      curYear,
      '_cycle_',
      cycle_nbr,
      '_',
      Sys.Date(),
      '.csv'
    ),
    fileEncoding = 'UTF-8',
    row.names = FALSE
  )
  
  message('\nDone!')
}


for (i in 1:2) {

  possibleError <- tryCatch(
    
    get_data(
      baseURL = baseURL,
      tbl = tbl,
      curYear = curYear,
      cycle_nbr = i
    )
  
  ,
  error=function(e) {
    e
    print(
      paste('Oops! -> Error in finding publication cycle', 
            i, 
            '. Trying cycle', i+1, '...' ), quote=FALSE)
  }
  )
  
  if(inherits(possibleError, 'error')) next
  
  print(paste('End of loop', i ), quote = FALSE)
  
}
