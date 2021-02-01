
#' @title Tableau Scrape Helpers
#'
#' @description These functions, used in a series, can scrape data from specific
#'   visualizations in Tableau dashboards.
#'
#' @param host_url The host url for a the tableau page you wish to scrape.
#' @param path The path for the host url for the tableau page you wish to
#'   scrape.
#' @param data The tableau datasets extracted from the POST response. This is
#'   the return from \code{tableauScrape_scrapeData()} and an input to
#'   \code{tableauScrape_listWorksheets()} and
#'   \code{tableauScrape_extractData()}
#' @param worksheetID The ID of the particular tableau dataset. These can be
#'   identified with \code{tableauScrape_listWorksheets()}
#' @param print Should the Worksheet ID's be printed when calling
#'   \code{tableauScrape_listWorksheets()}. Default is FALSE
#'
#' @section Origin of These Functions: These functions are almost copy and paste
#'   the same as methodology created by
#'   \href{https://github.com/bertrandmartel}{@bertrandmartel} in their
#'   \href{https://github.com/bertrandmartel/tableau-scraping}{tableau-scraping}
#'   repository. I just cleaned them up a little and formatted them into the
#'   functional pieces that we use most often. The credit goes to
#'   \href{https://github.com/bertrandmartel}{@bertrandmartel} and these
#'   functions are licensed under the MIT license Copyright (c) 2020 Bertrand
#'   Martel.
#'
#'
#' @examples \dontrun{
#' 
#' #Define Host URL and path
#' host_url <- "https://results.mo.gov"
#' path <- "/t/COVID19/views/VaccinationsDashboard/Vaccinations"
#' 
#' 
#' #Scrape all the data
#' scrapedData <- tableauScrape_scrapeData(host_url = host_url, path = path) 
#' 
#' #Find out what data is available 
#' scrapedData_worksheets <- tableauScrape_listWorksheets(scrapedData)
#' 
#' #Pull out the dataset needed
#' extractedData <- tableauScrape_extractData(scrapedData, which(scrapedData_worksheets == 'County - Table'))
#' extractedData
#' }
#'
#'
#' @rdname tableauScrape
#' @export
tableauScrape_scrapeData <- function(host_url, path) {
  
  body <- xml2::read_html(httr::modify_url(host_url, 
                                           path = path, 
                                           query = list(":embed" = "y",":showVizHome" = "no")
  ))
  
  data <- body %>% 
    rvest::html_nodes("textarea#tsConfigContainer") %>% 
    rvest::html_text()
  json <- rjson::fromJSON(data)
  
  url <- httr::modify_url(host_url, path = paste(json$vizql_root, "/bootstrapSession/sessions/", json$sessionid, sep =""))
  
  resp <- httr::POST(url, body = list(sheet_id = json$sheetId), encode = "form")
  data <- httr::content(resp, "text")
  
  extract <- stringr::str_match(data, "\\d+;(\\{.*\\})\\d+;(\\{.*\\})")
  info <- rjson::fromJSON(extract[1,1])
  data <- rjson::fromJSON(extract[1,3])
  
  return(data)
  
}

#' @rdname tableauScrape
#' @export
tableauScrape_listWorksheets <- function(data, print = FALSE) {
  
  worksheets <- names(data$secondaryInfo$presModelMap$vizData$presModelHolder$genPresModelMapPresModel$presModelMap)
  
  if(print) {
    for(i in 1:length(worksheets)){
      cat(paste0(paste0("[",i,"] ",worksheets[i]), sep="\n"))
    }
  }
  
  return(worksheets)
  
}

#' @rdname tableauScrape
#' @export
tableauScrape_extractData <- function(data, worksheetID) {
  
  #Get Worksheet Names
  worksheets <- names(data$secondaryInfo$presModelMap$vizData$presModelHolder$genPresModelMapPresModel$presModelMap)
  worksheet <- worksheets[as.integer(worksheetID)]
  # print(paste("you selected :", worksheet, sep=" "))
  
  
  #Extract Column data for specific worksheet
  columnsData <- data$secondaryInfo$presModelMap$vizData$presModelHolder$genPresModelMapPresModel$presModelMap[[worksheet]]$presModelHolder$genVizDataPresModel$paneColumnsData
  
  i <- 1
  result <- list();
  for(t in columnsData$vizDataColumns){
    if (is.null(t[["fieldCaption"]]) == FALSE) {
      paneIndex <- t$paneIndices
      columnIndex <- t$columnIndices
      if (length(t$paneIndices) > 1){
        paneIndex <- t$paneIndices[1]
      }
      if (length(t$columnIndices) > 1){
        columnIndex <- t$columnIndices[1]
      }
      result[[i]] <- list(
        fieldCaption = t[["fieldCaption"]], 
        valueIndices = columnsData$paneColumnsList[[paneIndex + 1]]$vizPaneColumns[[columnIndex + 1]]$valueIndices,
        aliasIndices = columnsData$paneColumnsList[[paneIndex + 1]]$vizPaneColumns[[columnIndex + 1]]$aliasIndices, 
        dataType = t[["dataType"]],
        stringsAsFactors = FALSE
      )
      i <- i + 1
    }
  }
  dataFull = data$secondaryInfo$presModelMap$dataDictionary$presModelHolder$genDataDictionaryPresModel$dataSegments[["0"]]$dataColumns
  
  cstring <- list();
  for(t in dataFull) {
    if(t$dataType == "cstring"){
      cstring <- t
      break
    }
  }
  data_index <- 1
  name_index <- 1
  frameData <-  list()
  frameNames <- c()
  for(t in dataFull) {
    for(index in result) {
      if (t$dataType == index["dataType"]){
        if (length(index$valueIndices) > 0) {
          j <- 1
          vector <- character(length(index$valueIndices))
          for (it in index$valueIndices){
            vector[j] <- t$dataValues[it+1]
            j <- j + 1
          }
          frameData[[data_index]] <- vector
          frameNames[[name_index]] <- paste(index$fieldCaption, "value", sep="-")
          data_index <- data_index + 1
          name_index <- name_index + 1
        }
        if (length(index$aliasIndices) > 0) {
          j <- 1
          vector <- character(length(index$aliasIndices))
          for (it in index$aliasIndices){
            if (it >= 0){
              vector[j] <- t$dataValues[it+1]
            } else {
              vector[j] <- cstring$dataValues[abs(it)]
            }
            j <- j + 1
          }
          frameData[[data_index]] <- vector
          frameNames[[name_index]] <- paste(index$fieldCaption, "alias", sep="-")
          data_index <- data_index + 1
          name_index <- name_index + 1
        }
      }
    }
  }
  
  df <- NULL
  lengthList <- c()
  for(i in 1:length(frameNames)){
    lengthList[i] <- length(frameData[[i]])
  }
  max <- max(lengthList)
  for(i in 1:length(frameNames)){
    if (length(frameData[[i]]) < max){
      len <- length(frameData[[i]])
      frameData[[i]][(len+1):max]<-""
    }
    df[frameNames[[i]]] <- frameData[i]
  }
  # options(width = 1200)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # print(df)
  
  return(df)
}
