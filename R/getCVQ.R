#' Calling and Vocational Questionnaire Function
#'
#' This function returns a dataframe with the CVQ scores for each participant. Users can
#' @param RawDataSet Required DataFrame or Matrix. The R object containing desired items.
#' @param longformat Optional Boolean (defaults FALSE) indicating whether to return the CVQ items in a wide of long format.
#' @param condensed Optional Boolean (defaults TRUE). Indicates whether the scale and item should be combined (e.g., "CVQ18") or in seperate columns (e.g., "CVQ" and "18"). Note that this ONLY applies when longformat = TRUE.
#' @param cutoff Optional Numerical, 0 - 1 (defaults 0.8) Indicates the percentage of required responses be a "qualified" case. For instance, if cutoff = 0.8 then they must have answered at >=80% of the possible items. Enter "0" to include all entries.
#' @param reportmissing Optional Boolean (defaults TRUE). Indicates whether to include "nMissing" and "pMissing" columns to indicate the number and percentage of items missing, respectively.
#' @param reverse Optional Boolean (defaults TRUE). Indicates whether to reverse score the CVQ-8 item or not.
#' @param new_id Optional Boolean (defaults FALSE). Indicates whether to combine year and ID into a new ID formatted as Year_ID.
#' @param drop_id Optional Boolean (defaults FALSE). Indicates whether to drop the original ID column.Note that this ONLY applies when new_id = TRUE.
#' @keywords CVQ
#' @export
#' @examples getCVQ()
#' getCVQ()

#NOTE: Want to add these features:
# 'report' to print metrics pre/post cleaning
# Need to specify version control for required packages
# Sample dataset
# Test functions

getCVQ <- function(RawDataSet,
                   longformat = FALSE,
                   condensed = TRUE,
                   cutoff = .8,
                   reportmissing = TRUE,
                   reverse = TRUE,
                   new_id = FALSE,
                   drop_id = FALSE){

  #Argument validation
  stopifnot(missing(RawDataSet)==FALSE)
  stopifnot(class(RawDataSet)=="data.frame" | class(RawDataSet)=="matrix")
  stopifnot(cutoff >= 0 & cutoff <=1)


  #Requires dplyr and tidyverse to be installed and activated. If either are missing, it installs tidyverse but activates tidyr and dplyr
  if(!(require("tidyr") | require("dplyr") | require("tidyselect"))){
    sp <- suppressPackageStartupMessages()
    suppressPackageStartupMessages(TRUE)
    print("Some required packages are missing. Please wait.")

    #Installs tidyr, if needed
    if(!require("tidyr")){
      install.packages("tidyr")
    }

    #Installs dplyr, if needed
    if(!require("dplyr")){
      install.packages("dplyr")
    }

    if(!require("tidyselect")){
      install.packages("tidyselect")
    }

    #Rechecks that the packages are installed
    if(!(require("tidyr") | require("dplyr")| require("tidyselect"))){
      stop("Some packages could not be loaded. Please install tidyr and dplyr before continuing.")
    }else{
      print("getCVQ NOTE: Required packages have been installed and attached.")
    }
  }

  #Builds the tempDF
  tempDF <- data.frame(RawDataSet)

  #Extracts CVQ items and puts in long format
  tempDF <- tempDF %>% #outDF will eventually be output as the function return
    select(ID,starts_with("CVQ")) %>% #Extracts CVQ and ID columns
    pivot_longer(cols=starts_with("CVQ"), names_to = 'Col', values_to = 'Score')
    # gather(key='Col',value = 'Score', - ID) #Makes it long form (pivotted on ID, 'Col' and 'Score' are new). Later on, this may get reversed based on the WideFormat argument.

  #Creates new variables
  tempDF <- tempDF %>%
    mutate(Year = substring(Col,nchar(Col)-3,nchar(Col)-2))%>% #Extracts year
    mutate(Time = substring(Col,nchar(Col)-1, nchar(Col))) %>% #Extracts Time
    mutate(FullItem = paste("CVQ", substring(Col,4,nchar(Col)-4),sep="")) %>% #Extracts Item (With an "I" prefix)
    select(ID, Year, Time, FullItem, Score) %>% #Final arrangement
    arrange(ID, Time, FullItem) #Sorting

  #####Goes back to wide for this part###
  tempDF <- tempDF %>%
    pivot_wider(id_cols = c(ID, Year, Time), names_from = FullItem, values_from = Score)

  #reverse scores item 8, if requested
  if(reverse == TRUE){
    tempDF$CVQ8_R <-  5 - tempDF$CVQ8 #Creates new variable
    tempDF$CVQ8 <- NULL #Drops the original value
  }

  #Proportion missing (these might get dropped later)
  tempDF <- tempDF %>%
    mutate(nMissing = tempDF %>% select(-c(ID, Year, Time)) %>% is.na %>% rowSums) %>% #Count of missing items
    mutate(pMissing=round((nMissing/24),2)) # Percent of missing items

  #Removes unqualified data, if requested
  if(cutoff!=0){
    tempDF <- tempDF[tempDF$pMissing <= (1-cutoff),] #Drops rows with too much missing data
  }

  #Drops pMissing and nMissing, if requested
  if(reportmissing == FALSE){
    tempDF <- tempDF %>%
      select(-c(nMissing, pMissing))
  }

  #Converts to long format, if requested
  if (longformat == TRUE){
  #If the user wants it returned a long format, it will run this code
    tempDF <- tempDF %>%
      pivot_longer(cols=starts_with("CVQ"), names_to = 'FullItem', values_to = 'Score') #Moves to long format

    #Splits Scale/Item into multiple columns, if requested
    if(condensed == FALSE){
      tempDF <- tempDF %>%
        mutate(Scale = "CVQ") %>% #Puts CVQ in each row
        mutate(Item = substring(FullItem, 4)) %>% #Pulls the item number into this column
        select(ID, Year, Time, Scale, Item, Score,-FullItem) #Drops the condensed column
    }
  }else{
    if(condensed == FALSE){
      print("getCVQ NOTE: condensed = FALSE only works while longformat = TRUE")
    }
  }

  #Creates new ID column, if requested
  if(new_id == TRUE){
    tempDF <- tempDF %>%
      mutate(Year_ID = paste(Year, "_", ID, sep="")) %>% #Combines year and ID into a new column
      select(Year_ID, ID, everything()) #Puts new_id as the first column

    #Drops the ID columns, if requested
    if(drop_id == TRUE){
      tempDF$ID <- NULL #Drops the ID columns
    }
  }else{
    if(drop_id == TRUE){
    print("getCVQ NOTE: drop_id = TRUE only works while new_id = TRUE")
    }
  }

  #Outputs the final product
  getCVQ <- data.frame(tempDF)
}