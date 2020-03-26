#' Calling and Vocational Questionnaire Function
#'
#' This function returns a dataframe with the CVQ scores for each participant. Users can
#' @param RawDataSet Required DataFrame or Matrix. The R object containing desired items.
#' @param longformat Optional Boolean (defaults FALSE) indicating whether to return the CVQ items in a wide of long format.
#' @param condensed Optional Boolean (defaults TRUE). Indicates whether the scale and item should be combined (e.g., "CVQ18") or in seperate columns (e.g., "CVQ" and "18"). Note that this ONLY applies when longformat = TRUE.
#' @param cutoff Optional Numerical, 0 - 1 (defaults 0.8) Indicates the percentage of required responses be a "qualified" case. For instance, if cutoff = 0.8 then they must have answered at >=80 percent of the possible items. Enter "0" to include all entries.
#' @param reportmissing Optional Boolean (defaults TRUE). Indicates whether to include "nMissing" and "pMissing" columns to indicate the number and percentage of items missing, respectively.
#' @param new_id Optional Boolean (defaults FALSE). Indicates whether to combine year and ID into a new ID formatted as Year_ID.
#' @param drop_id Optional Boolean (defaults FALSE). Indicates whether to drop the original ID column.Note that this ONLY applies when new_id = TRUE.
#' @param reverse Optional Boolean (defaults TRUE). Indicates whether to reverse score the CVQ-8 item or not.
#' @param subscales Optional numeric (defaults '0'). Indicates the subscales to calculate. Valid options are 0, 2, 6, or 8. The 8-factor option simply returns both the 2- and 6-factor scales. The 0 option doesn't return any subscales. Be aware that item 8 MUST be reverse scored to calculate correctly; you may use "Reverse = TRUE" to do it here, or leave "Reverse = FALSE" if you did it before importing the data. See Dik et al (2012) for details on scoring.
#' @keywords CVQ
#' @export
#' @examples getCVQ()

#NOTE: Want to add these features:
# 'report' to print metrics pre/post cleaning
# Need to specify version control for required packages
# Sample dataset
# Test functions
# 'Factor' argument to compute factor scores (might need multiple string options depending on whether they want the 2- or 3-factor scale)
# 'Scale' argument to compute a total scale score

getCVQ <- function(RawDataSet,
                   longformat = FALSE,
                   condensed = TRUE,
                   cutoff = .8,
                   reportmissing = TRUE,
                   new_id = FALSE,
                   drop_id = FALSE,
                   reverse = TRUE,
                   subscales = 0){

  #Argument validation
  stopifnot(missing(RawDataSet)==FALSE)
  stopifnot(class(RawDataSet)=="data.frame" | class(RawDataSet)=="matrix")
  stopifnot(cutoff >= 0 & cutoff <=1)
  stopifnot(subscales == 0 | subscales == 2 | subscales == 6 | subscales == 8)


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

  #Calculates subscale scores, if requested
  if(subscales != 0){
    print("NOTE: The subscales assume that CVQ8 has been reverse scored. If needed, you can include the argument Reverse = TRUE.")

    #Creates all 8 scales

      #Adds the correct CVQ8 to CVQ_Pres_TranSummons
      if(sum(colnames(tempDF)=="CVQ8_R")==1){
        #uses CVQ8_R
        tempDF <- tempDF %>%
          mutate(CVQ_Pres_TranSummons = CVQ1 + CVQ8_R + CVQ11 + CVQ23)
      }else{
        #uses CVQ8
        tempDF <- tempDF %>%
          mutate(CVQ_Pres_TranSummons = CVQ1 + CVQ8 + CVQ11 + CVQ23)
      }

      #Adds the other 7 scales
      tempDF <- tempDF %>%
        mutate(CVQ_Pres_PurpWork = CVQ3 + CVQ15 + CVQ20 + CVQ24) %>%
        mutate(CVQ_Pres_ProSoc = CVQ9 + CVQ12 + CVQ17 + CVQ22) %>%
        mutate(CVQ_Search_TranSummons = CVQ2 + CVQ13 + CVQ18 + CVQ19) %>%
        mutate(CVQ_Search_PurpWork = CVQ4 + CVQ6 + CVQ14 + CVQ21) %>%
        mutate(CVQ_Search_ProSoc = CVQ5 + CVQ7 + CVQ10 + CVQ16) %>%
        mutate(CVQ_Presence = CVQ_Pres_TranSummons + CVQ_Pres_PurpWork + CVQ_Pres_ProSoc) %>%
        mutate(CVQ_Searching = CVQ_Search_TranSummons + CVQ_Search_PurpWork + CVQ_Search_ProSoc)



    if(subscales == 2){
      #Drops the six scales
      tempDF <- tempDF %>%
        select(-c(CVQ_Pres_TranSummons,
                  CVQ_Pres_PurpWork,
                  CVQ_Pres_ProSoc,
                  CVQ_Search_TranSummons,
                  CVQ_Search_PurpWork,
                  CVQ_Search_ProSoc))
      }

    if(subscales == 6){
      #Drops the two scales
      tempDF <- tempDF %>%
        select(-c(CVQ_Searching,
                  CVQ_Presence))
      }

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
