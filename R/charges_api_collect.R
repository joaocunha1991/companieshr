


library(httr)
library(jsonlite)
library(tidyverse)
library(googleComputeEngineR)
library(googleCloudStorageR)
library(ggthemes)
library(tictoc)

#' Companies House - Charges Collector
#'
#' This function is a wrapper for the Charges API from Companies House. Allows the user to collect Charges data by parsing a company number.
#'
#' All Companies House APIs are accessed by company number. The user can collect company numbers from \link(companieshouse.com).
#'
#'
#' @param filename A character value representing the filename to import.
#' @return A tibble dataframe representing the file \code{filename} loaded.
#'
#' @import magrittr
#'
#' @examples
#' \dontrun{
#' fars_read("accident_2013.csv.bz2")
#'}



####################
# TASK START HERE: #
####################

FunctionIfNullNA = function(y){
  if(is.null(y))
    return(NA)
  if(length(y) == 0)
    return(NA)
  return(y)
}


functionFromJSONtoDF_charges = function(json_list){

  cols = setdiff(c("created_on", "delivered_on", "status", "charge_number", "persons_entitled", "satisfied_on"), colnames(json_list$items))

  json_list$items[,cols] = NA

  json_list$items %>% dplyr::select(created_on, delivered_on, status, charge_number, persons_entitled, satisfied_on)

}

#PULL API RESPONSE LOOKUP TABLE:
API_responses = read_csv("~/odrive/Google Drive - Personal/Personal/Project Bob /API_ResponseCodes.csv")



##########################################
## ALTERNATIVE FUNCTION - INFINITE LOOP ##
##########################################


charges_api_collect = function(companies_df, auth_api_key, time_to_rest = 3){

  url_f = "https://api.companieshouse.gov.uk"
  ids = unique(companies_df$CompanyNumber)
  final_df = list()
  errorLogs = list()
  auth_keys = auth_api_key

  API_responses = api_codes_lookup()

  i = 1

  while(TRUE){

    if(is.na(company_numbers[i])){break()}

    path_f = sprintf("/company/%s/charges?items_per_page=100000", companies_df$CompanyNumber[i])
    results_all <- httr::GET(url = url_f, path = path_f, add_headers(Host = "api.companieshouse.gov.uk", Authorization = auth_keys))

    print(results_all$headers$`x-ratelimit-remain`)

    if(results_all$status_code == 200){

      results_content = rawToChar(results_all$content)

      df_response = functionFromJSONtoDF_charges(fromJSON(results_content)) %>%
        dplyr::mutate(CompanyNumber = companies_df$CompanyNumber[i],
                      Charges_PersonsEntitled = sapply(persons_entitled, function(x){paste(x$name, collapse = " &&& ")}))

      final_df[[i]] = companies_df %>%
        dplyr::inner_join(df_response, by = "CompanyNumber")

      cat(paste0(nrow(final_df[[i]]), " Charges for Company Number", companies_df$CompanyNumber[i], ") were colected successfully !\n"))

      i = i + 1
      next()

    }else if(results_all$status_code == 429){

      Sys.sleep(time_to_rest)
      cat(paste0("Too many requests. ", time_to_rest, " seconds until next try ..."))
      next()

    }else{

      print(paste0("An error occured with an API Response code: ", results_all$status_code, ". Skipping CompanyID ", companies_df$CompanyNumber[i], "."))

      error_log = data_frame(ResponseCode = results_all$status_code,
                             CompanyNumber = companies_df$CompanyNumber[i],
                             Error_Time = Sys.time())

      errorLogs[[i]] = error_log %>%
        dplyr::inner_join(API_responses, by = c("ResponseCode" = "Code")) %>%
        dplyr::select(ResponseCode, API_Response, CompanyNumber, Error_Time)

      i = i + 1
      next()
    }

  }

  final_results = list(results_charges = final_df %>% dplyr::bind_rows(),
                       errorLogs = errorLogs %>% dplyr::bind_rows())

  cat(paste0("A total of ", nrow(final_results$results_charges), " charges were collected for ", length(unique(final_results$results_charges$CompanyNumber)), " companies. These results are stored on 'results_charges' data-frame. \n", nrow(final_results$results_charges), " companies produced errors and therefore no information was collected. The details on these errors can be found on ", final_results$errorLogs))
  return(final_results)

}

tic("Testing 3000 requests ...")
response_test1 = functionPull_Charges_alt(new_companies[1:100,])
toc()

######################################################################
##  REPLICATE FUNCTION FOR PEOPLES WITH SIGNIFICANT CONTROL SECTION ##
######################################################################

#try catch adapted function for exeptions when the column is not returned from the API:
tryFunction = function(vec){

  tryCatch({
    vec
  }, error = function(e){

    NA
  })
}


#Function that accesses the API response for 'People With Significant Control' and scrapes the json to extract the relevant information:
#Address, date_of_birth and identification columns are data.frames and therefore need a different approach:

functionFromJSONtoDF_persons = function(json_list){

  cols = setdiff(c("notified_on", "ceased_on", "natures_of_control", "country_of_residence", "nationality", "kind", "date_of_birth", "address", "name", "identification"), colnames(json_list$items))

  json_list$items[,cols] = NA

  address_df = data.frame(json_list$items$address) %>%
    dplyr::mutate(Person_SigControl_Locality = tryFunction(locality),
                  Person_SigControl_PostalCode = tryFunction(postal_code),
                  Person_SigControl_Premises = tryFunction(premises),
                  Person_SigControl_AddressLine1 = tryFunction(address_line_1),
                  Person_SigControl_Country = tryFunction(country),
                  Person_SigControl_Region = tryFunction(region),
                  Person_SigControl_AddressLine2 = tryFunction(address_line_2)) %>%
    dplyr::select(Person_SigControl_AddressLine1,
                  Person_SigControl_AddressLine2,
                  Person_SigControl_Premises,
                  Person_SigControl_Locality,
                  Person_SigControl_Region,
                  Person_SigControl_Country,
                  Person_SigControl_PostalCode)

  dob_df = data.frame(json_list$items$date_of_birth) %>%
    dplyr::mutate( Person_SigControl_DOB_Year = tryFunction(year),
                   Person_SigControl_DOB_Month = tryFunction(month)) %>%
    dplyr::select(Person_SigControl_DOB_Year,
                  Person_SigControl_DOB_Month)

  identification_df = data.frame(json_list$items$identification) %>%
    dplyr::mutate( Person_SigControl_CountryRegistered = tryFunction(country_registered),
                   Person_SigControl_PlaceRegistered = tryFunction(place_registered),
                   Person_SigControl_RegNumber = tryFunction(registration_number),
                   Person_SigControl_LegalForm = tryFunction(legal_form),
                   Person_SigControl_LegalAuthority = tryFunction(legal_authority)) %>%
    dplyr::select(Person_SigControl_CountryRegistered,
                  Person_SigControl_PlaceRegistered,
                  Person_SigControl_RegNumber,
                  Person_SigControl_LegalForm,
                  Person_SigControl_LegalAuthority)



  df = data.frame( Person_SigControl_Name = json_list$items$name,
                   Person_SigControl_NotifiedOn = json_list$items$notified_on,
                   Person_SigControl_CeasedOn = json_list$items$ceased_on,
                   Person_SigControl_NaturesOfControl = tryFunction(sapply(json_list$items$natures_of_control, function(x){paste0(x, collapse = " & ")})),
                   Person_SigControl_CountryOfResidence = json_list$items$country_of_residence,
                   Person_SigControl_Nationality =  json_list$items$nationality,
                   Person_SigControl_Kind = json_list$items$kind) %>%
    dplyr::mutate(Person_SigControl_CurrentStatus = ifelse(is.na(Person_SigControl_CeasedOn), "Active", "Ceased")) %>%
    dplyr::bind_cols(address_df, dob_df, identification_df)

  return(df)
}

#Main function that collects the data and keeps a log of errors:
functionPeople = function(companies_df){

  url_f = "https://api.companieshouse.gov.uk"
  ids = unique(companies_df$CompanyNumber)
  final_df = list()
  errorLogs = list()
  auth_keys = c("XIStesic-Ojpcp2g5LFD-PTcgJONR5tmO3XrSIxt", "iPTG8nQ-fMWpZVUakL1gjUasfk7oytyNRxaDMphA")
  i = 1

  while(TRUE){

    if(is.na(companies_df$CompanyNumber[i])){break()}
    path_f = sprintf("/company/%s/persons-with-significant-control?items_per_page=100000", companies_df$CompanyNumber[i])
    results_all <- GET(url = url_f, path = path_f, add_headers(Host = "api.companieshouse.gov.uk", Authorization= auth_keys[1]))
    print(results_all$headers$`x-ratelimit-remain`)

    if(results_all$status_code == 200){

      results_content = rawToChar(results_all$content)

      df_response = functionFromJSONtoDF_persons(fromJSON(results_content)) %>%
        dplyr::mutate(CompanyNumber = companies_df$CompanyNumber[i],
                      CompanyName = companies_df$CompanyName[i]) %>%
        dplyr::select(CompanyName, CompanyNumber, starts_with("Person_SigControl_"))

      final_df[[i]] = df_response

      print(paste0(nrow(final_df[[i]]), " records of Persons Info for Company ", companies_df$CompanyName[i], " (ID ", companies_df$CompanyNumber[i], ") were colected !"))

      i = i + 1
      next()

    }else if(results_all$status_code == 429){

      Sys.sleep(3)
      print("Too many requests ...")
      next()

    }else{

      print(paste0("API Response code: ", results_all$status_code, ". Skipping CompanyID ", companies_df$CompanyNumber[i], "."))

      error_log = data_frame(ResponseCode = results_all$status_code,
                             CompanyName = companies_df$CompanyName[i],
                             CompanyNumber = companies_df$CompanyNumber[i],
                             Time = Sys.time())

      errorLogs[[i]] = error_log %>% dplyr::inner_join(API_responses, by = c("ResponseCode" = "Code"))

      i = i + 1
      next()
    }

  }

  list(results = final_df %>% dplyr::bind_rows(),
       errorLogs = errorLogs %>% dplyr::bind_rows())

}


tic()
test = functionPeople(dplyr::distinct(bob[1:500,], CompanyName, CompanyNumber))
toc()


##############################################
##  REPLICATE FUNCTION FOR OFFICERS SECTION ##
##############################################

functionFromJSONtoDF_officers = function(json_list){

  cols = setdiff(c("appointed_on", "officer_role", "country_of_residence", "nationality", "kind", "date_of_birth", "address", "name", "occupation", "resigned_on"), colnames(json_list$items))

  json_list$items[,cols] = NA

  address_df = data.frame(json_list$items$address) %>%
    dplyr::mutate(Officer_Locality = tryFunction(locality),
                  Officer_PostalCode = tryFunction(postal_code),
                  Officer_Premises = tryFunction(premises),
                  Officer_AddressLine1 = tryFunction(address_line_1),
                  Officer_Country = tryFunction(country),
                  Officer_Region = tryFunction(region),
                  Officer_AddressLine2 = tryFunction(address_line_2)) %>%
    dplyr::select(Officer_AddressLine1,
                  Officer_AddressLine2,
                  Officer_Premises,
                  Officer_Locality,
                  Officer_Region,
                  Officer_Country,
                  Officer_PostalCode)

  dob_df = data.frame(json_list$items$date_of_birth) %>%
    dplyr::mutate( Officer_DOB_Year = tryFunction(year),
                   Officer_DOB_Month = tryFunction(month)) %>%
    dplyr::select(Officer_DOB_Year,
                  Officer_DOB_Month)


  df = data.frame( Officer_Name = json_list$items$name,
                   Officer_AppointedOn = json_list$items$appointed_on,
                   Officer_ResignedOn = json_list$items$resigned_on,
                   Officer_OfficerRole = json_list$items$officer_role,
                   Officer_Occupation = json_list$items$occupation,
                   Officer_CountryOfResidence = json_list$items$country_of_residence,
                   Officer_Nationality =  json_list$items$nationality) %>%
    dplyr::mutate(Officer_CurrentStatus = ifelse(is.na(Officer_ResignedOn), "Active", "Ceased")) %>%
    dplyr::bind_cols(address_df, dob_df)

  return(df)
}



functionOfficers = function(companies_df){

  url_f = "https://api.companieshouse.gov.uk"
  ids = unique(companies_df$CompanyNumber)
  final_df = list()
  errorLogs = list()
  auth_keys = c("XIStesic-Ojpcp2g5LFD-PTcgJONR5tmO3XrSIxt", "iPTG8nQ-fMWpZVUakL1gjUasfk7oytyNRxaDMphA")
  i = 1

  while(TRUE){

    if(is.na(companies_df$CompanyNumber[i])){break()}
    path_f = sprintf("/company/%s/officers?items_per_page=100000", companies_df$CompanyNumber[i])
    results_all <- GET(url = url_f, path = path_f, add_headers(Host = "api.companieshouse.gov.uk", Authorization= auth_keys[1]))
    print(results_all$headers$`x-ratelimit-remain`)

    if(results_all$status_code == 200){

      results_content = rawToChar(results_all$content)

      df_response = functionFromJSONtoDF_officers(fromJSON(results_content)) %>%
        dplyr::mutate(CompanyNumber = companies_df$CompanyNumber[i],
                      CompanyName = companies_df$CompanyName[i]) %>%
        dplyr::select(CompanyName, CompanyNumber, starts_with("Officer_"))

      final_df[[i]] = df_response

      print(paste0(nrow(final_df[[i]]), " records of Officers for Company ", companies_df$CompanyName[i], " (ID ", companies_df$CompanyNumber[i], ") were colected !"))

      i = i + 1
      next()

    }else if(results_all$status_code == 429){

      Sys.sleep(3)
      print("Too many requests ...")
      next()

    }else{

      print(paste0("API Response code: ", results_all$status_code, ". Skipping CompanyID ", companies_df$CompanyNumber[i], "."))

      error_log = data_frame(ResponseCode = results_all$status_code,
                             CompanyName = companies_df$CompanyName[i],
                             CompanyNumber = companies_df$CompanyNumber[i],
                             Time = Sys.time())

      errorLogs[[i]] = error_log %>% dplyr::inner_join(API_responses, by = c("ResponseCode" = "Code"))

      i = i + 1
      next()
    }

  }

  list(results = final_df %>% dplyr::bind_rows(),
       errorLogs = errorLogs %>% dplyr::bind_rows())

}


tic()
test_officers =  functionOfficers(dplyr::distinct(bob[1:500,], CompanyName, CompanyNumber))
toc()

