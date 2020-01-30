

#' Companies House - companies_house_collect
#'
#' This function is a wrapper for the Company Profile API end-point from Companies House. Allows the user to collect company profile data by parsing a CompanyNumbers and returning a tabular data-frame as response.
#'
#' All Companies House APIs are accessed by company number. The user can collect company numbers from monthly extracts offered by CompanyHouse that can be accessed \href{http://download.companieshouse.gov.uk/en_output.html}{here}.
#'
#' @param companies A data-frame with CompanyNumber column (see companies data file as example) or a character vector with CompanyNumber.
#' @param api_end_point A character value describing which end-point to call. Can be 'company_profile', 'persons_significant_control', 'officers' or 'charges'. Defaults to 'company_profile'.
#' @param auth_api_key The CompaniesHouse API Key. To generate one please go to \href{https://developer.companieshouse.gov.uk/api/docs/index/gettingStarted/apikey_authorisation.html}{CompaniesHouse Developers Page}.
#' @param time_to_rest In case the API repsonse is 429 ('Too Many Requests') the user can select the period to wait until it tries again. It defaults to 3 seconds.
#' @param items_per_page The items per page request to try and return (defaults to 1000).
#' @param join A boolean value that when TRUE whill return the results joined with the original df parsed. It only applies when companies argument is a data.frame. Defaults to TRUE.
#' @param verbose Boolean value set to TRUE to print the status of each API call.
#'
#' @return A list with two elements:
#' \itemize{
#'  \item{\strong{results_df}} {A data-frame with the results returned from the 'companies/' end point of the CompaniesHouse API. The results can be explored via \href{https://developer.companieshouse.gov.uk/api/docs/company/company_number/companyProfile-resource.html}{this link}.}
#'  \item{\strong{errorLogs}}  {In case some CompanyNumbers failed to return values either because of errors or because there was no results to be returned, these can be explored via errorLogs.}
#' }
#'
#' @import magrittr
#' @export
#'
#' @examples
#' \dontrun{
#' companies_house_collect(companies, auth_api_key , time_to_rest = 3)
#'}


companies_house_collect = function(companies, api_end_point = "company_profile", auth_api_key, time_to_rest = 3, items_per_page = 1000, join = TRUE, verbose = TRUE){


  if(!api_end_point %in% c("company_profile", "persons_significant_control", "officers", "charges")){
    stop("api_end_point must be either 'company_profile', 'persons_significant_control', 'officers', 'charges'")
  }

  url_f = "https://api.companieshouse.gov.uk"
  final_df = list()
  errorLogs = list()
  api_call_types_df = api_end_point_map_df()
  if(!('list' %in% class(auth_api_key))){
    auth_keys_list = list(auth_api_key)
  }else{
    auth_keys_list = auth_api_key
  }

  total_keys = length(auth_keys_list)


  if("data.frame" %in% class(companies)){
      companies_numbers = unique(companies$CompanyNumber)[!is.na(unique(companies$CompanyNumber))]
  }else if(class(companies) %in% c("character", "numeric")){companies_numbers = as.character(unique(companies[!is.na(companies)]))}

  API_responses = api_codes_lookup()

  current_key_position = 1
  i = 1

  #infinite loop:
  while(TRUE){

    if(i > length(companies_numbers)){break()}

    path_f = sprintf(api_call_types_df$end_point[api_call_types_df$api_end_point_type == api_end_point], companies_numbers[i], items_per_page)
    results_all <- httr::GET(url = url_f, path = path_f, httr::add_headers(Host = "api.companieshouse.gov.uk", Authorization = auth_keys_list[[current_key_position]]))

    if(i == 1){cat(paste0("Collecting Companies House Data (Current API Limit: ", results_all$headers$`x-ratelimit-remain`, " requests)...\n"))}

    # if(httr::http_type(results_all) != "application/json"){next("API did not return json", call. = FALSE)}
    if(results_all$status_code == 200){

      results_content = rawToChar(results_all$content)

      tryCatch(
        expr = {
          final_df[[i]] = fromJSONtoDF(jsonlite::fromJSON(results_content), api_end_point = api_end_point, company_number = companies_numbers[i])
          if(verbose){cat(paste0(i," - ", nrow(final_df[[i]]), " Company Profile Information record for CompanyNumber  ", companies_numbers[i], " was/were colected successfully \n"))}

        },
        error = function(e){
          cat(paste0(i," - ", "Data-processing error: ", e, companies_numbers[i], ".\n"))

          errorLogs[[i]] = data.frame(ResponseCode = 999,
                                 API_Response = "data-processing error due to api not returning a particular field",
                                 CompanyNumber = companies_numbers[i],
                                 Error_Time = Sys.time(),
                                 stringsAsFactors = FALSE)

        }
      )

      i = i + 1
      next()

    }else if(results_all$status_code == 429 | results_all$status_code == 403){

      if(current_key_position < total_keys){
        current_key_position = current_key_position + 1
        if(verbose){cat(paste0("\nSwitching API keys to key ", current_key_position, "...\n"))}
        Sys.sleep(time_to_rest)
      }else{
        current_key_position = 1
        if(verbose){cat(paste0("\nSwitching API keys to key ", current_key_position, "...\n"))}
        Sys.sleep(time_to_rest)
        if(verbose){cat(paste0("\nToo many requests. ", time_to_rest, " seconds until next try ...\n"))}
      }
      next()

    }else{

      if(verbose){cat(paste0(i," - ", "An error occured with an API Response code: ", results_all$status_code, ". Skipping CompanyID ", companies_numbers[i], ".\n"))}


      error_log = data.frame(ResponseCode = results_all$status_code,
                             CompanyNumber = companies_numbers[i],
                             Error_Time = Sys.time(),
                             stringsAsFactors = FALSE)

      errorLogs[[i]] = error_log %>%
        dplyr::inner_join(API_responses, by = c("ResponseCode" = "Code")) %>%
        dplyr::select(ResponseCode, API_Response, CompanyNumber, Error_Time)

      i = i + 1
      next()
    }

  }

  #formatting the final results:

  if(join && ("data.frame" %in% class(companies)) && nrow(dplyr::bind_rows(final_df)) > 0 ){

    final_results_df = companies %>%
      dplyr::left_join(dplyr::select(dplyr::bind_rows(final_df), -dplyr::contains("CompanyName")), by = "CompanyNumber")

  }else{

    final_results_df = final_df %>% dplyr::bind_rows()
  }


  final_results = list(results_df = final_results_df,
                       errorLogs = errorLogs %>% dplyr::bind_rows())

  cat(paste0("\nA total of ", nrow(final_results$results_df), " rows were returned for ", suppressWarnings(length(unique(final_results$results_df$CompanyNumber))), " companies. These results are stored on 'results_df' data-frame. \nThere are ", nrow(final_results$errorLogs), " companies that produced errors or didn't return information and therefore no information was collected. The details on these errors can be found on 'errorLogs'."))
  return(final_results)

}
