

#' Companies House - companies_profile_collect
#'
#' This function is a wrapper for the Company Profile API end-point from Companies House. Allows the user to collect company profile data by parsing a CompanyNumbers and returning a tabular data-frame as response.
#'
#' All Companies House APIs are accessed by company number. The user can collect company numbers from monthly extracts offered by CompanyHouse that can be accessed \href{http://download.companieshouse.gov.uk/en_output.html}{here}.
#'
#' @param companies_df A data-frame with CompanyNumber column (see companies data file as example).
#' @param auth_api_key The CompaniesHouse API Key. To generate one please go to \href{https://developer.companieshouse.gov.uk/api/docs/index/gettingStarted/apikey_authorisation.html}{CompaniesHouse Developers Page}.
#' @param time_to_rest In case the API repsonse is 429 ('Too Many Requests') the user can select the period to wait until it tries again. It defaults to 3 seconds.
#' @param items_per_page The items per page request to try and return (defaults to 1000).
#' @param join A boolean value that when TRUE whill return the results joined with the original df parsed. Defaults to TRUE.
#'
#' @return A list with two elements:
#' \itemize{
#'  \item{\strong{results_df}} {A data-frame with the results returned from the 'companies/' end point of the CompaniesHouse API. The results can be explored via \href{https://developer.companieshouse.gov.uk/api/docs/company/company_number/companyProfile-resource.html}{this link}.}
#'  \item{\strong{errorLogs}}  {In case some CompanyNumbers failed to return values either because of errors or because there was no results to be returned, these can be explored via errorLogs.}
#' }
#'
#' @import magrittr
#'
#' @examples
#' \dontrun{
#' companies_api_collect(companies_df, auth_api_key , time_to_rest = 3)
#'}

#To write:

#A general errorHandling function to be called in all the API endpoint wrappers (argument controls)

companies_profile_collect = function(companies_df, auth_api_key, time_to_rest = 3, items_per_page = 1000, join = TRUE ){

  url_f = "https://api.companieshouse.gov.uk"
  final_df = list()
  errorLogs = list()
  auth_keys = auth_api_key

  if("data.frame" %in% class(companies_df)){
      companies_numbers = unique(companies_df$CompanyNumber)[!is.na(unique(companies_df$CompanyNumber))]
  }else if(class(companies_df) %in% c("character", "numeric")){companies_numbers = as.character(unique(companies_df[!is.na(companies_df)]))}

  API_responses = api_codes_lookup()

  i = 1

  while(TRUE){

    if(i > length(companies_numbers)){break()}

    path_f = sprintf("/company/%s/?items_per_page=%s", companies_numbers[i], items_per_page)
    results_all <- httr::GET(url = url_f, path = path_f, httr::add_headers(Host = "api.companieshouse.gov.uk", Authorization = auth_keys))

    if(i == 1){cat(paste0("Current Limit: ", results_all$headers$`x-ratelimit-remain`, " requests\n"))}

    # if(httr::http_type(results_all) != "application/json"){next("API did not return json", call. = FALSE)}
    if(results_all$status_code == 200){

      results_content = rawToChar(results_all$content)

      final_df[[i]] = fromJSONtoDF_companies_base(jsonlite::fromJSON(results_content)) %>%
                      dplyr::rename(CompanyNumber = company_number,
                                    CompanyName = company_name) %>%
                      dplyr::select(CompanyNumber, CompanyName, dplyr::everything())

      cat(paste0(nrow(final_df[[i]]), " Company Profile Information record for CompanyNumber  ", companies_numbers[i], " was/were colected successfully \n"))

      i = i + 1
      next()

    }else if(results_all$status_code == 429){

      Sys.sleep(time_to_rest)
      cat(paste0("\nToo many requests. ", time_to_rest, " seconds until next try ...\n"))
      next()

    }else{

      cat(paste0("\nAn error occured with an API Response code: ", results_all$status_code, ". Skipping CompanyID ", companies_numbers[i], ".\n"))


      error_log = data.frame(ResponseCode = results_all$status_code,
                             CompanyNumber = companies_numbers[i],
                             Error_Time = Sys.time())

      errorLogs[[i]] = error_log %>%
        dplyr::inner_join(API_responses, by = c("ResponseCode" = "Code")) %>%
        dplyr::select(ResponseCode, API_Response, CompanyNumber, Error_Time)

      i = i + 1
      next()
    }

  }

  #formatting the final results:
  if(join & ("data.frame" %in% class(companies_df))){

    final_results_df = companies_df %>%
      dplyr::left_join(dplyr::bind_rows(final_df), by = "CompanyNumber")

  }else{

    final_results_df = final_df %>% dplyr::bind_rows()
  }


  final_results = list(results_df = final_results_df,
                       errorLogs = errorLogs %>% dplyr::bind_rows())

  cat(paste0("\nA total of ", nrow(final_results$results_df), " rows were returned for ", length(unique(final_results$results_df$CompanyNumber)), " companies. These results are stored on 'results_df' data-frame. \nThere are ", nrow(final_results$errorLogs), " companies that produced errors or didn't return information and therefore no information was collected. The details on these errors can be found on 'errorLogs'."))
  return(final_results)

}
