

#' Companies House - companies_profile_collect
#'
#' This function is a wrapper for the Charges API from Companies House. Allows the user to collect Charges data by parsing a company number.
#'
#' All Companies House APIs are accessed by company number. The user can collect company numbers from monthly extracts offered by CompanyHouse that can be accessed \href{http://download.companieshouse.gov.uk/en_output.html}{here}.
#'
#' @param companies_df A data-frame with CompanyNumber column (see companies data file as example).
#' @param auth_api_key The CompaniesHouse API Key. To generate one please go to \href{https://developer.companieshouse.gov.uk/api/docs/index/gettingStarted/apikey_authorisation.html}{CompaniesHouse Developers Page}.
#' @param time_to_rest In case the API repsonse is 429 ('Too Many Requests') the user can select the period to wait until it tries again. It defaults to 3 seconds.
#'
#' @return A list with two elements:
#' \itemize{
#'  \item{\strong{results_df}} {A data-frame with the results returned from the 'companies/ end point of the CompaniesHouse API. The results can be explored via \href{https://developer.companieshouse.gov.uk/api/docs/company/company_number/companyProfile-resource.html}{this link}.}
#'  \item{\strong{errorLogs}}  {In case some CompanyNumbers failed to return values either because of errors or because there was no results to be returned, these can be explored via errorLogs.}
#' }
#'
#' @import magrittr
#'
#' @examples
#' \dontrun{
#' companies_api_collect(companies_df, auth_api_key , time_to_rest = 3)
#'}


companies_profile_collect = function(companies_df, auth_api_key, time_to_rest = 3){

  url_f = "https://api.companieshouse.gov.uk"
  final_df = list()
  errorLogs = list()
  auth_keys = auth_api_key

  API_responses = api_codes_lookup()

  i = 1

  while(TRUE){

    if(i > nrow(companies_df)){break()}

    if(is.na(companies_df$CompanyNumber[i])){
      i = i + 1
      next()}

    path_f = sprintf("/company/%s/?items_per_page=100000", companies_df$CompanyNumber[i])
    results_all <- httr::GET(url = url_f, path = path_f, httr::add_headers(Host = "api.companieshouse.gov.uk", Authorization = auth_keys))

    #print(results_all$headers$`x-ratelimit-remain`)

    if(results_all$status_code == 200){

      results_content = rawToChar(results_all$content)

      df_response = fromJSONtoDF_companies_base(jsonlite::fromJSON(results_content)) %>%
                      dplyr::rename(CompanyNumber = company_number)

      final_df[[i]] = companies_df %>%
        dplyr::inner_join(df_response, by = "CompanyNumber")

      cat(paste0(nrow(final_df[[i]]), " Company Profile Information for CompanyNumber  ", companies_df$CompanyNumber[i], " were colected successfully \n"))

      i = i + 1
      next()

    }else if(results_all$status_code == 429){

      Sys.sleep(time_to_rest)
      cat(paste0("\nToo many requests. ", time_to_rest, " seconds until next try ...\n"))
      next()

    }else{

      print(paste0("An error occured with an API Response code: ", results_all$status_code, ". Skipping CompanyID ", companies_df$CompanyNumber[i], "."))

      error_log = data.frame(ResponseCode = results_all$status_code,
                             CompanyNumber = companies_df$CompanyNumber[i],
                             Error_Time = Sys.time())

      errorLogs[[i]] = error_log %>%
        dplyr::inner_join(API_responses, by = c("ResponseCode" = "Code")) %>%
        dplyr::select(ResponseCode, API_Response, CompanyNumber, Error_Time)

      i = i + 1
      next()
    }

  }

  final_results = list(results_df = final_df %>% dplyr::bind_rows(),
                       errorLogs = errorLogs %>% dplyr::bind_rows())

  cat(paste0("\nA total of ", nrow(final_results$results_df), " records were collected for ", length(unique(final_results$results_df$CompanyNumber)), " companies. These results are stored on 'results_df' data-frame. \nThere are ", nrow(final_results$errorLogs), " companies that produced errors or didn't return information and therefore no information was collected. The details on these errors can be found on 'errorLogs'."))
  return(final_results)

}
