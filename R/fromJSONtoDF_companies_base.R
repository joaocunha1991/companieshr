


#' fromJSONtoDF_companies_base
#'
#' This function is simplfies the conversion of the JSON response from the companies (base) API end-point into a data-frame format.
#'
#'
#' @param json_list A list representing the JSON response from the companies/ (base) API end-point.
#' @return A  dataframe with a single row response.
#'
#' @import magrittr
#'
#' @examples
#' \dontrun{
#' fromJSONtoDF_companies_base(json_list)
#'}

# In order to include another API end-point on the package, all I have to do is to adapt this function for possible cases where several granularities are returned per call.

fromJSONtoDF_companies_base = function(json_list){

  df_original = t(unlist(json_list, recursive = TRUE)) %>% data.frame(stringsAsFactors = FALSE)

  df_final = df_original %>%
    dplyr::select(-contains("sic_code"), -contains("previous_company_names")) %>%
    dplyr::mutate(SIC_Codes = paste(t(dplyr::select(df_original, contains("sic_code"))), collapse =', '),
           Previous_Company_Names = paste(t(dplyr::select(df_original, contains("previous_company_names.name"))), collapse =', '))

  return(df_final)

}

