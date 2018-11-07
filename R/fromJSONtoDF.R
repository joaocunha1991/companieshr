


#' fromJSONtoDF
#'
#' This function is simplfies the conversion of the JSON response from the companies (base) API end-point into a data-frame format.
#'
#'
#' @param json_list A list representing the JSON response from the companies/ (base) API end-point.
#' @param api_end_point A character value describing which end-point to call.
#' @param company_number Company number
#' @return A  dataframe with a single row response.
#'
#' @import magrittr
#'
#' @examples
#' \dontrun{
#' fromJSONtoDF(json_list)
#'}


fromJSONtoDF = function(json_list, api_end_point, company_number){

  df_original = t(unlist(json_list, recursive = TRUE)) %>% data.frame(stringsAsFactors = FALSE)

  if(api_end_point == "company_profile"){

    df_final = DFs_from_DF(json_list = json_list) %>%
      dplyr::mutate(SIC_Codes = paste(t(dplyr::select(df_original, dplyr::contains("sic_code"))), collapse =', '),
                    Previous_Company_Names = paste(t(dplyr::select(df_original, dplyr::contains("previous_company_names.name"))), collapse =', ')) %>%
      dplyr::select( -dplyr::contains("sic_code", ignore.case = FALSE), -dplyr::contains("previous_company_names", ignore.case = FALSE)) %>%
      dplyr::rename(CompanyNumber = company_number,
                    CompanyName = company_name) %>%
     dplyr::select(CompanyNumber, CompanyName, dplyr::everything())


  }else if(api_end_point == "persons_significant_control"){

    df_final = DFs_from_DF(json_list = json_list) %>%
      dplyr::mutate(CompanyNumber = company_number,
                    items.natures_of_control = sapply(items.natures_of_control, function(x){paste(x, collapse = " & ")})) %>%
      dplyr::select(CompanyNumber, dplyr::everything())

  }else if(api_end_point == "officers"){

    df_final = DFs_from_DF(json_list = json_list) %>%
      dplyr::mutate(CompanyNumber = company_number) %>%
      dplyr::select(CompanyNumber, dplyr::everything())

  }else if(api_end_point == "charges"){

    df_final = DFs_from_DF(json_list = json_list) %>%
      dplyr::mutate(CompanyNumber = company_number,
                    items.persons_entitled = sapply(items.persons_entitled, function(x){paste(x$name, collapse = " & ")})) %>%
      dplyr::select(CompanyNumber, dplyr::everything())

  }

  return(df_final)
}


