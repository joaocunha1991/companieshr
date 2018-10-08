

#' api_end_point_map_df
#'
#'
#' @return A data.frame with a map between api_end_point_type and end_point string.
#'
#' @import magrittr
#'



api_end_point_map_df = function(){


  data.frame(api_end_point_type = c("company_profile", "persons_significant_control", "officers", "charges"),
             end_point = c("/company/%s/?items_per_page=%s", "/company/%s/persons-with-significant-control?items_per_page=%s", "/company/%s/officers?items_per_page=%s", "/company/%s/charges?items_per_page=%s" ),
             stringsAsFactors = FALSE)


}
