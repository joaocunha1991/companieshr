
#' api_codes_lookup (operational function)
#'
#' This function is called to produce a simple API response codes lookup table.
#'
#'


api_codes_lookup = function(){

  tibble::data_frame(Code = c(200, 304, 400, 401, 403, 404, 406, 410, 420, 422, 429, 500, 502, 503, 504),
                     API_Response = c("OK", "Not Modified", "Bad Request", "Unauthorized", "Forbidden" , "Not Found" , "Not Acceptable" , "Gone" , "Enhance Your Calm", "Unprocessable Entity", "Too Many Requests", "Internal Server Error", "Bad Gateway", "Service Unavailable", "Gateway timeout")
                     )

  }


