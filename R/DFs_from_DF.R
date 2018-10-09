


#' DFs_from_DF
#'
#' This function simplifies items returns that include DataFrames and lists as columns.
#'
#' @param json_list A json items api response.
#' @return A  dataframe with simplified response.
#'
#' @import magrittr
#'
#' @examples
#' \dontrun{
#' DFs_from_DF(df_items)
#'}

DFs_from_DF = function(json_list){

  #Create a dataframe with all the fields that are not 'items'. Fields in lists will be unlisted recursively:
  df_original = t(unlist(json_list[names(json_list) != "items"], recursive = TRUE)) %>% data.frame(stringsAsFactors = FALSE)

  if(!is.null(json_list$items) & class(json_list$items) == "data.frame"){

    df_of_dfs = json_list$items[, sapply(json_list$items, class) == "data.frame"]

    df_items_non_sub_df = json_list$items[, sapply(json_list$items, class) != "data.frame"] %>%
                            #dplyr::mutate_if(is.list, dplyr::funs(sapply(persons_entitled, function(x){paste(x$name, collapse = " &&& ")}))) %>%
                            dplyr::mutate_if(is.factor, dplyr::funs(as.character(.))) %>%
                            dplyr::rename_all(dplyr::funs(paste0("items.", .)))

    if(length(df_of_dfs) == 0){

      final_df = df_original %>%
        dplyr::bind_cols(df_items_non_sub_df)


    }else{

      list_sub_dfs = list()

      for(i in 1:length(df_of_dfs)){

          #There are rare exceptions of columns inside the dataframes inside 'items' that actually contain another df. We ignore these:
          clean_df_of_dfs = df_of_dfs[,i][, sapply(df_of_dfs[,i], class) != "data.frame"]
          if(length(clean_df_of_dfs) == 0){next()}

          list_sub_dfs[[i]] = data.frame(clean_df_of_dfs, stringsAsFactors = FALSE) %>%
            #dplyr::mutate_if(is.list, dplyr::funs(paste(.[[1]], collapse = " & "))) %>%
            dplyr::rename_all(dplyr::funs(paste0(colnames(df_of_dfs)[i], ".", .)))

      }


      final_df = list_sub_dfs[!sapply(list_sub_dfs, is.null)] %>%
        do.call(what = "cbind", args = .) %>%
        cbind(df_original, df_items_non_sub_df)

    }

  }else{
    final_df = df_original

  }


  return(final_df)
}
