


#' DFs_from_DF
#'
#' This function simplifies items returns that include DataFrames and lists as columns.
#'
#' @param json_list A json items api response.
#' @return A dataframe with simplified response.
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

  #If there is there is 'items' data frame:
  if(!is.null(json_list$items) & class(json_list$items) == "data.frame"){

    #filter all the objects inside items that are a data-frame:
    df_of_dfs = json_list$items[, sapply(json_list$items, class) == "data.frame"]

    #All the columns that are inside items but are not a data.frame save them and apend the suffic 'items.' to the colnames:
    df_items_non_sub_df = json_list$items[, sapply(json_list$items, class) != "data.frame"] %>%
                            #dplyr::mutate_if(is.list, dplyr::funs(sapply(persons_entitled, function(x){paste(x$name, collapse = " &&& ")}))) %>%
                            dplyr::mutate_if(is.factor, dplyr::funs(as.character(.))) %>%
                            dplyr::rename_all(dplyr::funs(paste0("items.", .)))

    #If there are no data-frame columns in 'items', then the final_df is df_original + df_items_non_sub_df
    if(length(df_of_dfs) == 0){

      final_df = df_original %>%
        dplyr::bind_cols(df_items_non_sub_df)


    }else{

      list_sub_dfs = list()

      #If there are columns that are data-frames in 'items' then we have to iterate over them
      for(i in 1:length(df_of_dfs)){

          #There are rare exceptions of columns inside the dataframes inside 'items' that actually contain another df.
          #We ignore these and extract only the non-data-frame columns:
          clean_df_of_dfs = df_of_dfs[,i][, sapply(df_of_dfs[,i], class) != "data.frame"]

          #If one of the dfs inside 'items' have to columns we skip it.
          if(length(clean_df_of_dfs) == 0){next()}

          #Save these extracted data-frames (clean_df_of_dfs) in a list for later appending.
          #We also change the names of the columns based on the name of the df column (parent =).
          list_sub_dfs[[i]] = data.frame(clean_df_of_dfs, stringsAsFactors = FALSE) %>%
            #dplyr::mutate_if(is.list, dplyr::funs(paste(.[[1]], collapse = " & "))) %>%
            dplyr::rename_all(dplyr::funs(paste0(colnames(df_of_dfs)[i], ".", .)))

      }

      #The final_df when there are data-frame columns inside the items data frame is: all the dfs in list_sub_dfs + df_original + df_items_non_sub_df
      final_df = list_sub_dfs[!sapply(list_sub_dfs, is.null)] %>%
        do.call(what = "cbind", args = .) %>%
        cbind(df_original, df_items_non_sub_df)

    }

  #If there is no 'items' objest on the json_list then the result is simply all the columns of df_original.
  }else{
    final_df = df_original

  }

  return(final_df)
}
