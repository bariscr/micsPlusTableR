
col_group_perc_total_check <-
  
  function(cell_results, diff = 1e-11) {
    
    if (!"cond" %in% names(cell_results)) {
      
      cat("There isn't a columnwise percentage distribution in this table that's expected to sum to 100.")
      totals_col_perc_df <<- NULL
      totals_col_perc_df_issue <<- NULL
      totals_col_perc_df_issue_n <<- 0
      
    } else {
    
    conds_work <-
      cell_results |> 
      filter(stat_type %in% c("p", "100")) |> 
      distinct(cond, stat_type) |> 
      count(cond) |> 
      filter(n == 2) |> 
      pull(cond)
    
    if (length(conds_work) > 0) {
      
      # We determine the last 100 column, We need this to filter out p beyond this point
      max_col_100 <-
        cell_results |> 
        filter(stat_type == 100) |> 
        summarise(max_col_100 = max(col_index)) |> 
        pull()
      
      blocks <-
        cell_results |> 
        # We filtering out any p that is beyond the last 100 column
        filter(!col_index > max_col_100 & stat_type %in% c("p", "100")) |> 
        select(row_index, col_index, stat_type) |> 
          filter(row_index == min(row_index)) |> 
          dplyr::mutate(block = (cumsum(stat_type == "100") + 1L) - as.integer(stat_type == "100")) |> 
        select(col_index, block)

      
      totals_col_perc_df <-
      cell_results |> 
        left_join(blocks, by = c("col_index")) |> 
        filter(cond %in% conds_work, 
               stat_type == "p") |>
        filter(!is.na(block)) |> # If a block has not been created we drop these rows
        group_by(cond, block, row_index) |> 
        summarise(sum_block = sum(value)) |>
        ungroup() |> 
        mutate(diff_value = sum_block - 100) 
      
      totals_col_perc_df <<- totals_col_perc_df
      
      totals_col_perc_df_issue <-
        totals_col_perc_df |>
        filter(!is.na(sum_block), abs(diff_value) > diff)
      
      totals_col_perc_df_issue_n <<- nrow(totals_col_perc_df_issue)
      
      totals_col_perc_df_issue
      
      
    } else {
      
      cat("There isn't a columnwise percentage distribution in this table that's expected to sum to 100.")
      
    }
    }
  }