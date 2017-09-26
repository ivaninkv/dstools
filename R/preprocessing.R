#' Mark outliers in data.table.
#'
#' Add column with name or prefix 'out' and mark every row: -1 normal, 1 outlier.
#'
#' @param dt A data.table for modify.
#' @param one_column A boolean, default TRUE. One or more out column.
#' @param inline A boolean, default TRUE. Modify current data.table or return new data.table.
#' @param iqr_ratio A numeric, default 1.5. IQR ratio for detect outlier
#'
#' @examples
#' ds_mark_outlier(train.data)
#' ds_mark_outlier(train.data, one_column = TRUE, inline = FALSE)

ds_MarkOutlier <- function(dt, one_column = T, inline = T, iqr_ratio = 1.5) {
  ds_CheckLibrary()

  library(data.table)

  subFunc <- function(dt_) {
    num.col <- names(dt_)[sapply(dt_, is.numeric)]
    for (feat in num.col) {
      min_tr <- median(dt_[[feat]]) - iqr_ratio * IQR(dt_[[feat]])
      max_tr <- median(dt_[[feat]]) + iqr_ratio * IQR(dt_[[feat]])
      if (one_column == TRUE) {
        set(dt_, j = 'out',
            value = ifelse(dt_[[feat]] < min_tr | dt_[[feat]] > max_tr, 1,-1)
        )
      } else
      {
        set(dt_, j = paste(feat, 'out', sep = '_'),
            value = ifelse(dt_[[feat]] < min_tr | dt_[[feat]] > max_tr, 1,-1))
      }

    }
  }

  if (inline == TRUE) {
    subFunc(dt)
  }
  else
  {
    dt.new <- copy(dt)
    subFunc(dt.new)
    return(dt.new)
  }
}


#' Transform int columns to factor.
#'
#' Function tranform integer columns in factor by threshold.
#'
#' @param dt A data.table for modify.
#' @param threshold A integer, default 10. Number unique value in column for transform.
#' @param inline A boolean, default TRUE. Modify current data.table or return new data.table.
#' @param exclude A character vector for exclude columns.

ds_IntToFactor <- function(dt, threshold = 10, inline = T, exclude = c()) {
  ds_CheckLibrary()

  library(data.table)

  subFunc <- function(dt_) {
    int.col <- names(dt_)[sapply(dt_, is.integer)]
    if (length(exclude) > 0) {
      int.col <- int.col[!int.col %in% exclude]
    }
    if (length(int.col) > 0) {
      for (feat in int.col) {
        if (length(unique(dt_[[feat]])) <= threshold) {
          set(dt_, j = feat, value = factor(dt_[[feat]]))
        }
      }
    }
  }

  if (inline == TRUE) {
    subFunc(dt)
  }
  else
  {
    dt.new <- copy(dt)
    subFunc(dt.new)
    return(dt.new)
  }
}


#' Tranform data.table to sparse matrix
#'
#' @param dt A data.table for tranform.

ds_toSparseMatrix <- function(dt) {
  ds_CheckLibrary()

  library(Matrix)

  return(sparse.model.matrix(~ . -1, dt))
}


#' Tranform some continuous column to categorical.
#'
#' @param dt A data.table for tranform.
#' @param col_names A character vector columns name for transform.
#' @param group_count An integer, default 5. Quantity group.
#' @param inline A boolean, default TRUE. Modify current data.table or return new data.table.

ds_ContToCat <- function(dt, col_names, group_count = 5, inline = T) {
  library(data.table)

  subFunc <- function(dt_) {
    if (length(col_names) == 0) stop('Use columns name argument!')
    cut_manual <- function(x) {
      cnt <- ifelse(length(unique(quantile(x, seq(0, 1, length.out = group_count + 1), names = F))) < group_count + 1,
                    length(unique(quantile(x, seq(0, 1, length.out = group_count + 1), names = F))) - 1,
                    group_count + 1)
      cut(
        x,
        quantile(x, seq(0, 1, length.out = cnt), names = F),
        include.lowest = T,
        labels = paste('group', 1:(cnt - 1), sep = '_')
      )
    }

    for (feat in col_names)
      set(dt_, j = feat, value = cut_manual(dt_[[feat]]))
  }

  if (inline == T) {
    subFunc(dt)
  } else {
    dt.new <- copy(dt)
    subFunc(dt.new)
    return(dt.new)
  }
}


#' Check installed library

ds_CheckLibrary <- function() {
  inst.pkg <- installed.packages()[,1]

  if (!'data.table' %in% inst.pkg) stop('Install package data.table!')
  if (!'Matrix' %in% inst.pkg) stop('Install package Matrix!')

}
