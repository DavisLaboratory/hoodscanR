
col2rownames <- function(df, rn = "rowname"){
  stopifnot(is.data.frame(df))
  df <- as.data.frame(df)
  rownames(df) <- df[[rn]]
  df[[rn]] <- NULL
  return(df)
}

rownames2col <- function(df, rn = "rowname"){
  stopifnot(is.data.frame(df))
  df[[rn]] <- rownames(df)
  rownames(df) <- NULL
  return(df)
}