library(questionr)
library(janitor)
library(tibble)
library(dplyr)
library(weights)

ta_row <- function(x, y = NULL, w = NULL){
  if(!is.null(y) & is.null(w)){
    as.data.frame.matrix(questionr::wtd.table(x, y)) %>%
      rowid_to_column(var = 'id') %>% 
      adorn_percentages('row') %>% 
      adorn_totals('col') %>% 
      adorn_pct_formatting(digits = 2, affix_sign = FALSE)
  } else if(!is.null(y) & !is.null(w)){
    as.data.frame.matrix(questionr::wtd.table(x, y, w)) %>%
      rowid_to_column(var = 'id') %>% 
      adorn_percentages('row') %>% 
      adorn_totals('col') %>% 
      adorn_pct_formatting(digits = 2, affix_sign = FALSE)
  }
}

ta_col <- function(x, y = NULL, w = NULL){
  if(!is.null(y) & is.null(w)){
    as.data.frame.matrix(questionr::wtd.table(x, y)) %>%
      rowid_to_column(var = 'id') %>% 
      adorn_percentages('col') %>% 
      adorn_totals('row') %>% 
      adorn_pct_formatting(digits = 2, affix_sign = FALSE)
  } else if(!is.null(y) & !is.null(w)){
    as.data.frame.matrix(questionr::wtd.table(x, y, w)) %>%
      rowid_to_column(var = 'id') %>% 
      adorn_percentages('col') %>% 
      adorn_totals('row') %>% 
      adorn_pct_formatting(digits = 2, affix_sign = FALSE)
  }
}

ta <- function(x, y = NULL, w = NULL){
  if(is.null(w) & is.null(y)){
    tmp1 <- as.data.frame(table(x))
    tmp2 <- as.data.frame(wpct(x)*100)
    tmp <- base::cbind(tmp1, tmp2)
    names(tmp)[1] <- 'Code'
    names(tmp)[3] <- '%'
  } 
  if(is.null(y) & !is.null(w)){
    tmp1 <- as.data.frame(questionr::wtd.table(x, weights = w))
    tmp2 <- as.data.frame(wpct(x, w)*100)
    tmp <- base::cbind(tmp1, tmp2)
    names(tmp)[1] <- 'Code'
    names(tmp)[2] <- 'Freq'
    names(tmp)[3] <- '%'
  } 
  if(!is.null(y) & is.null(w)){
    tmp <- table(x, y)
  } 
  if(!is.null(y) & !is.null(w)){
    tmp <- questionr::wtd.table(x, y, w)
  }
  return(tmp)
}


