
# ---------------------------------------------

datashape <- function(df, diagnostics = FALSE) {
  library(tidyverse)
  library(reshape2)
  library(magrittr)

  #if (diagnostics == TRUE) df <- iris
  
  # get variable names
  dfout <- as.data.frame(colnames(df), stringsAsFactors = FALSE) %>% 
    set_colnames('Variables')
  
  # check if variable names are well-formed
  names.orig <- colnames(df)
  names.good <- make.names(names.orig)
  if (length(setdiff(names.good, names.orig)) > 0) {
    # give the df good names
    colnames(df) <- names.good
    
    #Change dfout
    dfout <- dfout %>%
      rename(Variables.Orig = Variables) %>%
      mutate(Variables = names.good) %>%
      mutate(Changed = if_else(Variables==Variables.Orig,"","CHANGED"))
  }
  
  # get variable class
  # sometime there is more than 1 description, eg. ordered factor
  dfout$Class <- lapply(sapply(df, class), paste, collapse = ".") %>%
    unlist()

  # get variable typeof
  if (diagnostics==TRUE) print('TypeOf')
  temp <- df %>%
    summarise_all(typeof) %>%
    gather() %>%
    set_colnames(c('Variables','Type')) %>%
    mutate_if(is.factor, as.character)
  dfout <- dfout %>%
    left_join(temp, by=c('Variables'))
  
  # get non-missing values
  temp <- df %>%
    dplyr::summarise_all(funs(sum(!is.na(.)))) %>%
    melt(id.vars = NULL, variable.name = 'Variables', value.name = 'RecordCount') %>%
    mutate_if(is.factor, as.character)
  dfout <- dfout %>%
    left_join(temp, by=c('Variables'))
  
  # get missing values
  if (diagnostics==TRUE) print('Missing')
  temp <- df %>%
    dplyr::summarise_all(funs(sum(is.na(.)))) %>%
    melt(id.vars = NULL, variable.name = 'Variables', value.name = 'Missing') %>%
    mutate_if(is.factor, as.character)
  dfout <- dfout %>%
    left_join(temp, by=c('Variables'))
  
  # get unique values
  temp <- df %>%
    dplyr::summarise_all(n_distinct) %>%
    melt(id.vars = NULL, variable.name = 'Variables', value.name = 'Unique') %>%
    mutate_if(is.factor, as.character)
  dfout <- dfout %>%
    left_join(temp, by=c('Variables'))
  
  # get length of content
  temp <- df %>% 
    dplyr::summarise_all(funs(mean(str_length(.)))) %>% 
    melt(id.vars = NULL, variable.name = 'Variables', value.name = 'StrLength') %>%
    mutate_if(is.factor, as.character)
  dfout <- dfout %>%
    left_join(temp, by=c('Variables'))
  
  # mean of numerics
  if (diagnostics==TRUE) print('Means')
  temp <- df %>%
    summarise_if(is.numeric, mean, na.rm = TRUE) %>%
    melt(id.vars = NULL, variable.name = 'Variables', value.name = 'Mean') %>%
    mutate_if(is.factor, as.character)
  dfout <- dfout %>%
    left_join(temp, by=c('Variables'))
  
  # median of numerics
  if (diagnostics==TRUE) print('Medians')
  temp <- df %>%
    summarise_if(is.numeric, median, na.rm = TRUE) %>%
    melt(id.vars = NULL, variable.name = 'Variables', value.name = 'Median') %>%
    mutate_if(is.factor, as.character)
  dfout <- dfout %>%
    left_join(temp, by=c('Variables'))
  
  # max of numerics
  if (diagnostics==TRUE) print('Max')
  temp <- df %>%
    summarise_if(is.numeric, max, na.rm = TRUE) %>%
    melt(id.vars = NULL, variable.name = 'Variables', value.name = 'Max') %>%
    mutate_if(is.factor, as.character)
  dfout <- dfout %>%
    left_join(temp, by=c('Variables'))
  
  # min of numerics
  if (diagnostics==TRUE) print('Min')
  temp <- df %>%
    summarise_if(is.numeric, min, na.rm = TRUE) %>%
    melt(id.vars = NULL, variable.name = 'Variables', value.name = 'Min') %>%
    mutate_if(is.factor, as.character)
  dfout <- dfout %>%
    left_join(temp, by=c('Variables'))
  
  # skewness of numerics
  vars.num <- dfout %>%
    filter(Class == 'numeric') %>%
    select(Variables) %>%
    pull()
  temp <- df %>%
    select(one_of(vars.num)) %>%
    melt(id = NULL, variable.name = 'Variables', value.name = 'Vals') %>%
    group_by(Variables) %>%
    summarise(Skewness = skewness(Vals)) %>%
    mutate_if(is.factor, as.character)
  dfout <- dfout %>%
    left_join(temp, by=c('Variables'))
  
  
  # vars.chr <- shape.1 %>%
  #   filter(Class=='character')%>%
  #   select(Variables) %>%
  #   pull()
  # 
  # temp <- df %>%
  #   select(one_of(vars.chr)) %>%
  #   melt(id = NULL, variable.name = 'Variables', value.name = 'Vals') %>%
  #   filter(!is.na(Vals)) %>%
  #   group_by(Variables) %>%
  #   arrange(Vals) %>%
  #   slice(c(1,n())) %>%
  #   mutate(Type=if_else(row_number()==1,'MIN','MAX')) 
  # 
  
  # return result
  return(dfout)
}


# datashape(iris, FALSE)
 


# -------------------

