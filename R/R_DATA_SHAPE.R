
# ================================================================
# FUNCTION TO SUMMARISE THE SHAPE OF AN INPUT DATAFRAME
# ================================================================
datashape <- function(data.input, diagnostics = FALSE) {

  # ------------------------------------------------------
  # Check package dependancies
  # ------------------------------------------------------

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr 0.7 or later needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("caret", quietly = TRUE)) {
    stop("caret needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # ------------------------------------------------------
  if (diagnostics==TRUE) print("Analysing dataframe - this version 26/Jul/2017")

  if (diagnostics==TRUE) print("Step 1")
  # STEP 1. get column names and make the basic dataframe
  x <- names(data.input)
  data.shape <- data.frame("COLUMN_NAME" = x)

  if (diagnostics==TRUE) print("Step 2")
  # STEP 2. get the variable type and add to data.shape
  x <- lapply(data.input, class)
  x <- data.frame(matrix(unlist(x), nrow=length(x), byrow=T))
  names(x)[1] <- "DATA_TYPE"
  data.shape <- dplyr::bind_cols(data.shape, x)

  if (diagnostics==TRUE) print("Step 3")
  # STEP 3. find the level of missing values
  x <- data.frame(MISSING_COUNT = sapply(data.input, function(x) sum(is.na(x))))
  data.shape <- dplyr::bind_cols(data.shape, x)

  if (diagnostics==TRUE) print("Step 4")
  # STEP 4. get the level of unique values for each column
  x <- data.frame(UNIQUE_COUNT = sapply(data.input, function(x) length(unique(x))))
  data.shape <- dplyr::bind_cols(data.shape, x)

  if (diagnostics==TRUE) print("Step 5")
  # STEP 5. work out as percents
  data.shape <- dplyr::mutate(data.shape, MISSING_PCENT = MISSING_COUNT / nrow(data.input) * 100)
  data.shape <- dplyr::mutate(data.shape, UNIQUE_PCENT = UNIQUE_COUNT / nrow(data.input) * 100)

  if (diagnostics==TRUE) print("Step 6")
  # STEP 6. caret estimation of near zero variance
  nrZeroVar <- caret::nearZeroVar(data.input)
  names <- names(data.input)
  nrZeroNames <- names[nrZeroVar]
  data.shape <- dplyr::mutate(data.shape,
                              CARET_NRZEROVAR = ifelse(COLUMN_NAME %in% nrZeroNames,1,0))

  # STEP 6a. caret estimation of high correlation
  #data.corr <- cor(data.input)
  #highCorr <- findCorrelation(data.corr, 0.90)

  if (diagnostics==TRUE) print("Step 7")
  # STEP 7. mins/max/medians

  # min/mean etc fail if you try to use them on factor variables, but are OK
  # with characters. So we need to convert any factor variables to character
  # use mutate_if to do this. BUT if the data has been read using fread then
  # for some reason mutate_if converts every column to character and you lose
  # the integers to character types and then max, min look for the string
  # values, so a range of ints 1-1500 should have max 1500, but if it's a
  # character type the max value is 999

  classes <- sapply(data.input, class)
  factor.count <- length(grep("factor", classes))
  if (factor.count > 0) {
    data.input.nofac <- dplyr::mutate_if(data.input, is.factor, as.character)
  } else {
    data.input.nofac <- data.input
  }

  options(warn=-1)
  # MIN
  imin <- data.input.nofac %>%
    dplyr::summarise_all(dplyr::funs(min(., na.rm = TRUE)))
  # MAX
  imax <- data.input.nofac %>%
    dplyr::summarise_all(dplyr::funs(max(., na.rm = TRUE)))

  # MEDIAN
  if (diagnostics==TRUE) print("Step 7 - median")
  imed <- data.input.nofac %>%
    dplyr::summarise_if(is.numeric,dplyr::funs(median(., na.rm = TRUE)))

  # MEAN
  if (diagnostics==TRUE) print("Step 7 - mean")
  imean  <- data.input.nofac %>%
    dplyr::summarise_if(is.numeric, dplyr::funs(mean(., na.rm = TRUE)))

  # STD DEV
  if (diagnostics==TRUE) print("Step 7 - std dev")
  istdd  <- data.input.nofac %>%
    dplyr::summarise_if(is.numeric, dplyr::funs(sd(., na.rm = TRUE)))

  # SKEWNESS
  if (diagnostics==TRUE) print("Step 7 - skewness")
  iskew  <- data.input.nofac %>%
    dplyr::summarise_if(is.numeric, dplyr::funs(e1071::skewness(., na.rm = TRUE)))

  # KURTOSIS
  if (diagnostics==TRUE) print("Step 7 - kurtosis")
  ikurt  <- data.input.nofac %>%
    dplyr::summarise_if(is.numeric, dplyr::funs(e1071::kurtosis(., na.rm = TRUE)))

  options(warn=0)
  if (diagnostics==TRUE) print("Step 7 - finished")

  # BIND TOGETHER
  data.x <- dplyr::bind_rows(imin, imax, imed, imean, istdd, iskew, ikurt)
  data.x <- data.frame(t(data.x))
  names(data.x) <- c("MIN","MAX","MEDIAN","MEAN","STD_DEV", "SKEWNESS","KURTOSIS")
  data.shape <- dplyr::bind_cols(data.shape, data.x)

  if (diagnostics==TRUE) print("Step 8")
  # STEP 8. remove factors
  data.shape <- dplyr::mutate_if(data.shape, is.factor, as.character)

  if (diagnostics==TRUE) print("Step 9")
  # STEP 9. return the result
  return(data.shape)
}

