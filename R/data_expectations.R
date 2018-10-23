


### data expectation functions -----

#' Check if a data frame has duplicates in a given column. If a vector is given, check for duplicates in the vector.
#'
#' @param df data frame to check
#' @param group_by_column character vector name of column expecting no duplicates
#' @param stop_if_surprise T/F for whether to consider failure an error
#' @param report_duplicates T/F for whether to return a partial list of the top duplicates if failure
#'
#' @return several options depending on whether it fails or succeeeds
#' @export
#' @importFrom dplyr group_by_
#' @importFrom dplyr count
#' @importFrom dplyr filter
#'
#' @examples
#' expect_no_duplicates(mtcars, "cyl")
#' # [1] "top duplicates..."
#' # A tibble: 3 x 2
#' # Groups:   cyl [3]
#' #cyl     n
#' #<dbl> <int>
#' #1     4    11
#' #2     6     7
#' #3     8    14
#' # Error in ifelse(stop_if_surprise, stop(paste0("Duplicates detected in column: ",  :
#'                                                 Duplicates detected in column: cyl
#'
#' expect_no_duplicates(rownames(mtcars))
#' # [1] "no vector duplicates...OK"
expect_no_duplicates <- function(df, group_by_column, stop_if_surprise = TRUE, report_duplicates = TRUE){
  if (!("data.frame" %in% class(df)) & is.vector(df)){
    df <- data.frame("vector" = df)
    group_by_column <- "vector"
  }

  if (sum(colnames(df) == as.name(group_by_column)) == 0) {
    stop(paste0("No column named: ", group_by_column))
  }
  if (sum(colnames(df) == as.name(group_by_column)) > 1) {
    stop(paste0("Expected only one, but multiple columns named: ", group_by_column))
  }

  df <- group_by_(df, as.name(group_by_column))
  df <- count(df)
  df <- filter(df, n > 1)
  if(nrow(df) == 0){
    print(paste0("no ", group_by_column, " duplicates...OK"))
  } else if(nrow(df) > 0){
    if(report_duplicates){
      print("top duplicates...")
      print(df)
    }
    ifelse(stop_if_surprise,
           stop(paste0("Duplicates detected in column: ", group_by_column)),
           warning(paste0("Duplicates detected in column: ", group_by_column)))
  }
}


#' Check if a dataframe has the same number of rows as another, or else 0 rows. If vectors are given the lengths of the vectors are compared.
#'
#' @param df1 dataframe or vector to check (required)
#' @param df2 optional second dataframe or vector to compare (if not given, defaults to zero row data frame)
#' @param stop_if_surprise T/F for whether to consider failure an error
#' @param report_rowcount T/F for whether to return the number of rows
#'
#' @return several options depending on whether it fails or succeeeds
#' @export
#'
#' @examples
#' expect_same_number_of_rows(mtcars, mtcars)
#' # [1] "Same number of rows...OK"
#'
#' expect_same_number_of_rows(mtcars, iris)
#' # Error in ifelse(stop_if_surprise, stop(paste0("Different number of rows: ",  :
#' #    Different number of rows: 32 vs: 150
#'
#' expect_same_number_of_rows(mtcars)
#' # Error in ifelse(stop_if_surprise, stop(paste0("Different number of rows: ",  :
#' #    Different number of rows: 32 vs: 0
expect_same_number_of_rows <- function(df1, df2 = data.frame(), stop_if_surprise = TRUE, report_rowcount = FALSE){
  # df2 = data.frame() default means if df2 is not specified, it checks if df1 has zero rows

  # if df is a vector not a df, make it into a df
  if (!("data.frame" %in% class(df1)) & is.vector(df1)){
    df1 <- data.frame(df1)
  }
  if (!("data.frame" %in% class(df2)) & is.vector(df2)){
    df2 <- data.frame(df2)
  }
  if((!("data.frame" %in% class(df1)) & !is.vector(df1)) |
     (!("data.frame" %in% class(df2)) & !is.vector(df2))){
    print(class(df1))
    print(typeof(df1))
    print(class(df2))
    print(typeof(df2))
    stop("One of the inputs is neither a data frame nor vector")
  }

  if (nrow(df1) == nrow(df2)){
    if(nrow(df2) == 0){
      print(paste0("No rows found as expected...OK"))
    } else{
      print(paste0("Same number of rows", ifelse(report_rowcount, paste0(": ", nrow(df1)), ""), "...OK"))
    }
  } else{
    ifelse(stop_if_surprise,
           stop(paste0("Different number of rows: ", nrow(df1), " vs: ", nrow(df2))),
           warning(paste0("Different number of rows: ", nrow(df1), " vs: ", nrow(df2))))
  }
}

#' Check if the column names you expect to be in the df, are indeed in there
#'
#' @param df
#' @param colums_expected a character vector
#'
#' @return
#' @export
#'
#' @examples
#' expect_column_names_somewhere_in_data_frame(mtcars, c("mpg", "cyl"))
#' # [1] "all columns found...OK"
#' expect_column_names_somewhere_in_data_frame(mtcars, c("mpg", "cyl", "car_name"))
#' # Error in expect_column_names_somewhere_in_data_frame(mtcars, c("mpg",  :
#' #   car_name column not found
expect_column_names_somewhere_in_data_frame <- function(df, colums_expected){
  if(sum(names(df) %in% colums_expected) == length(colums_expected)){
    print("all columns found...OK")
  } else{
    cols_not_found <- colums_expected[!(colums_expected %in% names(df))]
    stop(paste0(paste0(cols_not_found, collapse = ", "), " column",
                ifelse(length(cols_not_found) > 1, "s", ""),
                " not found"))
  }
}


#' Check that values in a discrete or categorical vector are within a set of acceptable values
#'
#' @param test_vector vector to test
#' @param correct_vector vector of all acceptable values
#'
#' @return
#' @export
#'
#' @examples
#' expect_values_only_in(mtcars$cyl, c(2, 4, 6))
#' # Error in expect_values_only_in(mtcars$cyl, c(2, 4, 6)) :
#' #   8 value not found in list given
#' expect_values_only_in(mtcars$cyl, c(2, 4, 6, 8))
#' # [1] "all values expected...OK"
expect_values_only_in <- function(test_vector, correct_vector){
  if(typeof(test_vector) != typeof(correct_vector) |
     class(test_vector) != class(correct_vector)){
    stop(paste0("typeof() or class() of test_vector does not match correct_vector"))
  }

  if(sum(!(unique(test_vector) %in% correct_vector)) == 0 ){
    print("all values expected...OK")
  } else{
    vals_not_found <- unique(test_vector)[!(unique(test_vector) %in% correct_vector)]
    stop(paste0(paste0(vals_not_found, collapse = ", "), " value",
                ifelse(length(vals_not_found) > 1, "s", ""),
                " not found in list given"))
  }
}


#' Check if there are any NA values in a data frame, or specified column, withon a tolerance
#'
#' @param df
#' @param test_column character string for column to test - optional
#' @param na_tolerance number of NA allowed before failure, default is zero
#'
#' @return
#' @export
#' @importFrom dplyr select_
#'
#' @examples
#' expect_no_nas(mtcars, "cyl")
#' # [1] "Detected 0 NAs...OK"
#' expect_no_nas(mtcars)
#' # [1] "Detected 0 NAs...OK"
#' expect_no_nas(c(0, 3, 4, 5))
#' # [1] "Detected 0 NAs...OK"
#' expect_no_nas(c(0, 3, NA, 5))
#' # Error in expect_no_nas(c(0, 3, NA, 5)) : Detected 1 NAs
expect_no_nas <- function(df, test_column = NA, na_tolerance = 0){
  if(!is.na(test_column)){
    # -i to handle multiple columns, need to iterate, maybe create or find a function that will make mult names
    df <- select_(df, as.name(test_column))
  }

  na_sum <- sum(is.na(df))
  if(na_sum > na_tolerance){
    stop(paste0("Detected ", na_sum, " NAs"))
  } else{
    print(paste0("Detected ", na_sum, " NAs...OK"))
  }
}





