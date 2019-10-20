

### sql query functions ------


#' Gets around limit of 1000 items in a list by iterating a query
#'
#' @param sortie_df data frame of one column potentially > 1000 rows needing iteration in SQL because it's too long to turn into a list
#' @param sql_left_join_to_in SQL code to SELECT the table to save as a left join starting point through the word IN just before the the (list)
#' @param sql_query_before_new_table SQL code to select table to return from SELECT up to and not including the name of the temp table
#' @param sql_query_after_new_table SQL code to select table to return after the name of the temp table through to the JOIN ON clause, etc.
#' @param conn an open DBIConnection object, as returned by dbConnect()
#'
#' @return data_frame object
#' @export
#' @import dplyr 
#' @importFrom glue glue_sql
#' @importFrom DBI dbSendQuery
#' @importFrom DBI dbGetQuery
#'
#' @examples
#' join_list_sql(mtcars, "wt", "mpg", "weight", "miles per gallon")
join_list_sql <- function(sortie_df, sql_left_join_to_in, sql_query_before_new_table, sql_query_after_new_table, conn){
  # todo: handle other that sortie_df with one column called "PERSON_ID"
  # todo: check connection is open
  # todo: check table was created and return useful error if not
  sortie_df <- mutate(sortie_df, 
                      PERSON_ID = if_else(grepl("'", PERSON_ID),
                                          gsub("'", "''", PERSON_ID),
                                          PERSON_ID),
                      # have to split them into groups of 999
                      sortie = ceiling(row_number() / 999))
  
  print(count(sortie_df, sortie))
  
  table_name <- paste0("temp_to_delete_", as.integer(Sys.time()))
  
  sortie_output <- list()
  n_sorties <- max(sortie_df$sortie)
  for(s in 1:n_sorties){
    print(paste0("building ", table_name,
                 " table, part: ", s, 
                 " of ", n_sorties, "..."))
    sub_key_ids <- pull(select(filter(
      sortie_df, sortie == s), -sortie))
    if(s == 1){
      try(dbSendQuery(conn,
                      glue_sql("CREATE TABLE ",
                               table_name,
                               " AS ",
                               sql_left_join_to_in,
                               " ('",
                               paste0(sub_key_ids, collapse = "','"),
                               "')")), silent = T)
    } else if (s > 1){
      try(dbSendQuery(conn,
                      glue_sql("INSERT INTO ",
                               table_name,
                               " ",
                               sql_left_join_to_in,
                               " ('",
                               paste0(sub_key_ids, collapse = "','"),
                               "')")), silent = T)
    }
  }
  # run full query
  print("running left join...")
  try(sortie_output <- 
        as_tibble(dbGetQuery(conn, 
                             glue_sql(sql_query_before_new_table,
                                      " ",
                                      table_name,
                                      " ",
                                      sql_query_after_new_table))))
  # then delete the temp table
  print(paste0("dropping temporary table ", table_name,
               "..."))
  try(dbSendQuery(conn, glue_sql("DROP TABLE ",
                                 table_name)), silent = T)
  
  return(sortie_output)
}



#' Syntatic sugar to turn a vector into a SQL "IN (x1, x2, x3, ...)" phrase
#'
#' @param x vector of values to search in
#' @param nbrs False if characters needing quotes, else will not quote values
#' 
#' @return character vector object
#' @export
#'
#' @examples
#' in_sql_list(mtcars$hp, nbrs = TRUE)
in_sql_list <- function(x, nbrs = FALSE){
  x <- unique(x)
  if(length(x) > 1000){
    warning(paste0("length of unique values is, ", 
                   length(x),
                   " more than 1000, which may result in an error. Consider using in conjunction with function join_list_sql()"))
  }
  if(nbrs){
    y <- paste0(" IN (", paste0(x, collapse = ","), ") ")
  } else {
    y <- paste0(" IN ('", paste0(x, collapse = "','"), "') ")
  }
  return(y)
}


#' Connect to DB, and if it's timed out, tries again 
#'
#' @param conn an open DBIConnection object, as returned by dbConnect()
#' @param statement SQL code to query
#' @param conn_specs a list that has item 1 = drv, item 2 = host, item 3 = keyring service name that is the server with password of user password for server
#' @param db_func specifies which dbXQuery function to use, where X is the name of the db_func
#'
#' @return data_frame object
#' @export
#' @import dplyr 
#' @import keyring
#' @importFrom DBI dbConnect
#' @importFrom DBI dbGetQuery
#'
#' @examples
#' dbQueryReconnect("SELECT * FROM GENDER_D")
#' dbQueryReconnect("DROP TABLE ABC_XYZ_123___", db_func = "send")
dbQueryReconnect <- function(statement, con = conn, ..., conn_specs = dbConnect_specs, db_func = c("get", "send")){
  # todo: get conn_specs to work better
  # todo: make recconect a general feature for dbSendQuery too, etc.
  if(length(db_func) > 1){db_func = "get"}
  db_func <- tolower(db_func)
  tryCatch({
    if(db_func == "get"){
      f <- as_tibble(dbGetQuery(con, statement, ...))
    } else if(db_func == "send"){
      f <- as_tibble(dbSendQuery(con, statement, ...))
    }
    return(f)
  },
  error = function(err) {
    print(paste("reconnecting after:  ", err))
    try(dbDisconnect(con))
    try(dbDisconnect(con))
    try({con <- dbConnect(conn_specs[[1]],
                           conn_specs[[2]], 
                           key_list(conn_specs[[3]])[1,2], 
                           key_get(conn_specs[[3]]))
    if(db_func == "get"){
      f <- as_tibble(dbGetQuery(con, statement, ...))
    } else if(db_func == "send"){
      f <- as_tibble(dbSendQuery(con, statement, ...))
    }
    })
    # have to disconnect if connected in a function
    try(dbDisconnect(con))
    try(dbDisconnect(con))
    return(f)
  })
}
