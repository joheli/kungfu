#' @name postgresql_uploader
#' @title postgresql_uploader
#'
#' @description `postgresql_uploader` is a convenience function to upload contents of a `data.frame`
#' object into an exsisting PostgreSQL table. The table must first be created on the PostgreSQL
#' backend (via "`CREATE TABLE ...`") before `postgresql_uploader` is used.
#'
#' @param con a `DBI::DBIConnection` object created with \link[RPostgreSQL]{dbConnect}.
#' @param r_df data.frame to be uploaded; must contain identical column names to `pg_table`.
#' @param pg_table character specifying existing PostgreSQL table name; the referred table on the
#' PostgreSQL backend must have the same column names as `r_df`; `pg_table` defaults to name of object passed
#' as `r_df` (i.e. `deparse(substitute(r_df))`).
#' @param unique.field.names character specifying unique column names as defined in PostgreSQL table constraints
#' (i.e. primary key); please note, that column names of `r_df` and `pg_table` must be identical!
#' @param update logical, specifying whether to update existing entries as defined by `unique.field.names`;
#' defaults to `TRUE`.
#' @param clean_r_df logical, specifying whether to clean `r_df` with function \link[kungfu]{cleaner},
#' if duplicates are found; defaults to `TRUE`, as the PostgreSQL backend will terminate the upload with an
#' error if primary key constraints are violated.
#'
#' @return `postgresql_uploader` returns a `list` with information about effected inserts and updates
#'
#' @import DBI
#' @import RPostgreSQL
#'
#' @seealso \link[RPostgreSQL]{dbConnect}
#'
#' @export
postgresql_uploader <- function(con, r_df, pg_table = deparse(substitute(r_df)),
                                unique.field.names, update = TRUE, clean_r_df = TRUE) {
  # extract field names - these *must* match postgresql table field names
  fn <- colnames(r_df)

  # check arguments
  if (!all(unique.field.names %in% fn))
    stop("'unique.field.names' must be a subset of 'colnames(r_df)'. Aborting.")

  # create unique and collision resistant name for temporary postgres table
  temp_table_name <- paste0("temp_",
                            paste(sample(letters, 3), collapse = ""), # as timestamp is not collision resistant enough
                            "_",
                            format(Sys.time(), "%Y%m%d%H%M%OS"))

  # sql for temporary table, which is just a copy of the truncated pg_table
  create_table_sql <- paste("CREATE TEMP TABLE", temp_table_name,
                            "ON COMMIT DROP AS SELECT * FROM", pg_table,
                            "WHERE FALSE")
  # delete command is also necessary, as temporary tables seem to have long lives
  delete_table_sql <- paste("DROP TABLE IF EXISTS", temp_table_name)

  #     prepare complicated insert SQL statement:
  insert_ <- paste("INSERT INTO", pg_table)
  select_ <- paste0("SELECT ", paste(paste0("a.", fn), collapse = ", "), " FROM ", temp_table_name, " a ",
                   "LEFT JOIN ", pg_table, " b USING(", paste(unique.field.names, collapse = ", "), ") ",
                   "WHERE ", paste0("b.", unique.field.names[1]))
  select_null_ <- paste(select_, "IS NULL")
  select_not_null_ <- paste(select_, "IS NOT NULL")

  uploaded_sql <- select_null_
  not_uploaded_sql <- select_not_null_
  insert_sql <- paste(insert_, select_null_)
  #     update statement
  update_ <- paste("UPDATE", pg_table, "SET")
  sets_ <- paste(paste(fn, "=", paste0(paste0(temp_table_name, "."), fn)), collapse = ", ")
  from_ <- paste("FROM", temp_table_name, "WHERE")
  where_ <- paste(paste(paste0(paste0(pg_table, "."), unique.field.names), "=", paste0(paste0(temp_table_name, "."), unique.field.names)), collapse = " AND ")
  update_sql <- paste(update_, sets_, from_, where_)

  # initialize return list
  info <- list(failure = "upload failed")

  # check, if r_df needs to be cleaned
  if (clean_r_df) {
    if (!is_clean(r_df, ufn = unique.field.names)) {
      r_df_c1 <- cleaner(r_df, ufn = unique.field.names) # remove duplicates
      r_df_c2 <- cleaned(r_df, r_df_c1)
      r_df <- r_df_c1
      message("Duplicate entries in 'r_df' were removed:")
      print(r_df_c2)
    }
  }

  # try to upload data
  tryCatch({
    # start transaction
    dbBegin(conn = con)

    # Create temporary table ...
    dbSendStatement(conn = con, create_table_sql)
    # .. and upload r_df into newly created temporary table
    dbWriteTable(conn = con, name = temp_table_name, value = r_df, row.names = FALSE)

    # extract info regarding uploaded and not uploaded data ...
    uploaded <- dbFetch(dbSendQuery(conn = con, uploaded_sql))
    not_uploaded <- dbFetch(dbSendQuery(conn = con, not_uploaded_sql))

    # perform insert and add feedback information to info
    status <- dbSendStatement(conn = con, insert_sql)
    info <- dbGetInfo(status)
    info$uploaded <- uploaded
    info$not_uploaded <- not_uploaded

    # perform update (if update == TRUE) and add feedback information to info
    if (update && nrow(not_uploaded) > 0) {
      # update
      status2 <- dbSendStatement(con, update_sql)
      info2 <- dbGetInfo(status2)

      # add prefix to names within info and info2 so they remain unique
      names(info) <- paste("upload:", names(info))
      names(info2) <- paste("update:", names(info2))

      # merge info and info2
      info <- c(info, info2)
    }

    # commit
    dbCommit(con)
  }, error = function(e) {
    # rollback
    dbRollback(con)
    # message
    message("Upload failed. Error message:")
    message(e)
  }, finally = {
    # delete temp table (unfortunately they persist if not explicitly deleted)
    dbSendStatement(con, delete_table_sql)
  })

  # return
  info
}

#' @rdname postgresql_uploader
#'
#' @description `killConnections` disconnects all PostgreSQL connections.
#'
#' @return `killConnections` has no return; its side effect is described above
#' @export
killConnections <- function() {
  for(con in dbListConnections(dbDriver("PostgreSQL"))){
    dbDisconnect(conn = con)
  }
}
