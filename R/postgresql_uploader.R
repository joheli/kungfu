#' @name postgresql_uploader
#' @title postgresql_uploader
#'
#' @description `postgresql_uploader` is a convenience function to upload contents of a `data.frame`
#' object into an exsisting PostgreSQL table. The table must first be created on the PostgreSQL
#' backend (via "`CREATE TABLE ...`") before `postgresql_uploader` is used.
#'
#' @param con a `DBI::DBIConnection` object created with [RPostgreSQL::dbConnect()].
#' @param r_df data.frame to be uploaded; must contain identical column names to `pg_table`.
#' @param pg_table character specifying PostgreSQL table name; the referred table on the
#' PostgreSQL backend must have the same column names as `r_df`
#' @param unique.field.names character specifying unique column names; please note, that column names
#' of `r_df` and `pg_table` must be identical.
#' @param update logical specifying whether to update existing entries as defined by `unique.field.names`.
#'
#' @return `postgresql_uploader` returns a `list` with information about effected inserts and updates
#'
#' @import DBI
#' @import RPostgreSQL
#'
#' @seealso [RPostgreSQL::dbConnect()]
#'
#' @export
postgresql_uploader <- function(con, r_df, pg_table, unique.field.names, update = TRUE) {
  # extract field names - these *must* match postgresql table field names
  fn <- colnames(r_df)

  # check arguments
  if (!all(unique.field.names %in% fn)) stop("'unique.field.names' must be a subset of 'colnames(r_df)'. Aborting.")

  # create unique name for temporary postgres table
  temp_table_name <- paste0("temp_",
                            paste(sample(letters, 3), collapse = ""), # as timestamp is not collision resistant enough
                            "_",
                            format(Sys.time(), "%Y%m%d%H%M%OS"))

  # sql for temporary table, which is just an empty copy of pg_table, which lives for the duration of a transaction
  create_table_sql <- paste("CREATE TEMP TABLE", temp_table_name, "ON COMMIT DROP AS SELECT * FROM", pg_table, "WHERE FALSE")

  # Start transaction
  dbBegin(con)

  # Create temporary table
  DBI::dbSendQuery(con, create_table_sql)

  # upload r_df into temporary table
  postgresqlWriteTable(con = con, name = temp_table_name, value = r_df, append = TRUE, row.names = FALSE)

  # prepare complicated insert SQL statement
  insert_ <- paste("INSERT INTO", pg_table)
  select_ <- paste("SELECT", paste(fn, collapse = ", "), "FROM", temp_table_name)
  where_ <- paste("WHERE",
                  paste(paste0(unique.field.names, " NOT IN (SELECT ", unique.field.names, " FROM ", pg_table, ")"),
                        collapse = " AND "))
  where2_ <- paste("WHERE",
                  paste(paste0(unique.field.names, " IN (SELECT ", unique.field.names, " FROM ", pg_table, ")"),
                        collapse = " OR "))
  uploaded_sql <- paste(select_, where_)
  not_uploaded_sql <- paste(select_, where2_)
  insert_sql <- paste(insert_, select_, where_)

  # extract info
  uploaded <- dbFetch(dbSendQuery(con, uploaded_sql))
  not_uploaded <- dbFetch(dbSendQuery(con, not_uploaded_sql))

  # copy unique entries from temporary table to pg_table
  status <- dbSendQuery(con, insert_sql)
  info <- dbGetInfo(status)

  # get some more info
  info$uploaded <- uploaded
  info$not_uploaded <- not_uploaded

  # perform update (if update == TRUE)
  if (update && nrow(not_uploaded) > 0) {
    # prepare complicated update SQL statement
    update_ <- paste("UPDATE", pg_table, "SET")
    sets_ <- paste(paste(fn, "=", paste0(paste0(temp_table_name, "."), fn)), collapse = ", ")
    from_ <- paste("FROM", temp_table_name, "WHERE")
    where_ <- paste(paste(paste0(paste0(pg_table, "."), unique.field.names), "=", paste0(paste0(temp_table_name, "."), unique.field.names)), collapse = " AND ")
    update_sql <- paste(update_, sets_, from_, where_)

    # update
    status2 <- dbSendQuery(con, update_sql)
    info2 <- dbGetInfo(status2)

    # add prefix to names within info and info2 so they remain unique
    names(info) <- paste("upload:", names(info))
    names(info2) <- paste("update:", names(info2))

    # merge info and info2
    info <- c(info, info2)
  }

  # commit (and end) transaction
  dbCommit(con)

  # return
  info
}

#' @rdname postgresql_uploader
#'
#' @description `killConnections` disconnects all PostgreSQL connections.
#'
#' @return `killConnections` has no return; its side effect is described above
#' @export
#'
#' @examples
killConnections <- function() {
  for(con in dbListConnections(dbDriver("PostgreSQL"))){
    dbDisconnect(con)
  }
}
