
# log_search(user_id, query)
#
# user_id: integer<br>query: character
#
# TRUE/FALSE
#
# Logs a search query in adem.search_logs. Returns TRUE on success, FALSE otherwise.
#
# log_search(user_id = 1, query = "machine learning jobs")

#' Logs a search query in a specific schema table, given a user ID and a search query.
#'
#' @param user_id The ID (an integer) of the user who issued the search query.
#' @param query The query string used in the search.
#'
#' @returns TRUE, if logging was completed / successful; FALSE, if any error has occurred (e.g., invalid or not found user ID, invalid search query), preventing the logging.
#' @export
#'
#' @examples
#' \dontrun{
#' log_search(user_id = 1, query = "machine learning jobs")
#' }
log_search <- function (user_id, query) {
  if ((is.na(user_id)) || (!is.numeric(user_id))) {
    warning("Error: user_id in log_search() not specified or not an integer. No search data logged.")
    return(FALSE)
  }
  conn = connect_db()
  query_user = glue::glue_sql("SELECT user_id, username
                              FROM adem.api_users
                              WHERE user_id = {user_id}", .con = conn)
  df_user <- DBI::dbGetQuery(conn, query_user)
  if (is.null(df_user)) {
    warning("Error: user_id not found in log_search(). No search data logged.")
    DBI::dbDisconnect(conn)
    return(FALSE)
  }
  if ((is.na(query)) || (!is.character(query))) {
    warning("Error: query in log_search() not specified or not a string. No search data logged.")
    return(FALSE)
  }
  insert_stmt = glue::glue_sql("INSERT INTO student_miguel.search_log (user_id, query) VALUES ({user_id}, {query})", .con = conn)
  result <- DBI::dbGetQuery(conn, insert_stmt)
  DBI::dbDisconnect(conn)
#  if (result)
  return(TRUE)
}



