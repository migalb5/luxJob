
#' Validates a user token via DB (and decreases user API call quota, if token validated)
#'
#' @param token The string representing the Bearer Token to be used for user authentication/authorization.
#' @param schema The name of the schema (a string) to be used for looking up API users.
#'
#' @returns TRUE, if token has been found in DB (i.e., it belongs to one existing user); FALSE, otherwise.
#' @export
#'
#' @examples
#' \dontrun{
#' verify_token("token123", "student_miguel")
#' }
verify_token <- function (token, schema) {
  if ((!is.na(token)) || (nchar(token) == 0)) {
    warning("Error: Authentication/authorization token not provided or empty. API call denied.")
    return(FALSE)
  }
  conn = connect_db()
  query = glue::glue_sql("SELECT user_id, username, token, quota
                          FROM {schema}.api_users
                          WHERE token = {token}", .con = conn)
  df <- DBI::dbGetQuery(conn, query)
  if (nrow(df) == 0) {
    warning("Error: Wrong / not existing authentication/authorization token provided. API call denied.")
    DBI::dbDisconnect(conn)
    return(FALSE)
  }
  decrement_quota(conn, token)
  DBI::dbDisconnect(conn)
  return(TRUE)
}

#' Decrements API call user quota (given a validated token)
#'
#' @param conn A valid DB connection object.
#' @param token The validated user token (a string).
#' @param schema The DB schema to use for user lookup/update.
#'
#' @returns Nothing.
#'
#' @examples
#' \dontrun{
#' decrement_quota(conn, "token456", student_miguel")
#' }
decrement_quota <- function (conn, token, schema) {
  query = glue::glue_sql("UPDATE {schema}.api_users
                          SET quota = quota - 1
                          WHERE token = {token}", .con = conn)
  DBI::dbExecute(conn, query)
}
