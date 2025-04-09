
# get_companies(limit = 100)
#
# limit: integer
#
# data.frame with company_id, name, sector
#
# Returns all companies from adem.companies.
#
# get_companies(limit = 50)

#' Obtains a list of the companies.
#'
#' @param limit The size (an integer) of the list of companies to be returned.
#'
#' @returns A data frame containing the list of the companies and corresponding details. List can be limited using limit argument.
#' @export
#'
#' @examples
#' \dontrun{
#' get_companies(limit = 50)
#' }
get_companies <- function (limit = 100) {
  if (!is.numeric(limit) || (limit < 1)) {
    warning("Warning: Specified limit in get_companies() is not a positive integer >= 1. Defaulting to 10.")
    limit = 10
  }
  conn = connect_db()
  query = glue::glue_sql("SELECT company_id, name, sector
                         FROM adem.companies
                         LIMIT {limit}", .con = conn)
  df <- DBI::dbGetQuery(conn, query)
  DBI::dbDisconnect(conn)
  return(df)
}




# get_company_details(company_id)
#
# company_id: integer
#
# list(company, vacancies)
#
# Returns company info and all its vacancies. Returns NULL if not found.
#
# get_company_details(42)

#' Obtains the details of a company, given its ID, and its published vacancies.
#'
#' @param company_id The ID of the company (an integer) whose details are to be retrieved.
#'
#' @returns A list containing a first data frame, with the details of the company, and a second data frame containing the details of each vacancy published by the company. In case an invalid company ID is provided, the function returns the data corresponding to company ID 1. If the company ID does not exist, the function returns NULL.
#' @export
#'
#' @examples
#' \dontrun{
#' get_company_details(42)
#' }
get_company_details <- function (company_id = NULL) {
  if ((is.na(company_id)) || (!is.numeric(company_id))) {
    warning("Warning: company_id in get_company_details() not specified or not an integer. Defaulting to: Company 1.")
    company_id = 1
  }
  conn = connect_db()
  query_company = glue::glue_sql("SELECT C.company_id, C.name, C.sector
                                    FROM adem.companies AS C
                                    WHERE C.company_id = {company_id}", .con = conn)
  df_company <- DBI::dbGetQuery(conn, query_company)
  if (is.null(df_company)) {
    DBI::dbDisconnect(conn)
    return(NULL)
  }
  query_comp_vac = glue::glue_sql("SELECT V.vacancy_id, V.canton, V.occupation, V.year, V.month
                                      FROM adem.companies AS C, adem.vacancies AS V
                                      WHERE C.company_id = V.company_id AND
                                      C.company_id = {company_id}", .con = conn)
  df_comp_vac <- DBI::dbGetQuery(conn, query_comp_vac)
  DBI::dbDisconnect(conn)
  return(list(df_company, df_comp_vac))
}


