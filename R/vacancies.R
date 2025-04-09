
# get_vacancies(skill = NULL, company = NULL, canton = NULL, limit = 100)
#
# skill: character<br>company: integer<br>canton: character<br>limit: integer
#
# data.frame with vacancy_id, company_id, occupation, canton, year, month
#
# Returns filtered list of job vacancies. You can combine filters.
#
# get_vacancies(skill = "skill_python", canton = "Luxembourg")

#' Obtains list of the vacancies according to filtering arguments and size limit.
#'
#' @param skill The name (label) (a string) of the skill to be used as a filter.
#' @param company The name of the company (a string) to be used as a filter.
#' @param canton The name of the canton (a string) to be used as a filter.
#' @param limit The size (an integer) of the list of vacancies to be returned.
#'
#' @returns A data frame containing the vacancies (and corresponding details) matching the search criteria specified. All search criterion are optional. NULL is returned if no vacancies (meeting the criteria) are found.
#' @export
#'
#' @examples
#' \dontrun{
#' get_vacancies(skill = "skill_python", canton = "Luxembourg")
#' }
get_vacancies <- function (skill = NULL, company = NULL, canton = NULL, limit = 100) {
  if ((is.na(company)) || (!is.character(company))) {
    warning("Warning: company in get_vacancies() is not a string (or not specified). Defaulting to: NULL (any value).")
    company = "%"
  }
  if ((is.na(skill)) || (!is.character(skill))) {
    warning("Warning: skill in get_vacancies() is not a string (or not specified). Defaulting to: NULL (any value).")
    skill = "%"
  }
  if ((is.na(canton)) || (!is.character(canton))) {
    warning("Warning: canton in get_vacancies() is not a string (or not specified). Defaulting to: NULL (any value).")
    canton = "%"
  }
  if ((is.na(limit)) || (!is.numeric(limit)) || (limit < 1)) {
    warning("Warning: company_id in get_vacancies() is not a positive integer >= 1 (or not specified). Defaulting to 100.")
    limit = 100
  }
  conn = connect_db()
  query = glue::glue_sql("SELECT DISTINCT V.vacancy_id, C.name, V.occupation, V.canton, V.year, V.month
                          FROM adem.skills AS S,
                          adem.vacancy_skills AS VS,
                          adem.vacancies AS V,
                          adem.companies AS C
                          WHERE (C.name LIKE {company} AND V.company_id = C.company_id) AND
                          (S.skill_label LIKE {skill} AND  S.skill_id = VS.skill_id AND VS.vacancy_id = V.vacancy_id) AND
                          V.canton LIKE {canton}
                          LIMIT {limit}", .con = conn)
  df <- DBI::dbGetQuery(conn, query)
  DBI::dbDisconnect(conn)
  if (nrow(df) == 0) {
    warning("Warning: No vacancies found.")
    return(NULL)
  }
  return(df)
}



# get_vacancy_by_id(vacancy_id)
#
# vacancy_id: integer
#
# list(vacancy, skills)
#
# Returns full vacancy info + required skills. Returns NULL if not found.
#
# get_vacancy_by_id(123456)

#' Obtains the details of a vacancy, given its ID, and the skills associated to it.
#'
#' @param vacancy_id The ID of the vacancy whose details are to be retrieved.
#'
#' @returns A list of 2 data frames: first, contains the vacancy details; second, contains list of associated skills. In case of an invalid or not found vacancy ID, the function returns NULL.
#' @export
#'
#' @examples
#' \dontrun{
#' get_vacancy_by_id(123456)
#' }
get_vacancy_by_id <- function (vacancy_id = NULL) {
  if ((is.na(vacancy_id)) || (!is.numeric(vacancy_id))) {
    warning("Error: vacancy_id in get_vacancy_by_id() not specified or not an integer. Vacancy not found.")
    return(NULL)
  }
  conn = connect_db()
  query_vac = glue::glue_sql("SELECT V.vacancy_id, V.company_id, V.canton, V.occupation, V.year, V.month
                              FROM adem.vacancies AS V
                              WHERE V.vacancy_id = {vacancy_id}", .con = conn)
  df_vac <- DBI::dbGetQuery(conn, query_vac)
  if (nrow(df_vac) == 0) {
    warning("Error: Vacancy (ID) not found.")
    DBI::dbDisconnect(conn)
    return(NULL)
  }
  query_skills = glue::glue_sql("SELECT S.skill_id, S.skill_label
                                FROM adem.skills AS S, adem.vacancy_skills AS VS, adem.vacancies AS V
                                WHERE S.skill_id = VS.skill_id AND
                                VS.vacancy_id = V.vacancy_id AND
                                V.vacancy_id = {vacancy_id}", .con = conn)
  df_skills <- DBI::dbGetQuery(conn, query_skills)
  DBI::dbDisconnect(conn)
  return(list(df_vac, df_skills))
}

