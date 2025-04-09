# get_skills(limit = 100)
#
# limit: integer
#
# data.frame with skill_id, skill_label
#
# Returns all skills from adem.skills. Limited by limit.
#
# get_skills(limit = 20)

#' Obtain a list of the skills.
#'
#' @param limit The size (an integer) of the list of skills to be returned.
#'
#' @returns A data frame with the list of the skills and corresponding details. List can be limited using limit argument.
#' @export
#'
#' @examples
#' \dontrun{
#' get_skills(limit = 20)
#' }
get_skills <- function (limit = 100) {
  if (!is.numeric(limit) || (limit < 1)) {
    warning("Warning: Specified limit in get_skills() is not a positive integer >= 1. Defaulting to 10.")
    limit = 10
  }
  conn = connect_db()
  query = glue::glue_sql("SELECT skill_id, skill_label
                         FROM adem.skills
                         LIMIT {limit}", .con = conn)
  df <- DBI::dbGetQuery(conn, query)
  DBI::dbDisconnect(conn)
  return(df)
}


#
# get_skill_by_id(skill_id)
#
# skill_id: character
#
# data.frame with skill_id, skill_label
#
# Returns a single skill based on its ID.
#
# get_skill_by_id("skill_r")

#' Obtains the details of a skill, given its ID
#'
#' @param skill_id The ID of skill (a string) whose details are to be retrieved.
#'
#' @returns A data frame containing the skill details. If no skill ID or an invalid skill ID is provided, then it returns the details about the Communication skill.
#' @export
#'
#' @examples
#' \dontrun{
#' get_skill_by_id("skill_r")
#' }
get_skill_by_id <- function (skill_id = NULL) {
  if ((is.na(skill_id)) || (!is.character(skill_id))) {
    warning("Warning: skill_id in get_skill_by_id() not specified or not a string. Defaulting to skill: Communication.")
    skill_id = "http://data.europa.eu/esco/skill/15d76317-c71a-4fa2-aadc-2ecc34e627b7"
  }
  conn = connect_db()
  query = glue::glue_sql("SELECT skill_id, skill_label
                         FROM adem.skills
                         WHERE skill_id = {skill_id}", .con = conn)
  df <- DBI::dbGetQuery(conn, query)
  DBI::dbDisconnect(conn)
  return(df)
}
