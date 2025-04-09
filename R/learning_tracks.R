
# get_learning_tracks(skill_id = NULL)
#
# skill_id: character (optional)
#
# data.frame with track_id, title, description, url
#
# Returns learning tracks. If skill_id is given, returns only tracks teaching that skill.
#
# get_learning_tracks(skill_id = "skill_r")

#' Obtains a list of the learning tracks, optionally filtered by a skill ID
#'
#' @param skill_id The ID (a string) of the skill to be used as a filter.
#'
#' @returns A data frame containing a list of all learning tracks, or, in case an (existing) skill ID is provided, it returns only those learning tracks matching that skill. NULL is returned if no learning track (matching the skill ID provided) can be found.
#' @export
#'
#' @examples
#' \dontrun{
#' get_learning_tracks(skill_id = "skill_r")
#' }
get_learning_tracks <- function (skill_id = NULL) {
  if ((is.na(skill_id)) || (!is.character(skill_id))) {
    warning("Warning: skill_id in get_learning_tracks() not specified or not a string. Defaulting to listing all learning tracks.")
    skill_id = "%"
  }
  conn = connect_db()
  query = glue::glue_sql("SELECT DISTINCT LT.track_id, LT.title, LT.description, LT.url
                         FROM adem.learning_tracks AS LT, adem.skills AS S, adem.track_skills AS TS
                         WHERE LT.track_id = TS.track_id AND
                         TS.skill_id = S.skill_id AND
                         S.skill_id LIKE {skill_id}", .con = conn)
  df <- DBI::dbGetQuery(conn, query)
  DBI::dbDisconnect(conn)
  if (nrow(df) == 0) {
    warning("Warning: No learning tracks found.")
    return(NULL)
  }
  return(df)
}



# get_learning_track_by_id(track_id)
#
# track_id: integer
#
# list(track, skills)
#
# Returns one learning track and all its linked skills. Returns NULL if not found.
#
# get_learning_track_by_id(71)

#' Obtains the details of a learning track, given its ID, and its associated skills.
#'
#' @param track_id The ID of the learning track whose details are to be retrieved.
#'
#' @returns A list of 2 data frames: first, contains the details of the learning track; second, contains the skils (details) associated to it. In case an invalid track ID is provided, the function returns data about track ID 1. If the track ID provided is not found, the function returns NULL.
#' @export
#'
#' @examples
#' \dontrun{
#' get_learning_track_by_id(71)
#' }
get_learning_track_by_id <- function (track_id = NULL) {
  if ((is.na(track_id)) || (!is.numeric(track_id))) {
    warning("Error: track_id in get_learning_track_by_id() not specified or not an integer. Defaulting to track ID = 1.")
    track_id = 1
  }
  conn = connect_db()
  query_track = glue::glue_sql("SELECT track_id, title, description, url
                                FROM adem.learning_tracks
                                WHERE track_id = {track_id}", .con = conn)
  df_track <- DBI::dbGetQuery(conn, query_track)
  if (nrow(df_track) == 0) {
    warning("Error: Track (ID) not found.")
    DBI::dbDisconnect(conn)
    return(NULL)
  }
  query_track_skills = glue::glue_sql("SELECT S.skill_id, S.skill_label
                                      FROM adem.learning_tracks AS LT, adem.track_skills AS TS, adem.skills AS S
                                      WHERE LT.track_id = TS.track_id AND
                                      TS.skill_id = S.skill_id AND
                                      LT.track_id = {track_id}", .con = conn)
  df_track_skills <- DBI::dbGetQuery(conn, query_track_skills)
  DBI::dbDisconnect(conn)
  return(list(df_track, df_track_skills))
}
