
# get_books(skill = NULL)
#
# skill: character (optional)
#
# data.frame with book_id, title, author, skill_id
#
# Returns all recommended books. Filter by skill if needed.
#
# get_books(skill = "skill_python")

#' Obtain list of the books, optionally filtered by a skill name.
#'
#' @param skill The skill name (label) (a string) describing the skill.
#'
#' @returns A data frame with the details of all books, or otherwise, if a skill name (even if partial) is provided, then it returns book details matching that skill. NULL is returned if no books are found (matching the skill provided).
#' @export
#'
#' @examples
#' \dontrun{
#' get_books(skill = "skill_python")
#' }
get_books <- function (skill = NULL) {
  if ((is.na(skill)) || (!is.character(skill))) {
    warning("Warning: skill in get_books() not specified or not a string. Defaulting to displaying books related to any skill.")
    skill = "%"
  }
  conn = connect_db()
  query = glue::glue_sql("SELECT BR.book_id, BR.title, BR.author, BR.skill_id
                          FROM adem.book_recommendations AS BR, adem.skills AS S
                          WHERE BR.skill_id = S.skill_id AND
                          S.skill_label LIKE {skill}", .con = conn) # improvement: allow for searching by partial skill_label
  df <- DBI::dbGetQuery(conn, query)
  DBI::dbDisconnect(conn)
  if (nrow(df) == 0) {
    warning("Warning: No books found.")
    return(NULL)
  }
  return(df)
}




# get_book_by_id(book_id)
#
# book_id: integer
#
# data.frame with one row
#
# Returns a list of recommended books. You can filter them by skill (optional)
#
# get_book_by_id(101)

#' Obtains the details of a book, given its ID.
#'
#' @param book_id The ID of the book whose details are to be retrieved.
#'
#' @returns A data frame containing the book details. In case an invalid or missing book ID is provided, the function provides the details of book ID 1.
#' @export
#'
#' @examples
#' \dontrun{
#' get_book_by_id(101)
#' }
get_book_by_id <- function (book_id = NULL) {
  if ((is.na(book_id)) || (!is.numeric(book_id))) {
    warning("Error: book_id in get_book_by_id() not specified or not an integer. Defaulting to book ID = 1.")
    book_id = 1
  }
  conn = connect_db()
  query = glue::glue_sql("SELECT book_id, title, author, skill_id
                          FROM adem.book_recommendations
                          WHERE book_id = {book_id}", .con = conn)
  df <- DBI::dbGetQuery(conn, query)
  DBI::dbDisconnect(conn)
  if (nrow(df) == 0) {
    warning("Warning: Book not found.")
    return(NULL)
  }
  return(df)
}


