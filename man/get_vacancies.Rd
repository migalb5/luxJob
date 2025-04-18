% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vacancies.R
\name{get_vacancies}
\alias{get_vacancies}
\title{Obtains list of the vacancies according to filtering arguments and size limit.}
\usage{
get_vacancies(skill = NULL, company = NULL, canton = NULL, limit = 100)
}
\arguments{
\item{skill}{The name (label) (a string) of the skill to be used as a filter.}

\item{company}{The name of the company (a string) to be used as a filter.}

\item{canton}{The name of the canton (a string) to be used as a filter.}

\item{limit}{The size (an integer) of the list of vacancies to be returned.}
}
\value{
A data frame containing the vacancies (and corresponding details) matching the search criteria specified (conjunction). All search criterion are optional. NULL is returned if no vacancies (meeting the criteria) are found.
}
\description{
Obtains list of the vacancies according to filtering arguments and size limit.
}
\examples{
\dontrun{
get_vacancies(skill = "skill_python", canton = "Luxembourg")
}
}
