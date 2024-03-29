% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/doi.R
\name{regex_doi}
\alias{regex_doi}
\alias{doi_patterns}
\alias{str_extract_doi}
\alias{str_extract_all_doi}
\title{Find DOIs with regular expressions}
\usage{
regex_doi(type = c("doi.org", "cr-modern"), ...)

doi_patterns(type = c("doi.org", "cr-modern"))

str_extract_doi(string)

str_extract_all_doi(string, type = "doi.org")
}
\arguments{
\item{type}{a character string giving the type of validation to run.
Implemented as regular expressions (see source code).
Must be one these syntax specifications:
\itemize{
\item \code{"doi.org"} (recommended) from
\href{https://www.doi.org/doi_handbook/2_Numbering.html#2.2}{doi.org},
via \href{https://stackoverflow.com/questions/27910/finding-a-doi-in-a-document-or-page}{stack-overflow}
uses the actual spec, but can cause problems when DOIs are not separated
by whitespace or linebreaks, because many other characters
are valid DOI and will extracted.
\item \code{"cr-modern"} from
\href{https://www.crossref.org/blog/dois-and-matching-regular-expressions/}{crossref}
is less vulnerable to over-extracting, but excludes some DOIs which,
while today uncommon are syntactically valid.
See examples.
}}

\item{...}{
  Arguments passed on to \code{\link[stringr:modifiers]{stringr::regex}}
  \describe{
    \item{\code{pattern}}{Pattern to modify behaviour.}
    \item{\code{ignore_case}}{Should case differences be ignored in the match?}
    \item{\code{multiline}}{If \code{TRUE}, \code{$} and \code{^} match
the beginning and end of each line. If \code{FALSE}, the
default, only match the start and end of the input.}
    \item{\code{comments}}{If \code{TRUE}, white space and comments beginning with
\code{#} are ignored. Escape literal spaces with \code{\\ }.}
    \item{\code{dotall}}{If \code{TRUE}, \code{.} will also match line terminators.}
  }}

\item{string}{Input vector. Either a character vector, or something
coercible to one.}
}
\description{
Find DOIs with regular expressions
}
\section{Related Functions and Methods}{
\subsection{Functions}{
\itemize{
\item \code{doi_patterns}: Find DOI fields with regular expressions
}
\itemize{
\item \code{str_extract_doi}: Extract \emph{first} DOIs from character strings
}
\itemize{
\item \code{str_extract_all_doi}: Extract \emph{all} DOIs from character strings
}}}

\examples{
regex_doi("doi.org")
regex_doi("cr-modern")

str_extract_doi(string = c(
  "10.1594/PANGAEA.726855",  # nothing to do here
  "10.1119/1.16433 ",  # remove space
  " 10.1594/PANGAEA.667386", # remove space
  "doi:10.3866/PKU.WHXB201112303", # remove DOI
  "http://dx.doi.org/10.3352/jeehp.2013.10.3", # parse URL
  "10.3972/water973.0145.db&", # remove forbidden symbol
  "foo bar" # no DOI here
))
str_extract_all_doi(string = c(
  # nothing to do here
  "10.17487/rfc1149",
    # space separated
  "10.1016/j.iheduc.2003.11.004 doi:10.7875/leading.author.2.e008",
  # separated by forbidden
  "doi:10.6084/m9.figshare.97218&doi:10.1126/science.169.3946.635 ",
  # separated by linebreak
  "10.5194/wes-2019-70\n10.5194/wes-5-819-202",
  # no DOI here
  "quux"
))
}
\seealso{
Other doi: 
\code{\link{doiEntry}},
\code{\link{doi_api}},
\code{\link{doi_examples}()},
\code{\link{doi_ra}},
\code{\link{doi}()},
\code{\link{view_doi_matches}()}
}
\concept{doi}
