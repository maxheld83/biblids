% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/doi.R
\name{doi}
\alias{doi}
\alias{is_doi}
\alias{is_doi_ish}
\alias{as_doi}
\alias{format.biblids_doi}
\alias{pillar_shaft.biblids_doi}
\alias{knit_print.biblids_doi}
\alias{is.na.biblids_doi}
\title{Digital Object Identifiers}
\usage{
doi(prefix = character(), suffix = character())

is_doi(x)

is_doi_ish(x)

as_doi(x, ...)

\method{format}{biblids_doi}(x, ..., protocol = FALSE)

\method{pillar_shaft}{biblids_doi}(x, ...)

\method{knit_print}{biblids_doi}(
  x,
  display = getOption("biblids.doi_display", default = "crossref"),
  inline = FALSE,
  ...
)

\method{is.na}{biblids_doi}(x, ...)
}
\arguments{
\item{prefix}{The naming authority.}

\item{suffix}{The unique string chosen by the registrant.}

\item{x}{A vector created by, or convertable to \code{\link[=doi]{doi()}}.}

\item{...}{Additional arguments passed to the S3 method. Currently ignored,
except two optional arguments \code{options} and \code{inline}; see
the references below.}

\item{protocol}{Logical flag, whether to prepend \verb{doi:} handle protocol,
as per the official
\href{https://doi.org/doi_handbook/2_Numbering.html#2.6.1}{DOI Handbook}.}

\item{display}{character scaling, giving how to display a DOI.
Must be one of:
\itemize{
\item \code{"crossref"} (recommended)
to apply their
\href{https://www.crossref.org/education/metadata/persistent-identifiers/doi-display-guidelines/}{display guidelines}.
Appears identical to the
\href{https://support.datacite.org/docs/datacite-doi-display-guidelines}{DataCite display guidelines}.
\item \code{"doi"}:
to apply the DOI Foundation
\href{https://www.doi.org/doi_handbook/2_Numbering.html#2.6}{presentation guidelines}.
}}

\item{inline}{Logical flag, giving whether to render DOIs as a chunk output or inline R.
Usually set by knitr.}
}
\description{
S3 record class for DOIs.
}
\section{Related Functions and Methods}{
\subsection{Functions}{
\itemize{
\item \code{is_doi}: Is this a \code{biblids_doi}?
}
\itemize{
\item \code{is_doi_ish}: Could this be converted to a \code{biblids_doi} using \code{\link[=as_doi]{as_doi()}}?
}
\itemize{
\item \code{as_doi}: Normalise
}}
\subsection{Methods extending \code{biblids_doi} class (by generic):}{
\itemize{
\item \code{format}: Display a DOI
}
\itemize{
\item \code{pillar_shaft}: Print DOIs in \code{\link[tibble:tibble]{tibble::tibble()}}s
}
\itemize{
\item \code{knit_print}: Print DOIs in R markdown (when knitr is available):
DOIs are hyperlined to the doi.org resolution service.\if{html}{\out{<div class="r">}}\preformatted{library(knitr)
# defaults to crossref style (recommended)
doi_examples(na.rm = FALSE)[1:3]
}\if{html}{\out{</div>}}
\itemize{
\item \url{https://doi.org/10.1038/nphys1170}
\item \code{NA}
\item \url{https://doi.org/10.1007/978-3-642-65840-2_5}
}\if{html}{\out{<div class="r">}}\preformatted{# or use doi style
knitr::knit_print(doi_examples(na.rm = FALSE)[1:3], display = "doi")
}\if{html}{\out{</div>}}
\itemize{
\item \href{https://doi.org/10.1038/nphys1170}{\code{doi:10.1038/nphys1170}}
\item \code{NA}
\item \href{https://doi.org/10.1007/978-3-642-65840-2_5}{\verb{doi:10.1007/978-3-642-65840-2_5}}
}

You can also include DOIs inline with \url{https://doi.org/10.1038/nphys1170}, \url{https://doi.org/10.1007/978-3-642-65840-2_5} and \url{https://doi.org/10.1594/PANGAE.726855}.
}
\itemize{
\item \code{is.na}: Detect if \code{prefix} and/\emph{or} \code{suffix} is missing
}}}

\note{
DOIs are returned as an S3 record class constructed by \code{\link[vctrs:new_rcrd]{vctrs::new_rcrd()}}.
Under the hood, these records are implemented as \emph{lists} of fields
(here: prefix, suffix).
Support for such records may still be limited.
For example, \code{\link[purrr:map]{purrr::map()}} will erroneously loop over the fields,
instead of over the DOIs (see \href{https://github.com/subugoe/biblids/issues/51}{#51}).
To avoid such problems,
cast the DOI to a simple character vector using \code{\link[=as.character]{as.character()}}.
}
\examples{
# this is the hard way to enter
doi(
  prefix = c("10.1038", "10.1000", "10.1007"),
  suffix = c("nphys1170", NA, "978-3-642-65840-2_5")
)
# DOIs are case insensitive and are compared as such
unique(as_doi(c("10.1000/foo", "10.1000/fOo")))
as_doi("10.1000/BAR") == as_doi("10.1000/bar")

# convert back to a (normalised) character
as.character(as_doi("10.1000/zap"))

is_doi(as_doi("10.1000/1"))
is_doi(1L)
as_doi(c(
  # example DOIs are from https://www.doi.org/demos.html
  "10.1594/PANGAE.726855",
  " 10.1594/GFZ.GEOFON.gfz2009kciu ",  # leading/trailing spaces are removed
  "https://doi.org/10.1000/182",  # URL form is parsed
  "doi:10.1000/7",  # DOI from is parsed
  "foo bar",  # returns NA
  NA_character_  # returns NA
))
\dontrun{
# there must be only one DOI per element
as_doi(c("10.1126/science.169.3946.635 10.6084/m9.figshare.97218"))
}
# there is extra pretty printing inside tibbles
tibble::tibble(c(doi_examples(na.rm = FALSE)[1:3]))
# this can be constructed but will be NA
is.na(doi(prefix = "10.1000", suffix = NA))
}
\seealso{
Other doi: 
\code{\link{doiEntry}},
\code{\link{doi_api}},
\code{\link{doi_examples}()},
\code{\link{doi_ra}},
\code{\link{regex_doi}()},
\code{\link{view_doi_matches}()}
}
\concept{doi}
