% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/doi.R
\name{doiEntry}
\alias{doiEntry}
\alias{doiEntryApp}
\alias{doiEntryUI}
\alias{doiEntryServer}
\alias{doi_entry_translator}
\title{Enter DOIs through a Shiny Module}
\usage{
doiEntryApp()

doiEntryUI(id, translator = NULL, width = "100\%", height = "200px")

doiEntryServer(
  id,
  example_dois = doi_examples(),
  char_limit = 900L,
  translator = NULL,
  lang = shiny::reactive("en"),
  millis = 500L
)

doi_entry_translator()
}
\arguments{
\item{id}{The id string to be namespaced (optional).}

\item{translator}{A \link[shiny.i18n:Translator]{shiny.i18n::Translator} object or \code{NULL} for english-only defaults.
Strings inside the module UI are marked as translateable.
You can pass a translator object included in the package,
or can create your own \code{translator} using \link[shiny.i18n:Translator]{shiny.i18n::Translator}.
This must not be a reactive, it is only set at shiny startup.
To update the language reactively \emph{during} a shiny session, see \code{lang}.}

\item{width}{The width of the input, e.g. \code{'400px'}, or \code{'100\%'};
see \code{\link[shiny:reexports]{validateCssUnit()}}.}

\item{height}{The height of the input, e.g. \code{'400px'}, or \code{'100\%'}; see
\code{\link[shiny:reexports]{validateCssUnit()}}.}

\item{example_dois}{A vector created by, or convertable to \code{\link[=doi]{doi()}},
to be used as as examples.
To initiate the UI with the example,
pass them to \code{\link[=doiEntryUI]{doiEntryUI()}}.}

\item{char_limit}{Integer scalar, giving the maximum number of characters.
To protect shiny against overlong strings, you can limit the maximum
length of strings allowed.
This limit is still enforced server-side, not client-side,
so the protection is not bullet-proof.}

\item{lang}{a reactive variable, returning a character scalar.
Must be one of the languages in \code{translator}.
Defaults to \code{shiny::reactive("en")},
in which case no server side translation
is triggered.}

\item{millis}{The debounce/throttle time window. You may optionally pass a
no-arg function or reactive expression instead, e.g. to let the end-user
control the time window.}
}
\value{
An object of class \code{biblids_doi} as returned by \code{\link[=doi]{doi()}}.

a \link[shiny.i18n:Translator]{shiny.i18n::Translator} object.
}
\description{
Input, validate and return DOIs in a shiny app.
}
\section{Related Functions and Methods}{
\subsection{Functions}{
\itemize{
\item \code{doiEntryApp}: Test app
}
\itemize{
\item \code{doiEntryUI}: Module UI
}
\itemize{
\item \code{doiEntryServer}: Module server
}
\itemize{
\item \code{doi_entry_translator}: Translator
Translations shipping with the package,
including en, de.

To find the keys you need to include in your own translations,
look at \code{biblids::doi_entry_translator()$translations()}.
}}}

\seealso{
Other doi: 
\code{\link{doi_api}},
\code{\link{doi_examples}()},
\code{\link{doi_ra}},
\code{\link{doi}()},
\code{\link{regex_doi}()},
\code{\link{view_doi_matches}()}
}
\concept{doi}
