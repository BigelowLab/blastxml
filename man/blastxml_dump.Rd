% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dump.R
\name{blastxml_dump}
\alias{blastxml_dump}
\title{Extract blast XML contents possibly writing to a text file}
\usage{
blastxml_dump(xml_file = get_example_filename(), out_file = NULL,
  sep = "\\t", bxd = NULL, fields = NULL, form = c("matrix",
  "tibble")[1], ...)
}
\arguments{
\item{xml_file}{character - the path and name of the XML file}

\item{out_file}{character the path and name to output file, 
or NULL to skip saving the file}

\item{sep}{charcater field separator if writing to file}

\item{bxd}{list of known fields for the blast type, see \code{link{BXD}}}

\item{fields}{character vector of fields to extract, defaults to all in bxd}

\item{form}{character, either matrix or tibble}

\item{...}{further arguments for \code{\link{write.table}}}
}
\value{
character matrix or tibble invisibly
}
\description{
Extract blast XML contents possibly writing to a text file
}
