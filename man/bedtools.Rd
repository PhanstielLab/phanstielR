% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bedtools.R
\name{bedtools}
\alias{bedtools}
\title{performs bedtools intersections}
\usage{
bedtools(action = "intersect", args = "-wo", a, b, acols = NA,
  bcols = NA, ahead = FALSE, bhead = FALSE, count = FALSE,
  tmpdir = "~/scratch/", columnfilt = NA, test = FALSE)
}
\arguments{
\item{action}{the bedtools function to be used (Default=intersect)}

\item{args}{any arguments to be added (e.g. -wo) (Default=-wo)}

\item{a}{a filename or data frame to be used a set A}

\item{b}{a filename or data frame to be used a set B}

\item{acols}{which columns from set A to use}

\item{bcols}{which columns from set B to use}

\item{ahead}{boolean whether set A has a header (only relevant for is a is a filename) (Default=F)}

\item{bhead}{boolean whether set B has a header (only relevant for is b is a filename) (Default=F)}

\item{count}{boolean whether to return the results of the intersection or just a line count of the result}

\item{tmpdir}{inset from the top of the blot as a fraction of the plot width}

\item{columnfilt}{The number of columns to return}

\item{test}{boolean, if TRUE the function will print out the bedtools command but not execute it}
}
\description{
Bedtools needs to be in your system PATH
}
