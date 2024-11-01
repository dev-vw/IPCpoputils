% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_popchlor.R
\name{plot_chlor}
\alias{plot_chlor}
\title{Plot a population choropleth map}
\usage{
plot_chlor(joined_df, country, yr, breaks = NULL)
}
\arguments{
\item{country}{A string}

\item{breaks}{A vector of breaks}

\item{adm_sf}{A shapefile with \code{GEO_MATCH} or (equivalent) attribute.}

\item{pop_data}{A data frame containing population data}

\item{year}{A numeric specifying year}
}
\value{
A choropleth plot
}
\description{
Given an ADM shapefile (adm_sf) and population data (pop_data),
returns a choropleth plot containing scaled population values. Defaults to
log transformed population.
}
\author{
Vania Wang
}
