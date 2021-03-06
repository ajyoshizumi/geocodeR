% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geocodeUSCB.R
\name{geocodeUSCB}
\alias{geocodeUSCB}
\title{A Function for Geocoding Using the US Census Bureau API}
\usage{
geocodeUSCB(
  Address,
  Street,
  City,
  State,
  Zip,
  Benchmark = "Public_AR_Current",
  SearchType = "onelineaddress"
)
}
\arguments{
\item{Address}{Used for one line addresses.}

\item{Street}{Used for separated addresses. Format is "#### RoadName RoadType".}

\item{City}{Used for separated addresses.}

\item{State}{Used for separated addresses.}

\item{Zip}{Used for seperated addresses.}

\item{Benchmark}{Indicates what version of the locater should be used. Defaults to "Public_AR_Current". For a list of other options please consult https://geocoding.geo.census.gov/geocoder/Geocoding_Services_API.pdf.}

\item{SearchType}{Indicates whether the search type is for a single line address ("onelineaddress") or a seperated address ("address"). Defaults to "onelineaddress".}
}
\description{
This function allows you to geocode addresses.
}
\examples{
geocodeUSCB(Address = "2800 Faucette Boulevard Raleigh NC 27607", SearchType = "onelineaddress")
geocodeUSCB(Street = "2800 Faucette Boulevard", City = "Raleigh", State = "NC", Zip = "27607", SearchType = "address")

geocodeUSCB()
}
