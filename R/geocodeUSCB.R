#' A Function for Geocoding Using the US Census Bureau API
#'
#' This function allows you to geocode addresses.
#' @param Address Used for one line addresses.
#' @param Street Used for separated addresses. Format is "#### RoadName RoadType".
#' @param City Used for separated addresses.
#' @param State Used for separated addresses.
#' @param Zip Used for seperated addresses.
#' @param Benchmark Indicates what version of the locater should be used. Defaults to "Public_AR_Current". For a list of other options please consult https://geocoding.geo.census.gov/geocoder/Geocoding_Services_API.pdf.
#' @param SearchType Indicates whether the search type is for a single line address ("onelineaddress") or a seperated address ("address"). Defaults to "onelineaddress".
#' @export
#' @examples geocodeUSCB(Address = "2800 Faucette Boulevard Raleigh NC 27607", SearchType = "onelineaddress")
#' @examples geocodeUSCB(Street = "2800 Faucette Boulevard", City = "Raleigh", State = "NC", Zip = "27607", SearchType = "address")
#'
#' geocodeUSCB()

# Geocoding function that leverages US Census Bureau
geocodeUSCB <- function(Address,Street,City,State,Zip,
                        Benchmark = "Public_AR_Current",
                        SearchType = "onelineaddress"){

  # Define url to be contacted.
  urlAddress <- paste(
    "https://geocoding.geo.census.gov/geocoder/locations/",
    SearchType,
    "?",
    sep = ""
  )

  # Change query based on search type.
  if(SearchType == "address"){

    # Query the US Census Bureau's geocoding service.
    r <- GET(url = urlAddress,
             query = list(
               street = Street,
               city = City,
               state = State,
               zip = Zip,
               benchmark = Benchmark
             )
    )

  } else if (SearchType == "onelineaddress"){

    # Query the US Census Bureau's geocoding service.
    r <- GET(url = urlAddress,
             query = list(
               address = Address,
               benchmark = Benchmark
             )
    )

  } else if (SearchType != "address" | SearchType != "onelineaddress"){

    # Return error if search type is not valid.
    stop("Invalid search type specified.")

  }

  # Store content of the response as text.
  c <- content(x = r, type = "text", encoding = "UTF-8")

  # Read JSON structure of text into a list.
  geoList <- fromJSON(txt = c)

  # Assign key variables.
  lon <- geoList$result$addressMatches$coordinates$x[1]
  lat <- geoList$result$addressMatches$coordinates$y[1]

  # Knit into data frame row.
  entry <- list(Longitude = lon,
                Latitude = lat)

  return(entry)
}
