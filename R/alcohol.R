setwd(here::here("data"))
library(dplyr)
apparent_per_capita_alcohol_consumption <- get_alcohol_data()
save(apparent_per_capita_alcohol_consumption,
     file = "apparent_per_capita_alcohol_consumption.rda")
readr::write_csv(apparent_per_capita_alcohol_consumption,
                 path = "apparent_per_capita_alcohol_consumption.csv")
haven::write_dta(apparent_per_capita_alcohol_consumption,
                 path = "apparent_per_capita_alcohol_consumption.dta")

get_alcohol_data <- function() {
  setwd(here::here("data"))
  data <- pdftools::pdf_text("surveillance_report_113.pdf")
  data <- unlist(strsplit(data, split = "\n"))
  data <- trimws(data)
  data <- tolower(data)

  # end of page 17, table 4 starts next page
  data <- data[grep("^17$", data):length(data)]
  data <- data[-1]

  data <- stringr::str_split_fixed(data, "\\s{2,}", 6)
  data <- data.frame(data, stringsAsFactors = FALSE)
  data <- data[!grepl("^table|^decile|^state|^geographic|^ethanol|^[0-9]{2}$",
                      data$X1), ]

  data$X1 <- gsub("\\.+", "", data$X1)
  data$location <- data$X1
  data$location <- gsub("[0-9]", "", data$location)
  data$location <- trimws(data$location)
  data$location[data$location %in% ""] <- NA
  data$location <- zoo::na.locf(data$location)

  data <- data[!data$X5 %in% c("", ".", "all beverages"), ]
  data[, 1:6] <- sapply(data[, 1:6], trimws)
  data[, 2:6] <- sapply(data[, 2:6], readr::parse_number)

  names(data) <- c("year",
                   "ethanol_beer_gallons_per_capita",
                   "ethanol_wine_gallons_per_capita",
                   "ethanol_spirit_gallons_per_capita",
                   "ethanol_all_drinks_gallons_per_capita",
                   "decile",
                   "location")
  data <-
    data %>%
    dplyr::select(location,
                  year,
                  ethanol_beer_gallons_per_capita,
                  ethanol_wine_gallons_per_capita,
                  ethanol_spirit_gallons_per_capita,
                  ethanol_all_drinks_gallons_per_capita) %>%
    dplyr::rename(state = location) %>%
    # Source for ounces per drink!
    #https://www.niaaa.nih.gov/alcohol-health/overview-alcohol-consumption/what-standard-drink
    dplyr::mutate(# A gallon of beer is 0.045 gallons of ethanol.
      # 128 ounces per gallon.
      # 12 ounces per beer.
      number_of_beers = ethanol_beer_gallons_per_capita / 0.045 * 128 / 12,
      # A gallon of wine is 0.129 gallons of ethanol.
      # 128 ounces per gallon.
      # 5 ounces per glass of wine.
      number_of_glasses_wine = ethanol_wine_gallons_per_capita / 0.129 * 128 / 5,
      # A gallon of liquor is 0.411 gallons of ethanol.
      # 128 ounces per gallon.
      # 1.5 ounces per shot.
      number_of_shots_liquor = ethanol_spirit_gallons_per_capita / 0.411 * 128 / 1.5,
      number_of_drinks_total = ethanol_all_drinks_gallons_per_capita * 128 / 0.6)




  return(data)

}
