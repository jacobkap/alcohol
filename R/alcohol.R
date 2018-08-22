setwd("C:/Users/user/Dropbox/R_project/alcohol/data")
alcohol_data <- get_alcohol_data()
save(alcohol_data, file = "alcohol_data.rda")
readr::write_csv(alcohol_data, path = "alcohol_data.csv")

get_alcohol_data <- function() {
  setwd("C:/Users/user/Dropbox/R_project/alcohol/data")
data <- pdftools::pdf_text("surveillance_report_110.pdf")
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
data <- data[, c("location",
                 "year",
                 "ethanol_beer_gallons_per_capita",
                 "ethanol_wine_gallons_per_capita",
                 "ethanol_spirit_gallons_per_capita",
                 "ethanol_all_drinks_gallons_per_capita")]
return(data)

}
