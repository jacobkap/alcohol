context("pdf_scrape_check")

data     <- get_alcohol_data()
cali     <- data[data$state == "california",]
delaware <- data[data$state == "delaware",]
hawaii   <- data[data$state == "hawaii",]
penn     <- data[data$state == "pennsylvania",]
texas    <- data[data$state == "texas",]
utah     <- data[data$state == "utah",]
usa      <- data[data$state == "us total",]

test_that("beer consumption", {
  expect_equal(head(data$ethanol_beer_gallons_per_capita), c(1.09, 1.11, 1.13,
                                                             1.14, 1.16, 1.16))
  expect_equal(tail(data$ethanol_beer_gallons_per_capita), c(1.38, 1.39, 1.37,
                                                             1.34, 1.3, 1.27))

  expect_equal(head(cali$ethanol_beer_gallons_per_capita), c(0.96, 0.99, 1,
                                                             0.98, 0.98, 0.95))
  expect_equal(tail(cali$ethanol_beer_gallons_per_capita), c(1.37, 1.43, 1.41,
                                                             1.39, 1.33, 1.29))

  expect_equal(head(delaware$ethanol_beer_gallons_per_capita), c(1.2, 1.18, 1.21,
                                                                 1.25, 1.32, 1.34))
  expect_equal(tail(delaware$ethanol_beer_gallons_per_capita), c(1.54, 1.52, 1.44,
                                                                 1.35, 1.31, 1.3))

  expect_equal(head(hawaii$ethanol_beer_gallons_per_capita), c(1.29, 1.28, 1.27,
                                                               1.25, 1.26, 1.25))
  expect_equal(tail(hawaii$ethanol_beer_gallons_per_capita), c(1.8, 1.74, 1.64,
                                                               1.51, 1.33, 1.55))

  expect_equal(head(penn$ethanol_beer_gallons_per_capita), c(1.31, 1.31, 1.32,
                                                             1.34, 1.36, 1.37))
  expect_equal(tail(penn$ethanol_beer_gallons_per_capita), c(1.44, 1.42, 1.45,
                                                             1.4, 1.35, 1.34))

  expect_equal(head(texas$ethanol_beer_gallons_per_capita), c(1.3, 1.28, 1.25,
                                                              1.3, 1.33, 1.35))
  expect_equal(tail(texas$ethanol_beer_gallons_per_capita), c(1.83, 1.84, 1.75,
                                                              1.7, 1.64, 1.59))

  expect_equal(head(utah$ethanol_beer_gallons_per_capita), c(0.63, 0.65, 0.66,
                                                             0.67, 0.69, 0.68))
  expect_equal(tail(utah$ethanol_beer_gallons_per_capita), c(1.01, 1.02, 0.96,
                                                             0.94, 0.93, 0.94))

  expect_equal(head(usa$ethanol_beer_gallons_per_capita), c(1.08, 1.09, 1.1,
                                                            1.12, 1.13, 1.12))
  expect_equal(tail(usa$ethanol_beer_gallons_per_capita), c(1.38, 1.39, 1.37,
                                                            1.34, 1.3, 1.27))

})


test_that("wine consumption", {
  expect_equal(head(data$ethanol_wine_gallons_per_capita), c(0.26, 0.26, 0.25,
                                                             0.25, 0.24, 0.24))
  expect_equal(tail(data$ethanol_wine_gallons_per_capita), c(0.36, 0.35, 0.34,
                                                             0.31, 0.3, 0.29))

  expect_equal(head(cali$ethanol_wine_gallons_per_capita), c(0.59, 0.53, 0.6,
                                                             0.6, 0.63, 0.58))
  expect_equal(tail(cali$ethanol_wine_gallons_per_capita), c(0.72, 0.72, 0.71,
                                                             0.68, 0.67, 0.65))

  expect_equal(head(delaware$ethanol_wine_gallons_per_capita), c(0.73, 0.72, 0.72,
                                                                 0.72, 0.69, 0.69))
  expect_equal(tail(delaware$ethanol_wine_gallons_per_capita), c(0.34, 0.33, 0.29,
                                                                 0.26, 0.24, 0.23))

  expect_equal(head(hawaii$ethanol_wine_gallons_per_capita), c(0.57, 0.56, 0.54,
                                                               0.54, 0.53, 0.53))
  expect_equal(tail(hawaii$ethanol_wine_gallons_per_capita), c(0.48, 0.44, 0.46,
                                                               0.42, 0.44, 0.35))

  expect_equal(head(penn$ethanol_wine_gallons_per_capita), c(0.33, 0.32, 0.32,
                                                             0.31, 0.31, 0.3))
  expect_equal(tail(penn$ethanol_wine_gallons_per_capita), c(0.23, 0.23, 0.22,
                                                             0.22, 0.21, 0.2))

  expect_equal(head(texas$ethanol_wine_gallons_per_capita), c(0.34, 0.33, 0.33,
                                                              0.32, 0.32, 0.31))
  expect_equal(tail(texas$ethanol_wine_gallons_per_capita), c(0.18, 0.24, 0.22,
                                                              0.14, 0.14, 0.14))

  expect_equal(head(utah$ethanol_wine_gallons_per_capita), c(0.19, 0.20, 0.19,
                                                             0.19, 0.19, 0.18))
  expect_equal(tail(utah$ethanol_wine_gallons_per_capita), c(0.15, 0.15, 0.14,
                                                             0.15, 0.14, 0.13))

  expect_equal(head(usa$ethanol_wine_gallons_per_capita), c(0.44, 0.42, 0.43,
                                                            0.42, 0.42, 0.41))
  expect_equal(tail(usa$ethanol_wine_gallons_per_capita), c(0.36, 0.35, 0.34,
                                                            0.31, 0.3, 0.29))

})


test_that("spirit consumption", {
  expect_equal(head(data$ethanol_spirit_gallons_per_capita), c(0.65, 0.64, 0.62,
                                                               0.6, 0.6, 0.59))
  expect_equal(tail(data$ethanol_spirit_gallons_per_capita), c(0.98, 1.02, 1.04,
                                                               1.04, 1.05, 1.04))

  expect_equal(head(cali$ethanol_spirit_gallons_per_capita), c(0.78, 0.75, 0.73,
                                                               0.75, 0.74, 0.72))
  expect_equal(tail(cali$ethanol_spirit_gallons_per_capita), c(1.13, 1.2, 1.24,
                                                               1.25, 1.29, 1.25))

  expect_equal(head(delaware$ethanol_spirit_gallons_per_capita), c(1.79, 1.74, 1.66,
                                                                   1.63, 1.58, 1.52))
  expect_equal(tail(delaware$ethanol_spirit_gallons_per_capita), c(1.38, 1.4, 1.37,
                                                                   1.31, 1.3, 1.33))

  expect_equal(head(hawaii$ethanol_spirit_gallons_per_capita), c(0.77, 0.77, 0.77,
                                                                 0.77, 0.75, 0.73))
  expect_equal(tail(hawaii$ethanol_spirit_gallons_per_capita), c(1.14, 1.16, 1.2,
                                                                 1.21, 1.25, 1.21))

  expect_equal(head(penn$ethanol_spirit_gallons_per_capita), c(0.72, 0.7, 0.7,
                                                               0.68, 0.67, 0.66))
  expect_equal(tail(penn$ethanol_spirit_gallons_per_capita), c(0.7, 0.72, 0.71,
                                                               0.72, 0.73, 0.74))

  expect_equal(head(texas$ethanol_spirit_gallons_per_capita), c(0.69, 0.67, 0.65,
                                                                0.63, 0.63, 0.61))
  expect_equal(tail(texas$ethanol_spirit_gallons_per_capita), c(0.84, 0.85, 0.8,
                                                                0.84, 0.82, 0.78))

  expect_equal(head(utah$ethanol_spirit_gallons_per_capita), c(0.52, 0.54, 0.52,
                                                               0.51, 0.5, 0.49))
  expect_equal(tail(utah$ethanol_spirit_gallons_per_capita), c(0.55, 0.59, 0.6,
                                                               0.59, 0.59, 0.57))

  expect_equal(head(usa$ethanol_spirit_gallons_per_capita), c(0.83, 0.81, 0.8,
                                                              0.79, 0.78, 0.76))
  expect_equal(tail(usa$ethanol_spirit_gallons_per_capita), c(0.98, 1.02, 1.04,
                                                              1.04, 1.05, 1.04))

})


test_that("total consumption", {
  expect_equal(head(data$ethanol_all_drinks_gallons_per_capita), c(2.01, 2.01, 2,
                                                                   1.99, 2, 1.98))
  expect_equal(tail(data$ethanol_all_drinks_gallons_per_capita), c(2.72, 2.76, 2.75,
                                                                   2.70, 2.66, 2.60))

  expect_equal(head(cali$ethanol_all_drinks_gallons_per_capita), c(2.33, 2.28, 2.32,
                                                                   2.33, 2.35, 2.25))
  expect_equal(tail(cali$ethanol_all_drinks_gallons_per_capita), c(3.22, 3.35, 3.36,
                                                                   3.32, 3.28, 3.19))

  expect_equal(head(delaware$ethanol_all_drinks_gallons_per_capita), c(3.72, 3.65, 3.59,
                                                                       3.59, 3.59, 3.55))
  expect_equal(tail(delaware$ethanol_all_drinks_gallons_per_capita), c(3.26, 3.24, 3.11,
                                                                       2.92, 2.85, 2.86))

  expect_equal(head(hawaii$ethanol_all_drinks_gallons_per_capita), c(2.63, 2.6, 2.59,
                                                                     2.56, 2.54, 2.52))
  expect_equal(tail(hawaii$ethanol_all_drinks_gallons_per_capita), c(3.41, 3.34, 3.3,
                                                                     3.14, 3.02, 3.11))

  expect_equal(head(penn$ethanol_all_drinks_gallons_per_capita), c(2.36, 2.33, 2.34,
                                                                   2.33, 2.34, 2.33))
  expect_equal(tail(penn$ethanol_all_drinks_gallons_per_capita), c(2.37, 2.38, 2.39,
                                                                   2.34, 2.3, 2.27))

  expect_equal(head(texas$ethanol_all_drinks_gallons_per_capita), c(2.34, 2.29, 2.24,
                                                                    2.25, 2.28, 2.27))
  expect_equal(tail(texas$ethanol_all_drinks_gallons_per_capita), c(2.84, 2.94, 2.77,
                                                                    2.68, 2.6, 2.51))

  expect_equal(head(utah$ethanol_all_drinks_gallons_per_capita), c(1.34, 1.39, 1.37,
                                                                   1.36, 1.37, 1.36))
  expect_equal(tail(utah$ethanol_all_drinks_gallons_per_capita), c(1.7, 1.75, 1.7,
                                                                   1.68, 1.66, 1.64))

  expect_equal(head(usa$ethanol_all_drinks_gallons_per_capita), c(2.35, 2.33, 2.32,
                                                                  2.33, 2.34, 2.29))
  expect_equal(tail(usa$ethanol_all_drinks_gallons_per_capita), c(2.72, 2.76, 2.75,
                                                                  2.7, 2.66, 2.6))

})
