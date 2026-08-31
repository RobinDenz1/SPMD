
test_that("general test cases, 2 datasets", {

  d1 <- data.table(ID=c(1, 1, 1, 2, 2, 3, 5),
                   start=c(20, 210, 370, 55, 98, 1, 9),
                   stop=c(189, 301, 375, 90, 190, 900, 10),
                   d1=c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE))

  d2 <- data.table(ID=c(1, 1, 1, 2, 2, 3, 5),
                   start=c(17, 211, 370, 58, 98, 1, 9),
                   stop=c(189, 321, 375, 90, 191, 94, 11),
                   d2=c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE))
  dlist <- list(d1, d2)

  expected <- data.table(ID=c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3,
                               5, 5),
                         start=c(17, 20, 189, 210, 211, 301, 321, 370, 55, 58,
                                 90, 98, 190, 1, 94, 9, 10),
                         stop=c(20, 189, 210, 211, 301, 321, 370, 375, 58, 90,
                                98, 190, 191, 94, 900, 10, 11),
                         d1=c(NA, TRUE, NA, TRUE, TRUE, NA, NA, FALSE, TRUE,
                              TRUE, NA, FALSE, NA, FALSE, FALSE, TRUE, NA),
                         d2=c(TRUE, TRUE, NA, NA, TRUE, TRUE, NA, FALSE, NA,
                              TRUE, NA, FALSE, FALSE, FALSE, NA, TRUE, TRUE))
  setkey(expected, ID, start)

  output <- merge_start_stop(d1, d2, by="ID")
  expect_equal(output, expected)

  # with center_on_first=TRUE
  output <- merge_start_stop(dlist=dlist, by="ID", center_on_first=TRUE)
  expect_true(all(output[, .(start = min(start)), by=ID]$start==0))
})

test_that("interval starts on the same day", {

  d1 <- data.table(id=c(1, 1),
                   start=c(0, 10),
                   stop=c(10, 22),
                   d1=c(32.1, 35))

  d2 <- data.frame(id=c(1, 1),
                   start=c(3, 10),
                   stop=c(4, 13),
                   B=c(TRUE, TRUE))
  dlist <- list(d1, d2)

  expected <- data.table(id=rep(1, 5),
                         start=c(0, 3, 4, 10, 13),
                         stop=c(3, 4, 10, 13, 22),
                         B=c(NA, TRUE, NA, TRUE, NA),
                         d1=c(32.1, 32.1, 32.1, 35.0, 35.0))
  setkey(expected, id, start)
  output <- merge_start_stop(dlist=dlist, by="id")

  expect_equal(output, expected)
})

test_that("interval ends on the same day", {

  d1 <- data.table(id=c(1, 1),
                   start=c(0, 4),
                   stop=c(4, 22),
                   value1=c(32.1, 35))

  d2 <- data.table(id=c(1, 1),
                   start=c(3, 10),
                   stop=c(4, 13),
                   value2=c(TRUE, TRUE))
  dlist <- list("d1"=d1, "d2"=d2)

  expected <- data.table(id=rep(1, 5),
                         start=c(0, 3, 4, 10, 13),
                         stop=c(3, 4, 10, 13, 22),
                         value1=c(32.1, 32.1, 35.0, 35.0, 35.0),
                         value2=c(NA, TRUE, NA, TRUE, NA))
  setkey(expected, id, start)
  output <- merge_start_stop(dlist=dlist, by="id")

  expect_equal(output, expected)
})

test_that("id not in every data.table (all.x, all.y)", {

  d1 <- data.table(id=c(1, 1, 2),
                   start=c(0, 4, 15),
                   stop=c(4, 22, 110),
                   value3=c(32.1, 35, 28))

  d2 <- data.table(id=c(1, 1, 3),
                   start=c(3, 10, 7),
                   stop=c(4, 13, 15),
                   value4=c(TRUE, TRUE, TRUE))
  dlist <- list("d1"=d1, "d2"=d2)

  expected_all <- data.table(id=c(rep(1, 5), 2, 3),
                         start=c(0, 3, 4, 10, 13, 15, 7),
                         stop=c(3, 4, 10, 13, 22, 110, 15),
                         value3=c(32.1, 32.1, 35.0, 35.0, 35.0, 28, NA),
                         value4=c(NA, TRUE, NA, TRUE, NA, NA, TRUE))
  setkey(expected_all, id, start)

  expected_all_x <- expected_all[id!=3]
  expected_all_y <- expected_all[id!=2]
  expected_neither <- expected_all[!id %in% c(2, 3)]

  # call merge_start_stop() with different all.x, all.y
  output_all <- merge_start_stop(dlist=dlist, by="id", all=TRUE)
  output_all_x <- merge_start_stop(dlist=dlist, by="id", all.x=TRUE,
                                   all.y=FALSE)
  output_all_y <- merge_start_stop(dlist=dlist, by="id", all.x=FALSE,
                                   all.y=TRUE)
  output_neither <- merge_start_stop(dlist=dlist, by="id")

  # test equality
  expect_equal(output_all, expected_all)
  expect_equal(output_all_x, expected_all_x)
  expect_equal(output_all_y, expected_all_y)
  expect_equal(output_neither, expected_neither)
})

test_that("using first_time", {

  d1 <- data.table(id=c(1, 1, 2),
                   start=c(0, 4, 15),
                   stop=c(4, 22, 110),
                   A=c(32.1, 35, 28))

  d2 <- data.table(id=c(1, 1),
                   start=c(3, 10),
                   stop=c(4, 13),
                   B=c(TRUE, TRUE))
  dlist <- list("d1"=d1, "d2"=d2)

  ## with first_time < actual entry
  expected <- data.table(id=c(rep(1, 6), 2, 2),
                         start=c(-10, 0, 3, 4, 10, 13, -10, 15),
                         stop=c(0, 3, 4, 10, 13, 22, 15, 110),
                         A=c(NA, 32.1, 32.1, 35.0, 35.0, 35.0, NA, 28),
                         B=c(NA, NA, TRUE, NA, TRUE, NA, NA, NA))
  setkey(expected, id, start)

  output <- merge_start_stop(dlist=dlist, by="id", first_time=-10, all=TRUE)
  expect_equal(output, expected)

  # with first_time sometimes > actual entry & remove_before_first=TRUE
  expected <- data.table(id=c(1, 1, 1, 2, 2),
                         start=c(9, 10, 13, 9, 15),
                         stop=c(10, 13, 22, 15, 110),
                         A=c(35, 35, 35, NA, 28),
                         B=c(NA, TRUE, NA, NA, NA))
  setkey(expected, id, start)

  output <- merge_start_stop(dlist=dlist, first_time=9,
                             remove_before_first=TRUE,
                             all=TRUE, by="id")
  expect_equal(output, expected)

  # with first_time sometimes > actual entry & remove_before_first=FALSE
  expected <- data.table(id=c(1, 1, 1, 1, 1, 1, 2, 2),
                         start=c(0, 3, 4, 9, 10, 13, 9, 15),
                         stop=c(3, 4, 9, 10, 13, 22, 15, 110),
                         A=c(32.1, 32.1, 35.0, 35.0, 35.0, 35.0, NA, 28.0),
                         B=c(NA, TRUE, NA, NA, TRUE, NA, NA, NA))
  setkey(expected, id, start)

  output <- merge_start_stop(dlist=dlist, first_time=9,
                             remove_before_first=FALSE,
                             all=TRUE, by="id")
  expect_equal(output, expected)
})

test_that("using last_time", {

  d1 <- data.table(id=c(1, 1, 2),
                   start=c(0, 4, 15),
                   stop=c(4, 22, 110),
                   value2=c(32.1, 35, 28))

  d2 <- data.table(id=c(1, 1),
                   start=c(3, 10),
                   stop=c(4, 13),
                   value3=c(TRUE, TRUE))
  dlist <- list("d1"=d1, "d2"=d2)

  ## with last_time > actual entry
  expected <- data.table(id=c(1, 1, 1, 1, 1, 1, 2, 2),
                         start=c(0, 3, 4, 10, 13, 22, 15, 110),
                         stop=c(3, 4, 10, 13, 22, 900, 110, 900),
                         value2=c(32.1, 32.1, 35.0, 35.0, 35.0, NA, 28.0, NA),
                         value3=c(NA, TRUE, NA, TRUE, NA, NA, NA, NA))
  setkey(expected, id, start)

  output <- merge_start_stop(dlist=dlist, last_time=900, all=TRUE, by="id")
  expect_equal(output, expected)

  # with last_time sometimes < actual entry & remove_after_last=TRUE
  expected <- data.table(id=c(1, 1, 1, 1, 1, 1, 2),
                         start=c(0, 3, 4, 10, 13, 22, 15),
                         stop=c(3, 4, 10, 13, 22, 101, 101),
                         value2=c(32.1, 32.1, 35.0, 35.0, 35.0, NA, 28.0),
                         value3=c(NA, TRUE, NA, TRUE, NA, NA, NA))
  setkey(expected, id, start)

  output <- merge_start_stop(dlist=dlist, last_time=101, remove_after_last=TRUE,
                     all=TRUE, by="id")
  expect_equal(output, expected)

  # with last_time sometimes < actual entry & remove_after_last=FALSE
  expected <- data.table(id=c(1, 1, 1, 1, 1, 1, 2, 2),
                         start=c(0, 3, 4, 10, 13, 22, 15, 101),
                         stop=c(3, 4, 10, 13, 22, 101, 101, 110),
                         value2=c(32.1, 32.1, 35.0, 35.0, 35.0, NA, 28.0, 28.0),
                         value3=c(NA, TRUE, NA, TRUE, NA, NA, NA, NA))
  setkey(expected, id, start)

  output <- merge_start_stop(dlist=dlist, last_time=101,
                             remove_after_last=FALSE,
                             all=TRUE, by="id")
  expect_equal(output, expected)
})

test_that("using different names in id, start, stop", {

  d1 <- data.table(by=c(1, 1),
                   anfang=c(0, 10),
                   ende=c(10, 22),
                   Wert1=c(32.1, 35))

  d2 <- data.table(by=c(1, 1),
                   anfang=c(3, 10),
                   ende=c(4, 13),
                   Wert2=c(TRUE, TRUE))
  dlist <- list("d1"=d1, "d2"=d2)

  expected <- data.table(by=rep(1, 5),
                         anfang=c(0, 3, 4, 10, 13),
                         ende=c(3, 4, 10, 13, 22),
                         Wert1=c(32.1, 32.1, 32.1, 35.0, 35.0),
                         Wert2=c(NA, TRUE, NA, TRUE, NA))
  setkey(expected, by, anfang)
  output <- merge_start_stop(dlist=dlist, by="by", start="anfang", stop="ende")

  expect_equal(output, expected)
})

test_that("using defaults", {

  d1 <- data.table(id=c(1, 1),
                   start=c(0, 10),
                   stop=c(10, 22),
                   value2=c(32.1, 35))

  d2 <- data.table(id=c(1, 1),
                   start=c(3, 10),
                   stop=c(4, 13),
                   value=c(TRUE, TRUE))
  dlist <- list("d1"=d1, "d2"=d2)

  expected <- data.table(id=c(1, 1, 1, 1, 1, 1, 1),
                         start=c(-2, 0, 3, 4, 10, 13, 22),
                         stop=c(0, 3, 4, 10, 13, 22, 110),
                         value=c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE),
                         value2=c(30.0, 32.1, 32.1, 32.1, 35.0, 35.0, 30.0))
  setkey(expected, id, start)

  # defaults for all
  output <- merge_start_stop(d1, d2, first_time=-2, last_time=110,
                     defaults=list("value2"=30, "value"=FALSE), by="id")
  expect_equal(output, expected)

  # defaults for one variable only
  output <- merge_start_stop(dlist=dlist, first_time=-2, last_time=110,
                     defaults=list("value"=FALSE), by="id")
  expected[, value2 := c(NA, 32.1, 32.1, 32.1, 35.0, 35.0, NA)]
  expect_equal(output, expected)
})

# TODO: not sure how events before first / after last time should be handled
# also check if this works / how this works with defined first_time / last_time
test_that("adding event status", {

  d1 <- data.table(id=c(1, 1, 2),
                   start=c(0, 10, 18),
                   stop=c(10, 22, 634),
                   value1=c(32.1, 35, 23))

  d2 <- data.table(id=c(1, 1),
                   start=c(3, 10),
                   stop=c(4, 13),
                   value2=c(TRUE, TRUE))
  dlist <- list("d1"=d1, "d2"=d2)

  d3 <- data.table(id=c(1, 1, 1, 2, 3),
                   time=c(2, 14, 21, 235, 99))

  expected <- data.table(id=c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2),
                         start=c(0, 2, 3, 4, 10, 13, 14, 21, 18, 235),
                         stop=c(2, 3, 4, 10, 13, 14, 21, 22, 235, 634),
                         value1=c(32.1, 32.1, 32.1, 32.1, 35.0, 35.0, 35.0,
                              35.0, 23.0, 23.0),
                         value2=c(NA, NA, TRUE, NA, TRUE, NA, NA, NA, NA, NA),
                         status=c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE,
                                  TRUE, FALSE, TRUE, FALSE))
  setkey(expected, id)

  output <- merge_start_stop(dlist=dlist, event_times=d3, all=TRUE, by="id")
  expect_equal(output, expected)
})

test_that("adding event status with just one dataset", {

  d1 <- data.table(id=c(1, 1, 2),
                   start=c(0, 10, 18),
                   stop=c(10, 22, 634),
                   value1=c(32.1, 35, 23))

  d3 <- data.table(id=c(1, 1, 1, 2, 3),
                   time=c(2, 14, 21, 235, 99))

  expected <- data.table(id=c(1, 1, 1, 1, 1, 2, 2),
                         start=c(0, 2, 10, 14, 21, 18, 235),
                         stop=c(2, 10, 14, 21, 22, 235, 634),
                         value1=c(32.1, 32.1, 35.0, 35.0, 35.0, 23.0, 23.0),
                         status=c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE))
  setkey(expected, id)

  output <- merge_start_stop(d1, event_times=d3, all=TRUE, by="id")
  expect_equal(output, expected)
})

test_that("continuous start / stop", {

  d1 <- data.table(id=c(1, 1, 2),
                   start=c(0, 10.124, 18.12452453523452345),
                   stop=c(10.124, 22.455, 634.454512),
                   value2=c(32.1, 35, 23))

  d2 <- data.table(id=c(1, 1, 2, 3),
                   start=c(3.2435, 10.657467, 18.12452453523452345, 425.234662),
                   stop=c(4.67467, 13.24323456, 56.5265, 5635.65346),
                   value4=c(TRUE, TRUE, TRUE, TRUE))
  dlist <- list("d1"=d1, "d2"=d2)

  output <- merge_start_stop(dlist=dlist, all=TRUE, by="id")
  expect_true(nrow(output)==9)
  expect_true(ncol(output)==5)
  expect_equal(max(output$stop), 5635.65346)
  expect_equal(max(output$start), 425.234662)
})

test_that("Date start / stop", {

  d1 <- data.table(id=c(1, 1, 2),
                   start=as.Date(c("01.01.2015", "05.04.2021", "01.08.1997"),
                                 format="%d.%m.%Y"),
                   stop=as.Date(c("04.04.2015", "21.11.2023", "07.08.1997"),
                                format="%d.%m.%Y"),
                   value3=c(32.1, 35, 23))

  d2 <- data.table(id=c(1, 1, 2, 3),
                   start=as.Date(c("21.11.2015", "25.05.2022", "11.03.1999",
                                   "01.12.2023"), format="%d.%m.%Y"),
                   stop=as.Date(c("01.01.2018", "05.04.2024", "01.08.2023",
                                  "04.12.2023"), format="%d.%m.%Y"),
                   value4=c(TRUE, TRUE, TRUE, TRUE))
  dlist <- list("d1"=d1, "d2"=d2)

  expected <- data.table(id=c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 3),
                         start=as.Date(c("2015-01-01", "2015-04-04",
                                         "2015-11-21", "2018-01-01",
                                         "2021-04-05", "2022-05-25",
                                         "2023-11-21", "1997-08-01",
                                         "1997-08-07", "1999-03-11",
                                         "2023-12-01")),
                         stop=as.Date(c("2015-04-04", "2015-11-21",
                                        "2018-01-01", "2021-04-05",
                                        "2022-05-25", "2023-11-21",
                                        "2024-04-05", "1997-08-07",
                                        "1999-03-11", "2023-08-01",
                                        "2023-12-04")),
                         value3=c(32.1, NA, NA, NA, 35.0, 35.0, NA, 23.0,
                              NA, NA, NA),
                         value4=c(NA, NA, TRUE, NA, NA, TRUE, TRUE, NA, NA,
                              TRUE, TRUE))
  setkey(expected, id, start)

  output <- merge_start_stop(dlist=dlist, all=TRUE, by="id")
  expect_equal(output, expected)

  # with center_on_first=TRUE
  output <- merge_start_stop(dlist=dlist, center_on_first=TRUE, by="id")
  expect_true(all(output[, .(start = min(start)), by=id]$start==0))
})

test_that("POSIXct start / stop", {

  d1 <- data.table(id=c(1, 1, 2),
                   start=as.POSIXct(c("01.01.2015", "05.04.2021", "01.08.1997"),
                                 format="%d.%m.%Y"),
                   stop=as.POSIXct(c("04.04.2015", "21.11.2023", "07.08.1997"),
                                format="%d.%m.%Y"),
                   value2=c(32.1, 35, 23))

  d2 <- data.table(id=c(1, 1, 2, 3),
                   start=as.POSIXct(c("21.11.2015", "25.05.2022", "11.03.1999",
                                   "01.12.2023"), format="%d.%m.%Y"),
                   stop=as.POSIXct(c("01.01.2018", "05.04.2024", "01.08.2023",
                                  "04.12.2023"), format="%d.%m.%Y"),
                   value3=c(TRUE, TRUE, TRUE, TRUE))
  dlist <- list("d1"=d1, "d2"=d2)

  expected <- data.table(id=c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 3),
                         start=as.POSIXct(c("2015-01-01", "2015-04-04",
                                         "2015-11-21", "2018-01-01",
                                         "2021-04-05", "2022-05-25",
                                         "2023-11-21", "1997-08-01",
                                         "1997-08-07", "1999-03-11",
                                         "2023-12-01")),
                         stop=as.POSIXct(c("2015-04-04", "2015-11-21",
                                        "2018-01-01", "2021-04-05",
                                        "2022-05-25", "2023-11-21",
                                        "2024-04-05", "1997-08-07",
                                        "1999-03-11", "2023-08-01",
                                        "2023-12-04")),
                         value2=c(32.1, NA, NA, NA, 35.0, 35.0, NA, 23.0,
                              NA, NA, NA),
                         value3=c(NA, NA, TRUE, NA, NA, TRUE, TRUE, NA, NA,
                              TRUE, TRUE))
  setkey(expected, id, start)

  output <- merge_start_stop(dlist=dlist, all=TRUE, by="id")
  expect_equal(output, expected)

  # with center_on_first=TRUE
  output <- merge_start_stop(dlist=dlist, center_on_first=TRUE, all=TRUE,
                             by="id")
  expect_true(all(output[, .(start = min(start)), by=id]$start==0))

  # with center_on_first=TRUE and event_times specified
  event_times <- data.table(id=3, time=as.POSIXct("2023-12-03"))
  output <- merge_start_stop(dlist=dlist, center_on_first=TRUE, all=TRUE,
                             by="id", event_times=event_times)
  expect_true(all(output[, .(start = min(start)), by=id]$start==0))
  expect_true(sum(output$status)==1)
})

test_that("include time-invariant variables", {

  d1 <- data.table(id=c(1, 1, 2),
                   start=c(0, 4, 15),
                   stop=c(4, 22, 110),
                   value1=c(32.1, 35, 28))

  d2 <- data.table(id=c(1, 1),
                   start=c(3, 10),
                   stop=c(4, 13),
                   value2=c(TRUE, TRUE))
  dlist <- list("d1"=d1, "d2"=d2)

  dfixed <- data.table(id=c(1, 2, 3, 4),
                       sex=c("m", "f", "m", "m"),
                       age=c(10, 23, 93, 12))

  output1 <- merge_start_stop(dlist=dlist, all=TRUE, by="id")
  output2 <- merge_start_stop(dlist=dlist, constant_vars=dfixed, all=TRUE,
                              by="id")

  expect_true(unique(output2[id==1]$sex)=="m")
  expect_true(unique(output2[id==2]$sex)=="f")

  expect_true(unique(output2[id==1]$age)==10)
  expect_true(unique(output2[id==2]$age)==23)

  output2[, sex := NULL]
  output2[, age := NULL]
  setkey(output2, id, start)
  expect_equal(output1, output2)
})

test_that("3 variables", {

  d1 <- data.table(id=c(1, 1, 2),
                   start=c(0, 4, 15),
                   stop=c(4, 22, 110),
                   A=c(32.1, 35, 28))

  d2 <- data.table(id=c(1, 1),
                   start=c(3, 10),
                   stop=c(4, 13),
                   BB=c(TRUE, TRUE))

  d3 <- data.table(id=c(1, 2, 2, 3),
                   start=c(18, 7, 28, 10),
                   stop=c(120, 14, 94, 25),
                   CCC=c("A", "B", "C", "C"))

  dlist <- list(d1, d2, d3)

  output <- merge_start_stop(dlist=dlist, all=TRUE, by="id")
  output2 <- merge_start_stop(d1, d2, d3, all=TRUE, by="id")

  expect_equal(output, output2)

  expected <- data.table(id=c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3),
                         start=c(0, 3, 4, 10, 13, 18, 22, 7, 14, 15, 28, 94,10),
                         stop=c(3, 4, 10, 13, 18, 22, 120, 14, 15, 28, 94,
                                110, 25),
                         A=c(32.1, 32.1, 35.0, 35.0, 35.0, 35.0, NA, NA, NA,
                             28.0, 28.0, 28.0, NA),
                         BB=c(NA, TRUE, NA, TRUE, NA, NA, NA, NA, NA, NA, NA,
                              NA, NA),
                         CCC=c(NA, NA, NA, NA, NA, "A", "A", "B", NA, NA, "C",
                               NA, "C"))
  setkey(expected, id, start)

  expect_equal(output, expected)
})

test_that("multiple variables in the same start-stop table", {

  d1 <- data.table(id=c(1, 1, 2),
                   start=c(0, 4, 15),
                   stop=c(4, 22, 110),
                   A=c(32.1, 35, 28),
                   B=c(33L, 36L, 29L),
                   C=c(TRUE, FALSE, TRUE))

  d2 <- data.table(id=c(1, 1),
                   start=c(3, 10),
                   stop=c(4, 13),
                   BB=c(TRUE, TRUE))

  d3 <- data.table(id=c(1, 2, 2, 3),
                   start=c(18, 7, 28, 10),
                   stop=c(120, 14, 94, 25),
                   CCC=c("A", "B", "C", "C"))

  dlist <- list(d1, d2, d3)

  output <- merge_start_stop(dlist=dlist, all=TRUE, by="id")

  expected <- data.table(id=c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3),
                         start=c(0, 3, 4, 10, 13, 18, 22, 7, 14, 15, 28, 94,10),
                         stop=c(3, 4, 10, 13, 18, 22, 120, 14, 15, 28, 94,
                                110, 25),
                         A=c(32.1, 32.1, 35.0, 35.0, 35.0, 35.0, NA, NA, NA,
                             28.0, 28.0, 28.0, NA),
                         B=c(33L, 33L, 36L, 36L, 36L, 36L, NA_integer_,
                             NA_integer_, NA_integer_,
                             29L, 29L, 29L, NA_integer_),
                         BB=c(NA, TRUE, NA, TRUE, NA, NA, NA, NA, NA, NA, NA,
                              NA, NA),
                         C=c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, NA, NA,
                             NA, TRUE, TRUE, TRUE, NA),
                         CCC=c(NA, NA, NA, NA, NA, "A", "A", "B", NA, NA, "C",
                               NA, "C"))
  setkey(expected, id, start)

  expect_equal(output, expected)
})

test_that("general test cases, 2 datasets, integers", {

  d1 <- data.table(ID=c(1, 1, 1, 2, 2, 3, 5),
                   start=c(20, 210, 370, 55, 98, 1, 9),
                   stop=c(189, 301, 375, 90, 190, 900, 10),
                   d1=c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE))
  d1[, d1 := as.integer(d1)]

  d2 <- data.table(ID=c(1, 1, 1, 2, 2, 3, 5),
                   start=c(17, 211, 370, 58, 98, 1, 9),
                   stop=c(189, 321, 375, 90, 191, 94, 11),
                   d2=c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE))
  d2[, d2 := as.integer(d2)]
  dlist <- list(d1, d2)

  expected <- data.table(ID=c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3,
                              5, 5),
                         start=c(17, 20, 189, 210, 211, 301, 321, 370, 55, 58,
                                 90, 98, 190, 1, 94, 9, 10),
                         stop=c(20, 189, 210, 211, 301, 321, 370, 375, 58, 90,
                                98, 190, 191, 94, 900, 10, 11),
                         d1=c(NA, TRUE, NA, TRUE, TRUE, NA, NA, FALSE, TRUE,
                              TRUE, NA, FALSE, NA, FALSE, FALSE, TRUE, NA),
                         d2=c(TRUE, TRUE, NA, NA, TRUE, TRUE, NA, FALSE, NA,
                              TRUE, NA, FALSE, FALSE, FALSE, NA, TRUE, TRUE))
  setkey(expected, ID, start)

  expected[, d1 := as.integer(d1)]
  expected[, d2 := as.integer(d2)]

  output <- merge_start_stop(d1, d2, by="ID")
  expect_equal(output, expected)

  # with center_on_first=TRUE
  output <- merge_start_stop(dlist=dlist, by="ID", center_on_first=TRUE)
  expect_true(all(output[, .(start = min(start)), by=ID]$start==0))
})
