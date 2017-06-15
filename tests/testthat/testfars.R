test_that("make_filename generates the file name", {
        expect_equal(make_filename(2013), "accident_2013.csv.bz2")
})

# test fars_read_years
test_that("fars_read_years return NULL if the file is missing", {
        expect_equal(fars_read_years(2013), list(NULL))
})