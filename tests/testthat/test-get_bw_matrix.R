test_that("bw matrix is correct", {
  # TODO: can we use rtracklayer test bigwigs??
  bw <- system.file("tests", "test.bw", package = "rtracklayer")
  bw <- rtracklayer::BigWigFile(bw)
  regions <- GRanges(c("chr2", "chr2"), IRanges::IRanges(c(1,101), c(100, 200)))

  # type errors correctly
  expect_error(get_bw_matrix(bw, regions, type = "mon"))
  #rtracklayer::import(bw, which = regions)

  # Ensure catching missing chromosomes
  bad_regions <- GRanges(c("chr3", "chr3"), IRanges::IRanges(c(1,101), c(100, 200)))
  expect_error(get_bw_matrix(bw, bad_regions), "do not match chromosomes")

  # Catch out of bounds
  bad_regions2 <- GRanges(c("chr2"), IRanges::IRanges(c(243199173), c(243199573)))
  suppressWarnings(expect_warning(get_bw_matrix(bw, bad_regions2), "out-of-bound"))
  suppressWarnings(expect_warning(get_bw_matrix(bw, bad_regions2), "Failed to summarize"))
})
