with_test_pdf_device <- function(code) {
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit(
    {
      grDevices::dev.off()
      unlink(path)
    },
    add = TRUE
  )

  force(code)
}
