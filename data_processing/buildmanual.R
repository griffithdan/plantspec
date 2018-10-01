build_manual <- function(pkg = ".", path = NULL) {
  pkg <- devtools::as.package(pkg)
  #path <- path %||% dirname(pkg$path)
  name <- paste0(pkg$package, "_", pkg$version, ".pdf", collapse = " ")
  msg <- callr::rcmd("Rd2pdf", cmdargs = c(
    "--force",
    paste0("--output=", path, "/", name),
    pkg$path
  ))
  cat(msg$stdout)
  invisible(msg)
}

getwd()


#build_manual(pkg = "../plantspec", path = "../")

devtools::check(pkg = "../../plantspec", manual = TRUE, vignette = FALSE, cleanup = FALSE)
devtools::build(pkg = "../../plantspec", manual = TRUE, vignette = FALSE)
