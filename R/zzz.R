# specify the progress bar options
.onAttach <- function(libname, pkgname){
  options("pboptions" = list(
    type = if (interactive()) "timer" else "none",
    char = "=",
    txt.width = 50,
    gui.width = 300,
    style = 6,
    initial = 0,
    title = "R progress bar",
    label = "",
    nout = 100L,
    min_time = 1e-3))
  invisible(NULL)
}