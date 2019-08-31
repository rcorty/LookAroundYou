# Generate `n` consecutive line breaks in the UI
brs <- function(n = 1) {
    lapply(seq(n), function(i) shiny::tags$br())
}
