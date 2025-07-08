## Render the flexdashboard with full logging ---------------------------

# Open a write-text connection for the log file
# This will overwrite the previous log each time you run the script.
log_con <- file("render-log.txt", open = "wt")

# Divert normal console output and messages to the log connection
sink(log_con, type = "output")
sink(log_con, type = "message")

# -------- Run the render -----------------------------------------------------
render_success <- tryCatch(
    {
        rmarkdown::render(
            "./index.Rmd", # path to the dashboard
            clean  = FALSE,
            quiet  = FALSE # keep verbose output for the log file
        )
        TRUE
    },
    error = function(e) {
        cat("\n---- RENDER FAILED ----\n")
        message(e)
        FALSE
    }
)

# -------- Restore console output --------------------------------------------
# Turn the message sink off first, then the output sink
sink(type = "message")
sink(type = "output")
close(log_con)

if (render_success) {
    cat("\nRender completed successfully. See 'render-log.txt' for details.\n")
} else {
    cat("\nRender encountered errors. Check 'render-log.txt' for details.\n")
}
