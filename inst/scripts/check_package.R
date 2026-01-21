# Check package
cat("Running devtools::check()...\n\n")
devtools::check(".", document = FALSE, args = "--no-manual")
