library(devtools)

# Document the package
document()

# Install the package
install(upgrade = FALSE)

# Run tests
test()

# Check the package
check(remote = TRUE, manual = FALSE)
check_rhub()

# cran helpers
devtools::check_win_devel()
revdepcheck::revdep_check(num_workers = 2)

# Build the package
build(manual = TRUE)

# submit to cran
release()
