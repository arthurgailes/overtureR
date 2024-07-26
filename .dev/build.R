library(devtools)

# Document the package
document()

# Install the package
install(upgrade = FALSE)

# Run tests
test()

# Check the package
check()

# Build the package
build(manual = TRUE)
