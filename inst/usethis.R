#' Script to automate package maintainance and development
#' Add here dependencies that should be Imports or Suggests in DESCRIPTION
library(usethis)

# Modify the description ----------------------------------------------
#> ✓ Setting License field in DESCRIPTION to 'MIT + file LICENSE'
#> ✓ Writing 'LICENSE.md'
#> ✓ Adding '^LICENSE\\.md$' to '.Rbuildignore'
#> ✓ Writing 'LICENSE'

# Set up other files -------------------------------------------------
use_readme_md()

use_description(fields = list(
  Title = "fslretho: a Shiny GUI to the Rethomics suite",
  Description = "fslretho provides a user-friendly GUI to analyse behavioral datasets and focus the attention of users in the biological questions at hand, freeing them from programming technicalities.",
  Version = numeric_version("2.0"),
  Language = "en"
  )
)
use_mit_license(name = "Liu Lab")

# Core
use_package("shiny", "Imports")
use_package("data.table", "Imports")
use_package("ggplot2", "Imports")
use_package("RSQLite", "Imports")
use_package("shaliulab/esquisse", "Remotes")

# Import Rethomics suite
use_package("shaliulab/behavr", "Remotes")
use_package("shaliulab/damr", "Remotes")
use_package("shaliulab/scopr", "Remotes")
use_package("shaliulab/sleepr", "Remotes")
use_package("shaliulab/ggetho", "Remotes")

# Import tidyverse dependencies
use_package("rlang", "Imports")


# Import GUI dependencies
use_package("shinydashboard", "Imports")
use_package("shinydashboardPlus", "Imports")
use_package("shinywidgets", "Imports")
use_package("shinybusy", "Imports")

use_package("stringr", "Imports")
use_package("lubridate", "Imports")
use_package("dplyr", "Imports")


# Import R6 and rjson to support FSLRethoConfiguration
use_package("R6", "Imports")
use_package("rjson", "Imports")


# Suggestions
use_package("knitr", "Suggests")
use_package("reactlog", "Suggests")
use_package("shinylogs", "Suggests")


use_news_md()
#> ✓ Writing 'NEWS.md'

# use_test("my-test")
#> ✓ Adding 'testthat' to Suggests field in DESCRIPTION
#> ✓ Creating 'tests/testthat/'
#> ✓ Writing 'tests/testthat.R'
#> ● Call `use_test()` to initialize a basic test file and open it for editing.
#> ✓ Writing 'tests/testthat/test-my-test.R'
#> ● Edit 'tests/testthat/test-my-test.R'

# x <- 1
# y <- 2
# use_data(x, y)
#> ✓ Adding 'R' to Depends field in DESCRIPTION
#> ✓ Creating 'data/'
#> ✓ Saving 'x', 'y' to 'data/x.rda', 'data/y.rda'
#> ● Document your data (see 'https://r-pkgs.org/data.html')

# Use git ------------------------------------------------------------
use_git()
#> ✓ Initialising Git repo
#> ✓ Adding '.Rhistory', '.RData', '.Rproj.user' to '.gitignore'