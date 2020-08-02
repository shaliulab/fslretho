metadatas <- list("antortjim-GL552VW" = "../shinytests/data/ethoscope_metadata/laptop_test.csv")
metadata <- metadatas[[Sys.info()[["nodename"]]]]

get_selector_drag <- function(variable, module="analyseSleep_01") {
  sprintf("#%s-dragvars-target-label-%s", module, variable)
}
get_selector_drop <- function(aesthetic, module="analyseSleep_01") {
  sprintf("#%s-dragvars-target-%s", module, aesthetic)
}

interact_dragula <- function(app, x="t", y="asleep") {
  script <- sprintf('triggerDragAndDrop("%s", "%s");', get_selector_drag(x), get_selector_drop("xvar"))
  app$executeScript(script)
  script <- sprintf('triggerDragAndDrop("%s", "%s");', get_selector_drag(y), get_selector_drop("yvar"))
  app$executeScript(script)

  return(app)
}


app <- shinytest::ShinyDriver$new("../../")
app$snapshotInit("ethoscope")
tab_load <- app$findElement('a[href="#shiny-tab-load"]')
tab_sleep <- app$findElement('a[href="#shiny-tab-sleep"]')
tab_bout <- app$findElement('a[href="#shiny-tab-bout"]')
tab_load$click()

app$uploadFile(`loadData-ethoscope-metadata` = metadata) # <-- This should be the path to the file, relative to the app's tests/shinytests directory
app$setInputs(`loadData-ethoscope-submit` = "click", timeout_ = 30000)

tab_sleep$click()
app$setInputs(`analyseSleep_01-controls-filter-data-filter_id` = c("2020-03-23_19-21-49_019aad|11", "2020-03-23_19-21-49_019aad|12", "2020-03-23_19-21-49_019aad|13", "2020-03-23_19-21-49_019aad|14", "2020-03-23_19-21-49_019aad|15", "2020-03-23_19-21-49_019aad|16", "2020-03-23_19-21-49_019aad|17", "2020-03-23_19-21-49_019aad|18", "2020-03-23_19-21-49_019aad|19", "2020-03-23_19-21-49_019aad|20"))
app$setInputs(`analyseSleep_01-controls-filter-data-filter_genotype` = c("A", "B"))
app$setInputs(`analyseSleep_01-controls-filter-data-filter_sex` = c("F", "M"))
app$setInputs(`analyseSleep_01-controls-filter-data-filter_datetime` = c(1584991309, 1584991309))
app$setInputs(`analyseSleep_01-controls-filter-data-filter_region_id` = c(11, 20))
app$setInputs(`analyseSleep_01-controls-filter-data-filter_reference_hour` = c(1.15, 2.8))
app$setInputs(`analyseSleep_01-controls-filter-data-filter_fly_count` = c(1, 10))
app$setInputs(`analyseSleep_01-controls-filter-data-filter_t` = c(61200, 153000))
app$setInputs(`analyseSleep_01-controls-filter-data-filter_asleep` = c(0, 1))
app$setInputs(`analyseSleep_02-controls-filter-data-filter_id` = c("2020-03-23_19-21-49_019aad|11", "2020-03-23_19-21-49_019aad|12", "2020-03-23_19-21-49_019aad|13", "2020-03-23_19-21-49_019aad|14", "2020-03-23_19-21-49_019aad|15", "2020-03-23_19-21-49_019aad|16", "2020-03-23_19-21-49_019aad|17", "2020-03-23_19-21-49_019aad|18", "2020-03-23_19-21-49_019aad|19", "2020-03-23_19-21-49_019aad|20"))
app$setInputs(`analyseSleep_02-controls-filter-data-filter_genotype` = c("A", "B"))
app$setInputs(`analyseSleep_02-controls-filter-data-filter_sex` = c("F", "M"))
app$setInputs(`analyseSleep_02-controls-filter-data-filter_datetime` = c(1584991309, 1584991309))
app$setInputs(`analyseSleep_02-controls-filter-data-filter_region_id` = c(11, 20))
app$setInputs(`analyseSleep_02-controls-filter-data-filter_reference_hour` = c(1.15, 2.8))
app$setInputs(`analyseSleep_02-controls-filter-data-filter_fly_count` = c(1, 10))
app$setInputs(`analyseSleep_02-controls-filter-data-filter_t` = c(61200, 153000))
app$setInputs(`analyseSleep_02-controls-filter-data-filter_asleep` = c(0, 1))
app$setInputs(`analyseSleep_01-play_plot` = FALSE)
interact_dragula(app)
app$setInputs(`analyseSleep_01-play_plot` = TRUE)
app$snapshot()




