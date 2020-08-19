#' @importFrom RSQLite dbGetQuery SQLite SQLITE_RO dbConnect
#' @importFrom data.table as.data.table
list_ethoscopes <- function(FILE="/etc/ethoscope-node.db", sorted = TRUE) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), FILE, flags = RSQLite::SQLITE_RO)
  ethos <- tryCatch({
    data.table::as.data.table(RSQLite::dbGetQuery(con, "SELECT * FROM ethoscopes"))$ethoscope_name
  },
  finally = {
    RSQLite::dbDisconnect(con)
  })

  if (sorted) {
    ethos <- sort(ethos)
  }

  return(ethos)
}

save_backupoff <- function(x, path="/etc/backup_off.conf") {
  x <- sort(x[x != ""])

  write.table(x = x, file = path, quote = F, row.names = F, col.names = F)
}

load_backupoff <- function(path="/etc/backup_off.conf") {
  x <- tryCatch({
    as.character(unname(unlist(read.table(file = path))))
  }, error = function(e) {
    ""
  })
  x <- sort(x[x != ""])
  x
}

remove_backupoff <- function(x, etho) {
  x[!(x == etho)]
}

add_backupoff <- function(x, etho) {
  c(x, etho)

}



#' @import shiny
#' @import shinyWidgets
#' @importFrom magrittr `%>%`
backupManagerUI <- function(id) {

  ns <- shiny::NS(id)

  ethos <- list_ethoscopes("/etc/ethoscope-node.db", sorted = TRUE)

  rows <- lapply(ethos, function(etho) {
    switch_id <- paste0(etho, "_switch")
    button_id <- paste0(etho, "_button")

    shiny::tags$tr(
      shiny::tags$td(
     # shiny::tags$p(etho, style = "display: inline-block;"),
        shiny::tags$p(etho)
      ),
      shiny::tags$td(
        shinyWidgets::materialSwitch(ns(switch_id), value = TRUE) #%>%
        # htmltools::tagAppendAttributes(., style = "display: inline-block;")
      ),
      shiny::tags$td(
        shinyWidgets::actionBttn(
          inputId = ns(button_id),
          label = "Backup!",
          color = "primary",
          style = "bordered"
        )
      )
    )
  })

  etho_table <- tags$table(
    tags$thead(
      tags$th("Ethoscope"),
      tags$th("Backup daemon"),
      tags$th("Manual backup")
    ),
    tags$tbody(
      shiny::tagList(rows)
    )
  )


  shiny::tagList(etho_table)


}

#' @importFrom glue glue
#' @import shiny
backupManagerServer <- function(id) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      switches <- grep(pattern = "switch", x = isolate(names(input)), value = T)
      buttons <- grep(pattern = "button", x = isolate(names(input)), value = T)

      python_binary <- FSLRethoConfiguration$new()$content[["python_binary"]]
      python_binary <- ifelse(is.null(python_binary), "python", python_binary)


      backup_tool.py <- "/opt/ethoscope-node/node_src/scripts/backup_tool.py"


      # Respond to switch toggles
      # If the switch is ON, we want the backup daemon
      # to back the corresponding ethoscope
      # If the switch is OFF, then the backup daemon
      # will ignore this ethoscope even if it's running

      for (sw in switches) {
        observeEvent(input[[sw]], {
          etho <- gsub(pattern = "_switch", replacement = "", x = sw)
          backup_off <- load_backupoff()
          if(input[[sw]]) {
            backup_off <- remove_backupoff(backup_off, etho)
          } else {
            backup_off <- add_backupoff(backup_off, etho)
          }

          save_backupoff(backup_off)
        }, ignoreInit = TRUE)
      }

      # Respond to click on manual backup buttons
      for (bt in buttons) {
        observeEvent(input[[bt]], {
          etho <- gsub(pattern = "_button", replacement = "", x = bt)
          pattern <- gsub(pattern = "ETHOSCOPE_", replacement = "", x = etho)
          backup_cmd <- glue::glue('{python_binary} {backup_tool.py} --safe --debug --ethoscope {pattern}')
          tryCatch({
            system(backup_cmd)
          }, error = function(e) {
            warning(e)
          })
        })
      }
    })
}
