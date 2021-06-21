#' @importFrom RSQLite dbGetQuery SQLite SQLITE_RO dbConnect
#' @importFrom data.table as.data.table
list_ethoscopes <- function(FILE="/etc/ethoscope-node.db", sorted = TRUE) {

  if(!file.exists(FILE)) {stop(paste0(FILE, ", the ethoscope resource database does not exist"))}

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

  path <- get_writable_path(path)
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


generate_tBody_ui <- function(ethos) {

  if (length(ethos) != 0) {

    rows <- lapply(1:length(ethos), function(i) {
      etho <- ethos[i]
      switch_id <- paste0(etho, "_switch")
      button_id <- paste0(etho, "_button")
      label <- paste0("Backup ", etho)
      class <- ifelse(i %% 2 == 0, "even", "odd")

      shiny::tags$tr(
        shiny::tags$td(
          shiny::tags$p(etho)
        ),
        shiny::tags$td(
          shinyWidgets::materialSwitch(ns(switch_id), value = ! (etho %in% backup_off))
        ),
        shiny::tags$td(
          shinyWidgets::actionBttn(
            inputId = ns(button_id),
            label = label,
            color = "primary",
            style = "bordered"
          )
        ),
        class = class
      )
    })
  } else {
    rows <- list()
  }

  rows <- shiny::tagList(rows)
  return(rows)

}

#' @import shiny
#' @import shinyWidgets
#' @importFrom magrittr `%>%`
backupManagerUI <- function(id) {

  ns <- shiny::NS(id)

  ethos <- list_ethoscopes("/etc/ethoscope-node.db", sorted = TRUE)
  backup_off <- load_backupoff()

  table_body <- generate_tBody_ui(ethos)


  etho_table <- tags$table(
    tags$thead(
      tags$th("Ethoscope"),
      tags$th("Backup daemon"),
      tags$th("Manual backup")
    ),
    tags$tbody(
      table_body
    )
  )


  shiny::tagList(
    shiny::tags$p(
        "PLEASE NOTE: The state of some switches upon page load might not be accurate immediately.
        Please give a couple of seconds for FSLRetho to update the switches.
        This is required when loading the page for the first time on each session, i.e. you dont need to wait
        if you have seen this text before in your current session.
        You will have to wait again if you reload or close, but not if you just move between the different sections
        of the application.
        "
      ),
    shiny::tags$p(
      "When a switch is OFF, the corresponding ethoscope gets listed under /etc/backup_off.conf.
      Manually editing the file and writing there the name of the ethoscope whose automatic backup
      you want to disable has the same effect. Correspondingly, removing the ethoscope from the file
      has the same effect as turning its switch ON.
    "),
    etho_table
    )


}

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
      backup_off <- load_backupoff()

      # Respond to switch toggles
      # If the switch is ON, we want the backup daemon
      # to back the corresponding ethoscope
      # If the switch is OFF, then the backup daemon
      # will ignore this ethoscope even if it's running
      lapply(switches, function(sw) {

        etho <- gsub(pattern = "_switch", replacement = "", x = sw)
        if (etho %in% backup_off) {
            # print("Setting to FALSE")
            shinyWidgets::updateMaterialSwitch(session = session, inputId = sw, value = FALSE)
        } else {
            # print("Setting to TRUE")
            shinyWidgets::updateMaterialSwitch(session = session, inputId = sw, value = TRUE)
        }

        observeEvent(input[[sw]], {
          if(input[[sw]]) {
            backup_off <<- remove_backupoff(backup_off, etho)
            message(sprintf("Enabling backup daemon for %s", etho))
          } else {
            backup_off <<- add_backupoff(backup_off, etho)
            message(sprintf("Disabling backup daemon for %s", etho))
          }
          save_backupoff(backup_off)
        }, ignoreInit = TRUE)
      })


      # Respond to click on manual backup buttons
      lapply(buttons, function(bt) {

        observeEvent(input[[bt]], {
          etho <- gsub(pattern = "_button", replacement = "", x = bt)
          pattern <- gsub(pattern = "ETHOSCOPE_", replacement = "", x = etho)
          backup_cmd <- paste0(python_binary, " backup_tool.py --safe --debug --ethoscope ", pattern)
          message(sprintf("CMD: %s", backup_cmd))

          tryCatch({
            system(backup_cmd)
            print(backup_cmd)
            message(sprintf("Manual backup of %s completed!", etho))
          }, error = function(e) {
            warning(e)
          })
        }, ignoreInit = TRUE)
      })
    })
}
