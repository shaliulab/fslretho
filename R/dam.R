loadDamServer <- function(id, las_monitor, dataset_name){

  moduleServer(
    id,
    function(input, output, session) {

      metadata <- reactive({
        load_metadata(input$metadata$datapath, "dam")
      })

      metadata_linked <- reactive({
        fsldamr::link_dam_metadata(metadata(), input$result_dir)
      })

      dt_raw <- reactive({
        fsldamr::load_dam(metadata_linked())
      })

      # make it eager
      observeEvent(input$submit, {
        print(dt_raw())
        last_monitor("dam")
        dataset_name(input$metadata$name)
      })


      return(dt_raw)
    }
  )
}