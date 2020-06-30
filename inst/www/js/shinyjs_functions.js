shinyjs.plotSize = function(params){
  size = params["params"];
  console.log(size);
  plot_id = params["plot_id"];
  console.log("css selector: .shiny-plot-output#" + plot_id);
  $('.shiny-plot-output#' + plot_id).css('height', size[0]);
  $('.shiny-plot-output#' + plot_id).css('width', size[1]);
  // console.log('Setting height to ' + size[0] + ' px');
  // console.log('Setting width to ' + size[1] + ' px');
};

//shinyjs.pageCol = function(params){
 // $('body').css('background', params);
//};