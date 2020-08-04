var refresh_plot = function() {
  new_height = parseInt(document.querySelector("div#analyseSleep_00").style.height.replace("px",""));
  setTimeout(1000);
  document.querySelector("div#analyseSleep_00 > img").height = new_height;
  console.log(new_height);
  setTimeout(1000);
  Shiny.onInputChange("refresh_analyseSleep_00", new_height);

  setTimeout(5000);
  document.querySelector("div#analyseSleep_00 > img").height = new_height;

};

// Check every 100ms if R is busy or not
// and set the visibility of the div.busy accordingly
// this div displays a stop sign that should be visible only when shiny is busy
setInterval(function(){
  if ($('html').attr('class')=='shiny-busy') {
    $('div.busy').show();
  } else {
    $('div.busy').hide();
  }
},100);

