$(document).on('click', '.needed', function () {
                              Shiny.onInputChange('last_btn',this.id);
                             });


console.log("Hello, I am Javascript and I am running in this page");


var labels = document.getElementsByTagName('LABEL');
for (var i = 0; i < labels.length; i++) {
    if (labels[i].htmlFor != '') {
         var elem = document.getElementById(labels[i].htmlFor);
         if (elem)
            elem.label = labels[i];         
    }
};


// Check every 100ms if R is busy or not
// and set the visibility of the div.busy accordingly
// this div displays a stop sign that should be visible only when shiny is busy
setInterval(function(){
  if ($('html').attr('class')=='shiny-busy') {
    $('div.busy').show()
  } else {
    $('div.busy').hide()
  }
},100);