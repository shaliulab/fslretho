// Keep the dropdown menus of the top bar (scoring parameters, binning, downloads)
// inside the window: if a menu would overflow on the right, shift it to the left.
$(document).on('shown.bs.dropdown', '.navbar .dropdown', function () {
  var menu = $(this).children('.dropdown-menu');
  menu.css('left', '0px');
  var overflow = menu[0].getBoundingClientRect().right - (window.innerWidth - 10);
  if (overflow > 0) menu.css('left', (-overflow) + 'px');
});

// Clicking inside a dropdown block (sliders, buttons) must not close it
$(document).on('click', '.navbar .dropdown-menu .fsl-dropdown', function (e) {
  e.stopPropagation();
});
