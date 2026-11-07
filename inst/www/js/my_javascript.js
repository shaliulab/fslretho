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

// "Close all" button above the notifications (warnings, errors, information)
// whenever more than one is shown
$(function () {
  var update = function () {
    var panel = document.getElementById('shiny-notification-panel');
    if (!panel) return;
    var n = panel.querySelectorAll('.shiny-notification').length;
    var button = document.getElementById('fsl-close-notifications');
    if (n > 1 && !button) {
      button = document.createElement('button');
      button.id = 'fsl-close-notifications';
      button.className = 'btn btn-default btn-sm';
      button.innerHTML = '&times; Close all';
      button.addEventListener('click', function () {
        $(panel).find('.shiny-notification').each(function () {
          var close = $(this).find('.shiny-notification-close');
          if (close.length) close.click(); else $(this).remove();
        });
      });
      panel.insertBefore(button, panel.firstChild);
    } else if (n <= 1 && button) {
      button.remove();
    }
  };
  new MutationObserver(update).observe(document.body, { childList: true, subtree: true });
});
