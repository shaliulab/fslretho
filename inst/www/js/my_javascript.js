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

// Load tab: the flies to load (see flies_table and loadDataServer).
// The selection is sent as {token, ids}: the token tells the server which table it comes from.
(function () {
  function send(table) {
    var ids = $(table).find('input.fsl-fly-select:checked').map(function () { return this.value; }).get();
    Shiny.setInputValue(table.dataset.input, { token: table.dataset.token, ids: ids }, { priority: 'event' });
    var boxes = $(table).find('input.fsl-fly-select');
    var all = boxes.length > 0 && boxes.filter(':checked').length === boxes.length;
    $('button.fsl-flies-toggle[data-table="' + table.id + '"]').text(all ? 'Deselect all' : 'Select all');
  }
  $(document).on('change', 'table.fsl-flies input.fsl-fly-select', function () {
    send($(this).closest('table')[0]);
  });
  // (de)select all: deselect if every fly is selected, select all otherwise
  $(document).on('click', 'button.fsl-flies-toggle', function () {
    var table = document.getElementById(this.dataset.table);
    if (!table) return;
    var boxes = $(table).find('input.fsl-fly-select');
    boxes.prop('checked', boxes.filter(':checked').length !== boxes.length);
    send(table);
  });
})();

// Raw tab: the snapshot closest to the mouse is shown next to it while hovering the plots
// (the server fills .fsl-snapshot-tip when the option is ticked and leaves it empty otherwise)
$(document).on('mousemove', '.fsl-snapshot-hover', function (e) {
  var tip = document.querySelector('.fsl-snapshot-tip');
  if (!tip) return;
  if (!tip.innerHTML.trim()) { tip.style.display = 'none'; return; }
  tip.style.display = 'block';
  var gap = 16;
  var left = e.clientX + gap;
  var top = e.clientY + gap;
  if (left + tip.offsetWidth > window.innerWidth - 10) left = e.clientX - gap - tip.offsetWidth;
  if (top + tip.offsetHeight > window.innerHeight - 10) top = Math.max(10, window.innerHeight - 10 - tip.offsetHeight);
  tip.style.left = Math.max(10, left) + 'px';
  tip.style.top = top + 'px';
});
$(document).on('mouseleave', '.fsl-snapshot-hover', function () {
  var tip = document.querySelector('.fsl-snapshot-tip');
  if (tip) tip.style.display = 'none';
});
