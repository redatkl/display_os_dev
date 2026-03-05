// sidebar_reporting.js
// Clicking an icon fires a Shiny input that swaps the main content area.
// No slide-out panel involved.

$(document).ready(function () {

  // ── Set classification as default on load ──────────────────────────────────
  $(document).on('shiny:connected', function () {
    $('.reporting-sidebar .sidebar-reporting-icon[data-module="classification"]').addClass('active');
    if (window.Shiny) {
      Shiny.setInputValue('reporting_active_module', 'classification');
    }
  });

  // ── Icon click ─────────────────────────────────────────────────────────────
  $(document).on('click', '.reporting-sidebar .sidebar-reporting-icon', function () {
    var module = $(this).data('module');
    if (!module) return;

    // Update active state
    $('.reporting-sidebar .sidebar-reporting-icon').removeClass('active');
    $(this).addClass('active');

    // Tell Shiny which module to display
    if (window.Shiny) {
      Shiny.setInputValue('reporting_active_module', module, { priority: 'event' });
      console.log('[Reporting] active module →', module);
    }
  });

});