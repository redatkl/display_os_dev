// sidebar_reporting.js
// Only handles the active icon highlight.
// The Shiny input is set by the inline script in reporting_module.R
// using the correct namespaced input ID.

$(document).ready(function () {

  // Set classification icon active on load
  $(document).on('shiny:connected', function () {
    $('.reporting-sidebar .sidebar-reporting-icon[data-module="classification"]').addClass('active');
  });

  // Update active icon on click
  $(document).on('click', '.reporting-sidebar .sidebar-reporting-icon', function () {
    $('.reporting-sidebar .sidebar-reporting-icon').removeClass('active');
    $(this).addClass('active');
  });

});