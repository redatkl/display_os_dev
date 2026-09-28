$(document).ready(function() {
  // Handle icon clicks
  $('.forecast-sidebar .sidebar-icon').on('click', function() {
    $('.forecast-sidebar .sidebar-icon').removeClass('active');
    $(this).addClass('active');
  });

  // Tooltip delay, unchanged
  $('.forecast-sidebar .sidebar-icon').on('mouseenter', function() {
    const tooltip = $(this).find('.icon-tooltip');
    setTimeout(function() { tooltip.addClass('show'); }, 500);
  }).on('mouseleave', function() {
    $(this).find('.icon-tooltip').removeClass('show');
  });
});
