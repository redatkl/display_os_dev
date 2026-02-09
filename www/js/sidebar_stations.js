$(document).ready(function() {
  // Click icon to highlight section
  $('.stations-sidebar .sidebar-icon').on('click', function() {
    const section = $(this).data('section');
    
    // Remove active from all icons and sections
    $('.stations-sidebar .sidebar-icon').removeClass('active');
    $('.stations-sidebar .panel-section').removeClass('active');
    
    // Add active to clicked icon and corresponding section
    $(this).addClass('active');
    $('.panel-section[data-section="' + section + '"]').addClass('active');
    
    // Scroll to section
    $('.panel-section[data-section="' + section + '"]')[0].scrollIntoView({
      behavior: 'smooth',
      block: 'start'
    });
  });
});