$(document).ready(function() {
  // Handle icon clicks
  $('.forecast-sidebar .sidebar-icon').on('click', function() {
    const blockName = $(this).data('block');
    
    $('.forecast-sidebar .sidebar-icon').removeClass('active');
    $('.data-block').removeClass('active');
    
    $(this).addClass('active');
    $('.data-block[data-block="' + blockName + '"]').addClass('active');
    
    console.log('Active block:', blockName);
  });
  
  // Optional: Click title to toggle
  $('.block-title').on('click', function() {
    const blockName = $(this).parent().data('block');
    
    $('.forecast-sidebar .sidebar-icon').removeClass('active');
    $('.data-block').removeClass('active');
    
    $('.forecast-sidebar .sidebar-icon[data-block="' + blockName + '"]').addClass('active');
    $(this).parent().addClass('active');
  });
});