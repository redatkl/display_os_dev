$(document).ready(function() {
  // Handle icon clicks
  $('.stations-sidebar .sidebar-icon').on('click', function() {
    const blockName = $(this).data('block');
    
    $('.stations-sidebar .sidebar-icon').removeClass('active');
    $('.data-block').removeClass('active');
    
    $(this).addClass('active');
    $('.data-block[data-block="' + blockName + '"]').addClass('active');
    
    console.log('Active block:', blockName);
  });
  
  // Optional: Click title to toggle
  $('.block-title').on('click', function() {
    const blockName = $(this).parent().data('block');
    
    $('.stations-sidebar .sidebar-icon').removeClass('active');
    $('.data-block').removeClass('active');
    
    $('.stations-sidebar .sidebar-icon[data-block="' + blockName + '"]').addClass('active');
    $(this).parent().addClass('active');
  });
});