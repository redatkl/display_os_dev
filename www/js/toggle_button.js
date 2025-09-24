$(document).ready(function() {
  // Handle clicks on all toggle containers
  $('.toggle-container').click(function() {
    let toggleId = $(this).data('toggle-id');
    let isActive = $(this).hasClass('active');
    
    // Toggle state
    isActive = !isActive;
    
    if (isActive) {
      $(this).addClass('active');
      $(this).find('.toggle-button').addClass('active');
    } else {
      $(this).removeClass('active');
      $(this).find('.toggle-button').removeClass('active');
    }
    
    // Send state to Shiny server
    Shiny.setInputValue(toggleId, isActive);
  });
});
