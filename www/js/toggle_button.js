$(document).ready(function() {
  // Handle clicks on group toggle containers
  $('.toggle-container').click(function() {
    let toggleId = $(this).data('toggle-id');
    let groupId = $(this).data('group-id');
    let optionKey = $(this).data('option-key');
    
    // If this is part of a group, deactivate all other toggles in the group
    if (groupId) {
      // Deactivate all toggles in this group
      $('[data-group-id="' + groupId + '"]').removeClass('active');
      $('[data-group-id="' + groupId + '"] .toggle-button').removeClass('active');
      
      // Activate only the clicked toggle
      $(this).addClass('active');
      $(this).find('.toggle-button').addClass('active');
      
      // Send the selected option key to Shiny server
      Shiny.setInputValue(groupId, optionKey);
    } else {
      // Handle individual toggles (original behavior)
      let isActive = $(this).hasClass('active');
      isActive = !isActive;
      
      if (isActive) {
        $(this).addClass('active');
        $(this).find('.toggle-button').addClass('active');
      } else {
        $(this).removeClass('active');
        $(this).find('.toggle-button').removeClass('active');
      }
      
      Shiny.setInputValue(toggleId, isActive);
    }
  });
});