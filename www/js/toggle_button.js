$(document).ready(function() {
  console.log("Toggle script loaded");
  console.log("Found toggles:", $('.toggle-container').length);
  
  // Handle clicks on group toggle containers
  $('.toggle-container').click(function() {
    
    // Check if disabled - improved check
    if ($(this).hasClass('disabled') || $(this).attr('data-disabled') === 'true') {
      console.log("Toggle is disabled, ignoring click");
      return false;
    }
    
    let toggleId = $(this).data('toggle-id');
    let groupId = $(this).data('group-id');
    let optionKey = $(this).data('option-key');
    
    console.log("Clicked toggle:", {toggleId, groupId, optionKey});
    
    if (groupId) {
      // Deactivate all toggles in this group (except disabled ones)
      $('[data-group-id="' + groupId + '"]').not('.disabled').removeClass('active');
      $('[data-group-id="' + groupId + '"] .toggle-button').not('.disabled .toggle-button').removeClass('active');
      
      // Activate only the clicked toggle
      $(this).addClass('active');
      $(this).find('.toggle-button').addClass('active');
      
      console.log("Setting input:", groupId, "=", optionKey);
      Shiny.setInputValue(groupId, optionKey, {priority: "event"});
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

// Ensure Shiny is loaded before adding custom handler
if (typeof Shiny !== 'undefined') {
  // Handler to update toggle switch from server
  Shiny.addCustomMessageHandler('updateToggleSwitch', function(message) {
    var groupId = message.id;
    var value = message.value;
    
    console.log("Updating toggle switch:", groupId, "to value:", value);
    
    // Find all toggle containers in this group and deactivate them (except disabled)
    $('[data-group-id="' + groupId + '"]').not('.disabled').removeClass('active');
    $('[data-group-id="' + groupId + '"] .toggle-button').not('.disabled .toggle-button').removeClass('active');
    
    // Find and activate the specific option (if not disabled)
    var targetToggle = $('[data-group-id="' + groupId + '"][data-option-key="' + value + '"]');
    if (!targetToggle.hasClass('disabled') && targetToggle.attr('data-disabled') !== 'true') {
      targetToggle.addClass('active');
      targetToggle.find('.toggle-button').addClass('active');
    }
    
    // Update the Shiny input value (without triggering event to avoid loop)
    Shiny.setInputValue(groupId, value);
  });
} else {
  console.error("Shiny not found - custom message handler not registered");
}