// map_layout.js - Handle layout switching without DOM recreation

Shiny.addCustomMessageHandler('updateMapLayout', function(message) {
  var container = document.getElementById(message.containerId);
  if (container) {
    // Remove all layout classes
    container.classList.remove('layout-1', 'layout-2', 'layout-4');
    // Add new layout class
    container.classList.add(message.layout.replace('layout', 'layout-'));
    
    console.log('Layout updated to:', message.layout);
    
    // Trigger resize on all visible maps after a short delay
    setTimeout(function() {
      window.dispatchEvent(new Event('resize'));
    }, 100);
  }
});

Shiny.addCustomMessageHandler('invalidateMaps', function(message) {
  // Trigger resize to fix any rendering issues
  setTimeout(function() {
    window.dispatchEvent(new Event('resize'));
  }, 200);
});