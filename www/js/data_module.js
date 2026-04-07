Shiny.addCustomMessageHandler('setActiveCard', function(msg) {
  // Remove active from all cards in this module
  document.querySelectorAll('[id^="' + msg.ns + 'card_"]').forEach(function(el) {
    el.classList.remove('active');
  });
  // Add active to the clicked card
  var target = document.getElementById(msg.ns + 'card_' + msg.active);
  if (target) target.classList.add('active');
});