// sidebar_custom.js

$(document).ready(function() {
  // Initialize variables
  let currentPanel = null;
  let isPanelOpen = false;
  
    // ── Helper: tell every Leaflet map to recalculate its size ───────────────
  function invalidateAllMaps(delay) {
    delay = delay || 80;
    setTimeout(function() {
      window.dispatchEvent(new Event('resize'));
      if (window.HTMLWidgets) {
        var mapEls = document.querySelectorAll('.leaflet.html-widget');
        mapEls.forEach(function(el) {
          var widget = HTMLWidgets.find('#' + el.id);
          if (widget && widget.getMap) {
            try { widget.getMap().invalidateSize(); } catch(e) {}
          }
        });
      }
    }, delay);
  }
  
  // Handle icon clicks
  $('.sidebar-icon').on('click', function() {
    const clickedIcon = $(this);
    const panelId = clickedIcon.data('panel');
    const panel = $('#' + panelId);
    const panelsContainer = $('.sidebar-panels');
    const mainContent = $('.main-content-area');
    
    // If clicking the same icon that's already active
    if (clickedIcon.hasClass('active') && isPanelOpen) {
      // Close the panel
      closePanel();
    } else {
      // Remove active class from all icons
      $('.sidebar-icon').removeClass('active');
      
      // Add active class to clicked icon
      clickedIcon.addClass('active');
      
      // Hide all panels
      $('.sidebar-panel').removeClass('active');
      
      // Show the selected panel
      panel.addClass('active');
      
      // Expand panels container if not already expanded
      if (!isPanelOpen) {
        panelsContainer.addClass('expanded');
        mainContent.addClass('sidebar-expanded');
        isPanelOpen = true;
      }
      
      currentPanel = panelId;
   // Send panel change to Shiny
      if (window.Shiny) {
        // Extract panel type from panelId (panel1 -> climate, panel2 -> vegetation, etc.)
        const panelTypeMap = {
          'panel1': 'climate',
          'panel2': 'vegetation',
          'panel3': 'water',
          'panel4': 'soil',
          'panel5': 'combined'
        };
        
        const panelType = panelTypeMap[panelId.split('-').pop()] || 'climate';
        
        Shiny.setInputValue('active_panel_type', panelType, {priority: 'event'});
        console.log('Active panel changed to:', panelType);
      }
      
      invalidateAllMaps(120);
    }
  });
  
  // Function to close panel
  function closePanel() {
    $('.sidebar-icon').removeClass('active');
    $('.sidebar-panel').removeClass('active');
    $('.sidebar-panels').removeClass('expanded');
    $('.main-content-area').removeClass('sidebar-expanded');
    isPanelOpen = false;
    currentPanel = null;
    
    
   // KEY FIX: checkResponsive() sets an inline style="width:280px" while the
    // panel is open. Inline styles beat CSS rules, so even after .expanded is
    // removed the element keeps its width and leaves a gray ghost rectangle.
    // Clearing the inline width lets the CSS "width: 0" rule take over again.
    $('.sidebar-panels').css('width', '');

    invalidateAllMaps(120);
  }
  
  // Close panel when clicking outside
  $(document).on('click', function(e) {
    const sidebar = $('.custom-sidebar');
    const isClickInsideSidebar = sidebar.has(e.target).length > 0 || sidebar.is(e.target);
    
    if (!isClickInsideSidebar && isPanelOpen) {
      closePanel();
    }
  });
  
  // Keyboard navigation
  $(document).on('keydown', function(e) {
    // ESC key to close panel
    if (e.keyCode === 27 && isPanelOpen) {
      closePanel();
    }
  });
  
  // Add hover effects for better UX
  $('.sidebar-icon').hover(
    function() {
      if (!$(this).hasClass('active')) {
        $(this).css('transform', 'scale(1.1)');
      }
    },
    function() {
      if (!$(this).hasClass('active')) {
        $(this).css('transform', 'scale(1)');
      }
    }
  );
  
  // Handle panel transitions
  $('.sidebar-panels').on('transitionend', function() {
    if (!$(this).hasClass('expanded')) {
      $('.sidebar-panel').hide();
    } else {
      $('.sidebar-panel.active').show();
    }
    
    invalidateAllMaps(50);
  });
  
  // Initialize Shiny input bindings for custom interactions
  if (window.Shiny) {
    // Send panel state to Shiny
    Shiny.addCustomMessageHandler('updatePanelState', function(message) {
      if (message.action === 'open' && message.panel) {
        const icon = $(`.sidebar-icon[data-panel="${message.panel}"]`);
        icon.click();
      } else if (message.action === 'close') {
        closePanel();
      }
    });
    
    // Track panel state
    $('.sidebar-icon').on('click', function() {
      const panelId = $(this).data('panel');
      const isOpen = $(this).hasClass('active');
      
      // Send state to Shiny
      if (window.Shiny) {
        Shiny.setInputValue('sidebar_panel_state', {
          panel: panelId,
          isOpen: isOpen,
          timestamp: new Date().getTime()
        });
      }
    });
  }
  
  // Smooth scroll for panel content
  $('.sidebar-panel').on('scroll', function() {
    const scrollTop = $(this).scrollTop();
    if (scrollTop > 100) {
      // Add shadow to panel header when scrolled
      $(this).find('h4').css('box-shadow', '0 2px 4px rgba(0,0,0,0.1)');
    } else {
      $(this).find('h4').css('box-shadow', 'none');
    }
  });
  
  // Animation queue for smooth transitions
  function animatePanel(panel, callback) {
    panel.fadeIn(300, function() {
      if (callback) callback();
    });
  }
  
  // Responsive handling
  function checkResponsive() {
// Only set inline width when open — closePanel() clears it on close
    if (isPanelOpen) {
      const windowWidth = $(window).width();
      $('.sidebar-panels').css('width', windowWidth < 768 ? '240px' : '280px');
    }
  }
  $(window).resize(checkResponsive);
  
  // Initialize tooltips with delay
  $('.sidebar-icon, .map-layout-icon').on('mouseenter', function() {
    const tooltip = $(this).find('.icon-tooltip');
    setTimeout(function() {
      tooltip.addClass('show');
    }, 500);
  }).on('mouseleave', function() {
    $(this).find('.icon-tooltip').removeClass('show');
  });
  
  
  
  // Handle map layout icon clicks
$('.map-layout-icon').on('click', function() {
  const clickedIcon = $(this);
  const layoutType = clickedIcon.data('map-layout');
  const inputId = clickedIcon.data('input-id');
  
  // Remove active-layout class from all map layout icons
  $('.map-layout-icon').removeClass('active-layout');
  
  // Add active-layout class to clicked icon
  clickedIcon.addClass('active-layout');
  
   // Send layout type to Shiny with the correct namespaced ID
  if (window.Shiny && inputId) {
    Shiny.setInputValue(inputId, layoutType, {priority: 'event'});
    console.log('Sent to Shiny:', inputId, '=', layoutType);
  }
  
  
  // Optional: Add console log for debugging
  console.log('Map layout selected:', layoutType);
});

// Add hover effects for map layout icons
$('.map-layout-icon').hover(
  function() {
    if (!$(this).hasClass('active-layout')) {
      $(this).css('transform', 'scale(1.1)');
    }
  },
  function() {
    if (!$(this).hasClass('active-layout')) {
      $(this).css('transform', 'scale(1)');
    }
  }
);

// Initialize first layout as active (optional)
$('.map-layout-icon[data-map-layout="layout1"]').addClass('active-layout');
  
  
  
});
