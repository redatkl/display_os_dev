// Map Synchronization Manager
window.MapSyncManager = (function() {
  let syncedMaps = [];
  let synchronizing = false;
  let syncHandlers = [];
  
  // Helper to find map by namespaced ID
  function findMap(mapId) {
    const element = document.getElementById(mapId);
    if (element && window.HTMLWidgets) {
      const widget = HTMLWidgets.find('#' + mapId);
      if (widget) {
        return widget.getMap();
      }
    }
    return null;
  }
  
  // Synchronize a specific map with all others
  function syncMapWithOthers(sourceMap, targetMaps) {
    if (!synchronizing) {
      synchronizing = true;
      const center = sourceMap.getCenter();
      const zoom = sourceMap.getZoom();
      
      targetMaps.forEach(function(targetMap) {
        if (targetMap && targetMap !== sourceMap) {
          targetMap.setView(center, zoom, {animate: false});
        }
      });
      
      setTimeout(function() { 
        synchronizing = false; 
      }, 10);
    }
  }
  
  // Remove all existing sync handlers
  function clearSync() {
    syncHandlers.forEach(function(handler) {
      if (handler.map && handler.event && handler.fn) {
        handler.map.off(handler.event, handler.fn);
      }
    });
    syncHandlers = [];
    syncedMaps = [];
  }
  
  // Set up synchronization for multiple maps
  function setupSync(mapIds) {
    console.log('Setting up map sync for:', mapIds);
    
    // Clear previous sync
    clearSync();
    
    // Wait for maps to initialize
    setTimeout(function() {
      const maps = [];
      
      // Find all maps
      mapIds.forEach(function(mapId) {
        const map = findMap(mapId);
        if (map) {
          maps.push(map);
          console.log('Found map:', mapId);
        } else {
          console.warn('Map not found:', mapId);
        }
      });
      
      // Only sync if we have at least 2 maps
      if (maps.length < 2) {
        console.warn('Not enough maps found for sync. Found:', maps.length);
        return;
      }
      
      syncedMaps = maps;
      
      // Set up event handlers for each map
      maps.forEach(function(sourceMap) {
        // Create target maps array (all maps except source)
        const targetMaps = maps.filter(function(m) { return m !== sourceMap; });
        
        // Move event
        const moveHandler = function() {
          syncMapWithOthers(sourceMap, targetMaps);
        };
        sourceMap.on('move', moveHandler);
        syncHandlers.push({map: sourceMap, event: 'move', fn: moveHandler});
        
        // Zoom event
        const zoomHandler = function() {
          syncMapWithOthers(sourceMap, targetMaps);
        };
        sourceMap.on('zoom', zoomHandler);
        syncHandlers.push({map: sourceMap, event: 'zoom', fn: zoomHandler});
        
        // Drag event for smoother panning
        const dragHandler = function() {
          if (!synchronizing) {
            synchronizing = true;
            const center = sourceMap.getCenter();
            targetMaps.forEach(function(targetMap) {
              if (targetMap) {
                targetMap.panTo(center, {animate: false, duration: 0});
              }
            });
            setTimeout(function() { 
              synchronizing = false; 
            }, 5);
          }
        };
        sourceMap.on('drag', dragHandler);
        syncHandlers.push({map: sourceMap, event: 'drag', fn: dragHandler});
      });
      
      console.log('Map sync setup complete for', maps.length, 'maps');
    }, 500);
  }
  
  // Public API
  return {
    sync: setupSync,
    clear: clearSync
  };
})();

// Shiny custom message handler
if (window.Shiny) {
  Shiny.addCustomMessageHandler('syncMaps', function(message) {
    console.log('Received syncMaps message:', message);
    if (message.mapIds && message.mapIds.length > 0) {
      window.MapSyncManager.sync(message.mapIds);
    } else {
      window.MapSyncManager.clear();
    }
  });
}