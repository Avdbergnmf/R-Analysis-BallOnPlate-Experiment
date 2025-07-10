// Page 16 Interactions - Extracted from vibe coding mess
// =====================================================

// Configuration
const CONFIG = {
  DRAG_DROP_RESET_DELAY: 4000,
  DRAG_DROP_VALID_TYPES: ['text/csv', 'application/csv', 'text/comma-separated-values'],
  DRAG_DROP_VALID_EXTENSIONS: ['.csv', '.CSV']
};

// Initialize when DOM is ready
$(document).ready(function() {
  initializePage16Interactions();
});

function initializePage16Interactions() {
  setupPageFocus();
  setupKeyboardHandlers();
  setupDragDropZones();
}

// Make page focusable for keyboard events
function setupPageFocus() {
  $('body').attr('tabindex', -1).focus();
}

// Keyboard event handlers
function setupKeyboardHandlers() {
  $(window).on('keydown', handleKeyDown);
}

function handleKeyDown(e) {
  // Prevent handling if in input fields
  if ($(e.target).is('input, textarea, select')) {
    return;
  }
  
  // Ignore auto-repeat events
  if (e.repeat) {
    return;
  }
  
  // Handle specific keys
  switch(e.key) {
    case 'Tab':
      if (!e.shiftKey && !e.ctrlKey && !e.altKey && !e.metaKey) {
        e.preventDefault();
        toggleOutlierMode();
      }
      break;
      
    case 'ArrowLeft':
      if (!e.shiftKey && !e.ctrlKey && !e.altKey && !e.metaKey) {
        e.preventDefault();
        navigateToSpecial('previous');
      }
      break;
      
    case 'ArrowRight':
      if (!e.shiftKey && !e.ctrlKey && !e.altKey && !e.metaKey) {
        e.preventDefault();
        navigateToSpecial('next');
      }
      break;
  }
}

// Outlier mode toggle
function toggleOutlierMode() {
  const current = $('input[name="outlier_mode"]:checked').val();
  const next = (current === 'heel') ? 'step' : 'heel';
  
  $('input[name="outlier_mode"][value="' + next + '"]')
    .prop('checked', true)
    .trigger('change');
}

// Navigation functions
function navigateToSpecial(direction) {
  const inputName = (direction === 'previous') ? 'nav_prev_special' : 'nav_next_special';
  Shiny.setInputValue(inputName, Math.random(), {priority: 'event'});
}

// Drag & Drop functionality
function setupDragDropZones() {
  setupDropZone('#heelstrike-drop-zone', '#importHeelStrikeOutliers');
  setupDropZone('#step-drop-zone', '#importStepOutliers');
}

function setupDropZone(zoneSelector, inputSelector) {
  const $zone = $(zoneSelector);
  
  // File input change handler
  $(document).on('change', inputSelector + ' input[type="file"]', function() {
    handleFileSelect(this, zoneSelector);
  });
  
  // Drag & drop event handlers
  $zone.on('dragenter dragover', function(e) {
    e.preventDefault();
    e.stopPropagation();
    
    if (!$(this).hasClass('success')) {
      $(this).addClass('dragover');
      $(this).find('.drag-drop-hint').text('Drop CSV file here!');
    }
  });
  
  $zone.on('dragleave', function(e) {
    e.preventDefault();
    e.stopPropagation();
    
    // Only remove dragover if we're actually leaving the zone
    if (!$(e.currentTarget).is(e.relatedTarget) && !$(e.currentTarget).has(e.relatedTarget).length) {
      $(this).removeClass('dragover');
      
      if (!$(this).hasClass('success')) {
        $(this).find('.drag-drop-hint').text('Drag & drop CSV file here or click to browse');
      }
    }
  });
  
  $zone.on('drop', function(e) {
    e.preventDefault();
    e.stopPropagation();
    
    $(this).removeClass('dragover');
    
    const files = e.originalEvent.dataTransfer.files;
    if (files.length > 0) {
      handleDroppedFile(files[0], zoneSelector, inputSelector);
    }
  });
}

function handleFileSelect(fileInput, zoneSelector) {
  const $dropZone = $(zoneSelector);
  
  if (fileInput.files && fileInput.files.length > 0) {
    const file = fileInput.files[0];
    
    if (isValidCSVFile(file)) {
      showDropZoneSuccess($dropZone, file.name);
    } else {
      showDropZoneError($dropZone, 'Invalid file type. Please select a CSV file.');
    }
  }
}

function handleDroppedFile(file, zoneSelector, inputSelector) {
  const $dropZone = $(zoneSelector);
  
  if (!isValidCSVFile(file)) {
    showDropZoneError($dropZone, 'Invalid file type. Please drop a CSV file.');
    return;
  }
  
  // Create a new file input event
  const fileInput = $(inputSelector + ' input[type="file"]')[0];
  const dataTransfer = new DataTransfer();
  dataTransfer.items.add(file);
  fileInput.files = dataTransfer.files;
  
  // Trigger the change event
  $(fileInput).trigger('change');
  
  showDropZoneSuccess($dropZone, file.name);
}

function isValidCSVFile(file) {
  // Check MIME type
  if (CONFIG.DRAG_DROP_VALID_TYPES.includes(file.type)) {
    return true;
  }
  
  // Check file extension if MIME type is not recognized
  const fileName = file.name.toLowerCase();
  return CONFIG.DRAG_DROP_VALID_EXTENSIONS.some(ext => 
    fileName.endsWith(ext.toLowerCase())
  );
}

function showDropZoneSuccess($dropZone, fileName) {
  $dropZone.removeClass('processing dragover error');
  $dropZone.addClass('success');
  $dropZone.find('.drag-drop-hint').text('File loaded: ' + fileName);
}

function showDropZoneError($dropZone, message) {
  $dropZone.removeClass('processing dragover success');
  $dropZone.addClass('error');
  $dropZone.find('.drag-drop-hint').text(message);
  
  // Reset after delay
  setTimeout(function() {
    resetDropZone($dropZone);
  }, CONFIG.DRAG_DROP_RESET_DELAY);
}

function resetDropZone($dropZone) {
  $dropZone.removeClass('processing dragover error');
  
  // Only reset to default state if no file is loaded
  if (!$dropZone.hasClass('success')) {
    $dropZone.find('.drag-drop-hint').text('Drag & drop CSV file here or click to browse');
  }
}

function clearDropZone($dropZone) {
  $dropZone.removeClass('processing dragover success error');
  $dropZone.find('.drag-drop-hint').text('Drag & drop CSV file here or click to browse');
}

// Utility functions
function showStatus(message, type = 'info', duration = 3000) {
  // This would integrate with your notification system
  console.log(`[${type.toUpperCase()}] ${message}`);
}

// Export for testing or external use
window.Page16Interactions = {
  toggleOutlierMode,
  navigateToSpecial,
  isValidCSVFile,
  resetDropZone,
  clearDropZone
};