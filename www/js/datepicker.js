// datepicker.js
class DatePicker {
  constructor(id, options = {}) {
    this.id = id;
    this.input = $('#' + id);
    this.popup = $('#' + id + '_popup');
    this.temporalite = options.temporalite || 'mensuel'; // 'mensuel', 'decadaire', 'trimestriel' or 'annuel'
    
    // Parse YYYY-MM format
    const val = this.input.val();
    this.selected = this.parseValue(val);
    this.viewYear = this.selected.year;
    
    this.months = ['Jan', 'Fév', 'Mar', 'Avr', 'Mai', 'Juin',
                   'Jul', 'Août', 'Sep', 'Oct', 'Nov', 'Déc'];
    this.quarters = ['T1', 'T2', 'T3', 'T4'];
    this.decades = ['D1', 'D2', 'D3'];
    this.mode = 'year'; // 'year' or 'month'
    this.init();
  }
  
  
  parseValue(val) {
    const currentYear = new Date().getFullYear();
    const currentMonth = new Date().getMonth() + 1;
    const currentQuarter = Math.ceil(currentMonth / 3);
    
    if (!val) {
      return {
        year: currentYear,
        month: currentMonth,
        quarter: currentQuarter
      };
    }
    
    if (this.temporalite === 'annuel') {
      //format YYYY
      const year = parseInt(val) || currentYear;
      return { year, month: 1, quarter: 1 };
    } else if (this.temporalite === 'trimestriel') {
      //format YYYY-T1, YYYY-T2, YYYY-T3, YYYY-T4
      const match = val.match(/^(\d{4})-T(\d)$/);
      if (match) {
        return {
          year: parseInt(match[1]),
          month: 1,
          quarter: parseInt(match[2])
        };
      }
      return { year: currentYear, month: 1, quarter: currentQuarter };
    } else {
      // Format: YYYY-MM (mensuel)
      const [year, month] = val.split('-').map(Number);
      return {
        year: year || currentYear,
        month: month || currentMonth,
        quarter: Math.ceil((month || currentMonth) / 3)
      };
    }
  }
  
  setTemporalite(temporalite) {
    this.temporalite = temporalite;
    // Re-parse the input value with new temporalite
    const val = this.input.val();
    this.selected = this.parseValue(val);
    this.viewYear = this.selected.year;
    this.mode = 'year';
  }
  
  
    
  
  init() {
    // Toggle popup on input click
    this.input.on('click', (e) => {
      e.stopPropagation();
      $('.dp-popup').removeClass('show');
      this.mode = 'year';
      this.popup.toggleClass('show');
      if (this.popup.hasClass('show')) {
        this.render();
      }
    });
    
    // Previous decade
    this.popup.on('click', '.dp-prev', () => {
      if (this.mode === 'year') {
        this.viewYear -= 12;
        this.render();
      }
    });
    
    // Next decade
    this.popup.on('click', '.dp-next', () => {
      if (this.mode === 'year') {
        const currentYear = new Date().getFullYear();
        const nextViewYear = this.viewYear + 12;
        // Only navigate if next decade contains years <= current year
        if (nextViewYear <= currentYear) {
          this.viewYear = nextViewYear;
          this.render();
        }
      }
    });
    
    // Year selection
    this.popup.on('click', '.dp-year:not(.disabled)', (e) => {
      this.selected.year = parseInt($(e.target).text());
      
      // For annual mode, select immediately
      if (this.temporalite === 'annuel') {
        this.updateInput();
        this.popup.removeClass('show');
      } else if (this.temporalite === 'trimestriel') {
        this.mode = 'quarter';
        this.render();
      } else {
        // mensuel, decadaire, etc.
        this.mode = 'month';
        this.render();
      }
    });
    
    // Month selection
    this.popup.on('click', '.dp-month-cell:not(.disabled)', (e) => {
      this.selected.month = parseInt($(e.target).data('month'));
      this.updateInput();
      this.popup.removeClass('show');
    });
    
    // Quarter selection
    this.popup.on('click', '.dp-quarter-cell:not(.disabled)', (e) => {
      this.selected.quarter = parseInt($(e.target).data('quarter'));
      this.updateInput();
      this.popup.removeClass('show');
    });
    
    // Close on outside click
    $(document).on('click', () => {
      this.popup.removeClass('show');
    });
    
    // Prevent popup close when clicking inside
    this.popup.on('click', (e) => {
      e.stopPropagation();
    });
  }
  
  render() {
    if (this.mode === 'year') {
      this.renderYearGrid();
    } else if (this.mode === 'quarter') {
      this.renderQuarterGrid();
    } else {
      this.renderMonthGrid();
    }
  }
  
  renderYearGrid() {
    const currentYear = new Date().getFullYear();
    const startYear = Math.floor(this.viewYear / 12) * 12;
    const endYear = startYear + 11;
    
    this.popup.find('.dp-month').text(`${startYear} - ${endYear}`);
    
    const grid = this.popup.find('.dp-grid');
    grid.empty();
    
    // Create 4x3 grid of years
    for (let i = 0; i < 12; i++) {
      const year = startYear + i;
      const yearCell = $(`<div class="dp-year">${year}</div>`);
      
      // Disable future years
      if (year > currentYear) {
        yearCell.addClass('disabled');
      }
      
      if (year === this.selected.year) {
        yearCell.addClass('selected');
      }
      
      if (year === currentYear) {
        yearCell.addClass('today');
      }
      
      grid.append(yearCell);
    }
  }
  
  
    renderQuarterGrid() {
    this.popup.find('.dp-month').text(this.selected.year);
    
    const grid = this.popup.find('.dp-grid');
    grid.empty();
    
    const currentYear = new Date().getFullYear();
    const currentMonth = new Date().getMonth() + 1;
    const currentQuarter = Math.ceil(currentMonth / 3);
    
    // Create 2x2 grid of quarters
    this.quarters.forEach((quarterName, index) => {
      const quarterNum = index + 1;
      const quarterCell = $(`<div class="dp-quarter-cell" data-quarter="${quarterNum}">${quarterName}</div>`);
      
      // Disable future quarters if selected year is current year
      if (this.selected.year === currentYear && quarterNum > currentQuarter) {
        quarterCell.addClass('disabled');
      }
      
      if (quarterNum === this.selected.quarter) {
        quarterCell.addClass('selected');
      }
      
      if (this.selected.year === currentYear && quarterNum === currentQuarter) {
        quarterCell.addClass('today');
      }
      
      grid.append(quarterCell);
    });
  }
  
  
  
  
  renderMonthGrid() {
    this.popup.find('.dp-month').text(this.selected.year);
    
    const grid = this.popup.find('.dp-grid');
    grid.empty();
    
    const currentYear = new Date().getFullYear();
    const currentMonth = new Date().getMonth() + 1;
    
    // Create 4x3 grid of months
    this.months.forEach((monthName, index) => {
      const monthNum = index + 1;
      const monthCell = $(`<div class="dp-month-cell" data-month="${monthNum}">${monthName}</div>`);
      
      // Disable future months if selected year is current year
      if (this.selected.year === currentYear && monthNum > currentMonth) {
        monthCell.addClass('disabled');
      }
      
      if (monthNum === this.selected.month && this.selected.year === currentYear) {
        if (monthNum === currentMonth) {
          monthCell.addClass('today');
        }
      }
      
      if (monthNum === this.selected.month) {
        monthCell.addClass('selected');
      }
      
      grid.append(monthCell);
    });
  }
  
  updateInput() {
    const year = this.selected.year;
    let dateStr;
    if (this.temporalite === 'annuel') {
      // Format: YYYY
      dateStr = `${year}`;
    } else if (this.temporalite === 'trimestriel') {
      // Format: YYYY-Q1, YYYY-Q2, etc.
      dateStr = `${year}-T${this.selected.quarter}`;
    } else {
      // Format: YYYY-MM (mensuel, decadaire, etc.)
      const fullMonths = ['janvier', 'février', 'mars', 'avril', 'mai', 'juin',
                        'juillet', 'août', 'septembre', 'octobre', 'novembre', 'décembre'];
  
      const monthName = fullMonths[this.selected.month - 1];
      dateStr = `${monthName} ${this.selected.year}`;
    }
    this.input.val(dateStr).trigger('change');
  }
}


// Store datepicker instances by ID
const datePickerInstances = {};

// Initialize the datepicker when the element is ready
$(document).on('shiny:connected', function() {
  // Find all custom date pickers and initialize them
  $('[id$="custom_date"]').each(function() {
    const fullId = $(this).attr('id');
    datePickerInstances[fullId] = new DatePicker(fullId, {
      temporalite: 'mensuel'  // Initial value
    });
  });
});

// Listen for updates from Shiny
Shiny.addCustomMessageHandler('updateDatePickerTemporalite', function(message) {
  const picker = datePickerInstances[message.id];
  if (picker) {
    picker.setTemporalite(message.temporalite);
  }
});