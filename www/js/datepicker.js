// datepicker.js
class DatePicker {
  constructor(id) {
    this.id = id;
    this.input = $('#' + id);
    this.popup = $('#' + id + '_popup');
    
    // Parse YYYY-MM format
    const val = this.input.val();
    const [year, month] = val.split('-').map(Number);
    this.selected = { year: year || new Date().getFullYear(), month: month || 1 };
    this.viewYear = this.selected.year;
    
    this.months = ['Jan', 'Fév', 'Mar', 'Avr', 'Mai', 'Juin',
                   'Jul', 'Août', 'Sep', 'Oct', 'Nov', 'Déc'];
    this.mode = 'year'; // 'year' or 'month'
    this.init();
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
      this.mode = 'month';
      this.render();
    });
    
    // Month selection
    this.popup.on('click', '.dp-month-cell:not(.disabled)', (e) => {
      this.selected.month = parseInt($(e.target).data('month'));
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
    const fullMonths = ['janvier', 'février', 'mars', 'avril', 'mai', 'juin',
                      'juillet', 'août', 'septembre', 'octobre', 'novembre', 'décembre'];
  
    const monthName = fullMonths[this.selected.month - 1];
    const dateStr = `${monthName} ${this.selected.year}`;
    this.input.val(dateStr).trigger('change');
  }
}