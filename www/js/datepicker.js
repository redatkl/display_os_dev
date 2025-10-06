// datepicker.js
class DatePicker {
  constructor(id) {
    this.id = id;
    this.input = $('#' + id);
    this.popup = $('#' + id + '_popup');
    this.selected = new Date(this.input.val());
    this.view = new Date(this.selected);
    this.months = ['January', 'February', 'March', 'April', 'May', 'June',
                   'July', 'August', 'September', 'October', 'November', 'December'];
    this.days = ['Su', 'Mo', 'Tu', 'We', 'Th', 'Fr', 'Sa'];
    this.init();
  }
  
  init() {
    // Toggle popup on input click
    this.input.on('click', (e) => {
      e.stopPropagation();
      $('.dp-popup').removeClass('show');
      this.popup.toggleClass('show');
      if (this.popup.hasClass('show')) {
        this.render();
      }
    });
    
    // Previous month
    this.popup.on('click', '.dp-prev', () => {
      this.view.setMonth(this.view.getMonth() - 1);
      this.render();
    });
    
    // Next month
    this.popup.on('click', '.dp-next', () => {
      this.view.setMonth(this.view.getMonth() + 1);
      this.render();
    });
    
    // Day selection
    this.popup.on('click', '.dp-day:not(.other)', (e) => {
      const day = parseInt($(e.target).text());
      this.selected = new Date(this.view.getFullYear(), this.view.getMonth(), day);
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
    this.renderHeader();
    this.renderCalendar();
  }
  
  renderHeader() {
    const month = this.months[this.view.getMonth()];
    const year = this.view.getFullYear();
    this.popup.find('.dp-month').text(`${month} ${year}`);
  }
  
  renderCalendar() {
    const grid = this.popup.find('.dp-grid');
    grid.empty();
    
    // Add day headers
    this.days.forEach(day => {
      grid.append(`<div class="dp-day-header">${day}</div>`);
    });
    
    // Calculate first day of month and starting date
    const year = this.view.getFullYear();
    const month = this.view.getMonth();
    const firstDay = new Date(year, month, 1);
    const startDate = new Date(firstDay);
    startDate.setDate(1 - firstDay.getDay());
    
    const today = new Date();
    today.setHours(0, 0, 0, 0);
    
    // Generate 42 days (6 weeks)
    for (let i = 0; i < 42; i++) {
      const currentDate = new Date(startDate);
      currentDate.setDate(startDate.getDate() + i);
      
      const dayCell = $(`<div class="dp-day">${currentDate.getDate()}</div>`);
      
      // Add classes
      if (currentDate.getMonth() !== month) {
        dayCell.addClass('other');
      }
      
      if (this.sameDay(currentDate, this.selected)) {
        dayCell.addClass('selected');
      }
      
      if (this.sameDay(currentDate, today)) {
        dayCell.addClass('today');
      }
      
      grid.append(dayCell);
    }
  }
  
  sameDay(date1, date2) {
    return date1.getDate() === date2.getDate() &&
           date1.getMonth() === date2.getMonth() &&
           date1.getFullYear() === date2.getFullYear();
  }
  
  updateInput() {
    const year = this.selected.getFullYear();
    const month = String(this.selected.getMonth() + 1).padStart(2, '0');
    const day = String(this.selected.getDate()).padStart(2, '0');
    const dateStr = `${year}-${month}-${day}`;
    this.input.val(dateStr).trigger('change');
  }
}