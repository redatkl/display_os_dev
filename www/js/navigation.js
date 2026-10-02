// navigation.js
// Shows nav buttons normally; collapses overflow ones under a hamburger button

$(document).ready(function () {

  // ── DOM refs ──────────────────────────────────────────────
  var $navbar       = $('.navbar');
  var $btnContainer = $('.navbar-buttons');
  var $hamburger    = $('#hamburger-btn');
  var $dropdown     = $('#nav-overflow-dropdown');
  var $overlay      = $('#nav-overlay');

  // ── State ─────────────────────────────────────────────────
  var currentPage = 'geo';

  // ── Active page highlight (from navigation.js original) ───
  $(document).on('shiny:inputchanged', function (e) {
    if (e.name === 'current_page') {
      setActivePage(e.value);
    }
  });

  // ── Hamburger toggle ──────────────────────────────────────
  $hamburger.on('click', function (e) {
    e.stopPropagation();
    var isOpen = $dropdown.hasClass('open');
    isOpen ? closeMenu() : openMenu();
  });

  $overlay.on('click', closeMenu);

  $(document).on('keydown', function (e) {
    if (e.key === 'Escape') closeMenu();
  });

  // Clicking a button inside the dropdown navigates and closes
  $dropdown.on('click', '.nav-btn', function () {
    var page = $(this).data('page');
    Shiny.setInputValue('current_page', page, { priority: 'event' });
    setActivePage(page);
    closeMenu();
  });

  // ── "Salons" submenu ──────────────────────────────────────
  var $submenu = $('#salons-submenu');

  function closeSubmenu () {
    $submenu.removeClass('open');
  }

  $btnContainer.on('click', '.nav-parent', function (e) {
    e.stopPropagation();
    if ($submenu.hasClass('open')) { closeSubmenu(); return; }
    var rect = this.getBoundingClientRect();
    $submenu.css({ top: rect.bottom + 4, left: rect.left }).addClass('open');
  });

  $submenu.on('click', '.nav-btn', closeSubmenu);
  $(document).on('click', closeSubmenu);
  $(window).on('resize', closeSubmenu);

  // ── Overflow detection ────────────────────────────────────
  function checkOverflow () {
    // 1. Show all buttons first so we can measure
    $btnContainer.find('.nav-btn').removeClass('nav-hidden');
    $hamburger.hide();
    $dropdown.empty();

    // 2. Let the browser reflow
    requestAnimationFrame(function () {
      var containerWidth  = $btnContainer.width();
      var $buttons        = $btnContainer.find('.nav-btn');
      var overflowButtons = [];
      var usedWidth       = 0;
      var hamburgerWidth  = 44; // approximate width of the hamburger button

      $buttons.each(function () {
        usedWidth += $(this).outerWidth(true);
      });

      // If everything fits, nothing to do
      if (usedWidth <= containerWidth) {
        return;
      }

      // 3. Hide buttons from the right until they fit
      //    (leave room for the hamburger button)
      usedWidth = 0;
      var fitsAll = true;

      $buttons.each(function () {
        usedWidth += $(this).outerWidth(true);
        if (usedWidth > containerWidth - hamburgerWidth) {
          overflowButtons.push(this);
          fitsAll = false;
        }
      });

      if (fitsAll) return;

      // 4. Hide overflowing buttons & build dropdown
      overflowButtons.forEach(function (btn) {
        var $btn = $(btn);
        $btn.addClass('nav-hidden');

        // Parent entry: list its sub-pages in the dropdown instead
        if ($btn.hasClass('nav-parent')) {
          $submenu.find('.nav-btn').each(function () {
            $dropdown.append($(this).clone(false).attr('data-page', $(this).data('page')));
          });
          return;
        }

        // Clone for dropdown, keep data-page and active state
        var $clone = $btn.clone(false)
          .removeClass('nav-hidden')
          .attr('data-page', $btn.data('page') || extractPage($btn));

        $dropdown.append($clone);
      });

      // 5. Show hamburger
      $hamburger.css('display', 'flex');

      // Sync active state in dropdown
      syncActiveInDropdown();
    });
  }

  // Helper: extract page id from onclick attr (fallback)
  function extractPage ($btn) {
    var onclick = $btn.attr('onclick') || '';
    var match   = onclick.match(/'([^']+)'/);
    return match ? match[1] : '';
  }

  function syncActiveInDropdown () {
    $dropdown.find('.nav-btn').removeClass('active');
    $dropdown.find('.nav-btn[data-page="' + currentPage + '"]').addClass('active');
  }

  function openMenu () {
    $hamburger.addClass('open');
    $dropdown.addClass('open');
    $overlay.addClass('open');
  }

  function closeMenu () {
    $hamburger.removeClass('open');
    $dropdown.removeClass('open');
    $overlay.removeClass('open');
  }

  function setActivePage (page) {
    currentPage = page;
    // Visible buttons
    $('.navbar-buttons .nav-btn').removeClass('active');
    $('#nav_' + page).addClass('active');
    // Highlight the "Salons" parent when one of its sub-pages is active
    $submenu.find('.nav-btn').removeClass('active');
    if ($submenu.find('.nav-btn[data-page="' + page + '"]').length) {
      $submenu.find('.nav-btn[data-page="' + page + '"]').addClass('active');
      $('#nav_salons').addClass('active');
    }
    // Dropdown buttons
    syncActiveInDropdown();
  }

  // ── Run on load & resize ──────────────────────────────────
  checkOverflow();

  var resizeTimer;
  $(window).on('resize', function () {
    clearTimeout(resizeTimer);
    resizeTimer = setTimeout(checkOverflow, 80);
  });
});