(function(){
  function init() {
    // Draggable resizer logic for content split
    var res = document.getElementById('split-resizer');
    var root = document.querySelector('.content-split');
    if (!res || !root) return;

    var dragging = false, startX = 0, startWidth = 0, bbox = null;
    var save = function(px){ try { localStorage.setItem('ragnar_split_width_px', String(px)); } catch(e){} };
    var load = function(){ try { return parseInt(localStorage.getItem('ragnar_split_width_px')||'',10); } catch(e){ return NaN; } };
    var apply = function(px){ if (!isFinite(px)) return; root.style.setProperty('--left-pane-width', px + 'px'); };
    var init = load(); if (isFinite(init) && init > 200) apply(init);
    // Set ARIA min/max/value
    function updateAria(px){
      try {
        var left = root.querySelector('.left-pane');
        var w = isFinite(px) ? px : (left ? left.getBoundingClientRect().width : NaN);
        var rect = root.getBoundingClientRect();
        var min = 200;
        var max = Math.round(rect.width * 0.8);
        res.setAttribute('aria-valuemin', String(min));
        res.setAttribute('aria-valuemax', String(max));
        if (isFinite(w)) res.setAttribute('aria-valuenow', String(Math.round(w)));
      } catch(_){}
    }
    updateAria(init);

    var overlay = null;
    function resetToGolden() {
      var rect = root.getBoundingClientRect();
      var target = Math.max(200, Math.round(rect.width * 0.382));
      apply(target); updateAria(target); save(target);
    }

    res.addEventListener('mousedown', function(e){
      // Detect double-click via event.detail before creating overlay
      if (e.detail && e.detail >= 2) {
        resetToGolden();
        e.preventDefault();
        e.stopPropagation();
        return;
      }

      dragging = true; startX = e.clientX; bbox = root.getBoundingClientRect();
      var left = root.querySelector('.left-pane'); startWidth = left ? left.getBoundingClientRect().width : 0;
      document.body.style.userSelect = 'none';
      document.body.style.cursor = 'col-resize';
      root.classList.add('resizing');
      // Disable pointer events on iframes under the split while dragging
      root.querySelectorAll('iframe').forEach(function(ifr){
        try { ifr.dataset.prevPe = ifr.style.pointerEvents || ''; } catch(_){}
        ifr.style.pointerEvents = 'none';
      });
      // Create fullscreen transparent overlay to capture events
      overlay = document.createElement('div');
      overlay.style.cssText = 'position: fixed; inset: 0; cursor: col-resize; z-index: 2147483647; background: transparent;';
      document.body.appendChild(overlay);
      // Also listen on overlay for robustness
      overlay.addEventListener('mousemove', onMove, true);
      overlay.addEventListener('mouseup', onUp, true);
      e.preventDefault();
      e.stopPropagation();
    }, true);

    window.addEventListener('mousemove', function(e){
      if (!dragging) return;
      var dx = e.clientX - startX; var newPx = Math.max(200, Math.min(bbox.width * 0.8, startWidth + dx));
      apply(newPx);
      updateAria(newPx);
      e.preventDefault();
    }, true);

    window.addEventListener('mouseup', function(e){
      if (!dragging) return; dragging = false;
      var left = root.querySelector('.left-pane'); var w = left ? left.getBoundingClientRect().width : NaN;
      if (isFinite(w)) save(Math.round(w));
      document.body.style.userSelect = '';
      document.body.style.cursor = '';
      root.classList.remove('resizing');
      updateAria(w);
      // Restore iframe pointer events
      root.querySelectorAll('iframe').forEach(function(ifr){
        try { ifr.style.pointerEvents = ifr.dataset.prevPe || ''; delete ifr.dataset.prevPe; } catch(_){}
      });
      // Remove overlay and its listeners
      if (overlay) {
        try { overlay.removeEventListener('mousemove', onMove, true); overlay.removeEventListener('mouseup', onUp, true); } catch(_){}
        try { overlay.remove(); } catch(_){}
        overlay = null;
      }
    }, true);

    // (dblclick handled via mousedown detail check)

    // Keyboard resizing for accessibility
    res.addEventListener('keydown', function(e){
      var left = root.querySelector('.left-pane');
      if (!left) return;
      var rect = root.getBoundingClientRect();
      var current = left.getBoundingClientRect().width;
      var min = 200, max = rect.width * 0.8;
      var step = (e.shiftKey ? 48 : 24);
      var handled = true;
      if (e.key === 'ArrowLeft') {
        current = Math.max(min, current - step);
      } else if (e.key === 'ArrowRight') {
        current = Math.min(max, current + step);
      } else if (e.key === 'Home' || (e.key.toLowerCase && e.key.toLowerCase() === 'g')) {
        // Home (Fn+Left on Mac) or 'g' to reset to golden ratio
        current = Math.max(min, Math.round(rect.width * 0.382));
      } else if (e.key === 'End') {
        current = Math.min(max, Math.round(rect.width * 0.8));
      } else {
        handled = false;
      }
      if (handled) {
        e.preventDefault();
        apply(current);
        updateAria(current);
        save(Math.round(current));
      }
    }, true);
  }

  // Global shortcuts: '/' to focus search, 'Esc' to clear
  function initShortcuts() {
    var q = document.querySelector('[data-inspector-query="1"]');
    if (!q) return;
    var isEditable = function(el){
      if (!el) return false;
      var t = el.tagName;
      return t === 'INPUT' || t === 'TEXTAREA' || el.isContentEditable;
    };
    window.addEventListener('keydown', function(e){
      if (e.defaultPrevented) return;
      var active = document.activeElement;
      // '/' focuses search when not typing in another field
      if (e.key === '/' && !e.ctrlKey && !e.metaKey && !e.altKey && !isEditable(active)) {
        e.preventDefault();
        try { q.focus(); q.select(); } catch(_){}
        return;
      }
      // Escape clears search if it has content
      if (e.key === 'Escape') {
        if (q.value && q.value.length) {
          q.value = '';
          try { q.dispatchEvent(new Event('input', { bubbles: true })); } catch(_){}
        }
      }
    }, true);
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', function(){ init(); initShortcuts(); }, { once: true });
  } else {
    init();
    initShortcuts();
  }
})();
