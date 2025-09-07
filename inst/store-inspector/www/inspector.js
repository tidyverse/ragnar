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

    var overlay = null;
    res.addEventListener('mousedown', function(e){
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
      e.preventDefault();
    }, true);

    window.addEventListener('mouseup', function(e){
      if (!dragging) return; dragging = false;
      var left = root.querySelector('.left-pane'); var w = left ? left.getBoundingClientRect().width : NaN;
      if (isFinite(w)) save(Math.round(w));
      document.body.style.userSelect = '';
      document.body.style.cursor = '';
      root.classList.remove('resizing');
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
