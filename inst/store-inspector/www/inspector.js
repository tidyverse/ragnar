(function () {
  function initInstance(root) {
    // For a single inspector instance: wire its resizer inside root
    var res = root.querySelector('[role="separator"]');
    if (!res) return;

    var dragging = false, startX = 0, startWidth = 0, bbox = null;

    var key = "ragnar_split_width_px";
    var save = function (px) {
      try {
        localStorage.setItem(key, String(px));
      } catch (e) {}
    };
    var load = function () {
      try {
        return parseInt(localStorage.getItem(key) || "", 10);
      } catch (e) {
        return NaN;
      }
    };
    var apply = function (px) {
      if (!isFinite(px)) return;
      root.style.setProperty("--left-pane-width", px + "px");
    };
    var initWidth = load();
    if (isFinite(initWidth) && initWidth > 200) apply(initWidth);
    function updateAria(px) {
      try {
        var left = root.querySelector(".left-pane");
        var w = isFinite(px)
          ? px
          : (left ? left.getBoundingClientRect().width : NaN);
        var rect = root.getBoundingClientRect();
        var min = 200;
        var max = Math.round(rect.width * 0.8);
        res.setAttribute("aria-valuemin", String(min));
        res.setAttribute("aria-valuemax", String(max));
        if (isFinite(w)) {
          res.setAttribute("aria-valuenow", String(Math.round(w)));
        }
      } catch (_) {}
    }
    updateAria(initWidth);

    function resetToGolden() {
      var rect = root.getBoundingClientRect();
      var target = Math.max(200, Math.round(rect.width * 0.382));
      apply(target);
      updateAria(target);
      save(target);
    }

    function onMove(e) {
      if (!dragging) return;
      var dx = e.clientX - startX;
      var newPx = Math.max(200, Math.min(bbox.width * 0.8, startWidth + dx));
      apply(newPx);
      updateAria(newPx);
      e.preventDefault();
    }

    function onUp(e) {
      if (!dragging) return;
      dragging = false;
      var left = root.querySelector(".left-pane");
      var w = left ? left.getBoundingClientRect().width : NaN;
      if (isFinite(w)) save(Math.round(w));
      document.body.style.userSelect = "";
      document.body.style.cursor = "";
      root.classList.remove("resizing");
      updateAria(w);
      root.querySelectorAll("iframe").forEach(function (ifr) {
        try {
          ifr.style.pointerEvents = ifr.dataset.prevPe || "";
          delete ifr.dataset.prevPe;
        } catch (_) {}
      });
      // no overlay to clean up
    }

    res.addEventListener("mousedown", function (e) {
      if (e.detail && e.detail >= 2) {
        resetToGolden();
        e.preventDefault();
        e.stopPropagation();
        return;
      }
      dragging = true;
      startX = e.clientX;
      bbox = root.getBoundingClientRect();
      var left = root.querySelector(".left-pane");
      startWidth = left ? left.getBoundingClientRect().width : 0;
      document.body.style.userSelect = "none";
      document.body.style.cursor = "col-resize";
      root.classList.add("resizing");
      root.querySelectorAll("iframe").forEach(function (ifr) {
        try {
          ifr.dataset.prevPe = ifr.style.pointerEvents || "";
        } catch (_) {}
        ifr.style.pointerEvents = "none";
      });
      e.preventDefault();
      e.stopPropagation();
    }, true);

    window.addEventListener("mousemove", onMove, true);
    window.addEventListener("mouseup", onUp, true);

    res.addEventListener("keydown", function (e) {
      var left = root.querySelector(".left-pane");
      if (!left) return;
      var rect = root.getBoundingClientRect();
      var current = left.getBoundingClientRect().width;
      var min = 200, max = rect.width * 0.8;
      var step = e.shiftKey ? 48 : 24;
      var handled = true;
      if (e.key === "ArrowLeft") {
        current = Math.max(min, current - step);
      } else if (e.key === "ArrowRight") {
        current = Math.min(max, current + step);
      } else if (
        e.key === "Home" || (e.key.toLowerCase && e.key.toLowerCase() === "g")
      ) {
        current = Math.max(min, Math.round(rect.width * 0.382));
      } else if (e.key === "End") {
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

  function initAll() {
    var roots = document.querySelectorAll(".content-split");
    roots.forEach(initInstance);
  }

  // Global shortcuts: '/' to focus search, 'Esc' to clear
  function initShortcuts() {
    var q = document.querySelector('[data-inspector-query="1"]');
    if (!q) return;
    var isEditable = function (el) {
      if (!el) return false;
      var t = el.tagName;
      return t === "INPUT" || t === "TEXTAREA" || el.isContentEditable;
    };
    window.addEventListener("keydown", function (e) {
      if (e.defaultPrevented) return;
      var active = document.activeElement;
      // '/' focuses search when not typing in another field
      if (
        e.key === "/" && !e.ctrlKey && !e.metaKey && !e.altKey &&
        !isEditable(active)
      ) {
        e.preventDefault();
        try {
          q.focus();
          q.select();
        } catch (_) {}
        return;
      }
      // Escape clears search if it has content
      if (e.key === "Escape") {
        if (q.value && q.value.length) {
          q.value = "";
          try {
            q.dispatchEvent(new Event("input", { bubbles: true }));
          } catch (_) {}
        }
      }
    }, true);
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", function () {
      initAll();
      initShortcuts();
    }, { once: true });
  } else {
    initAll();
    initShortcuts();
  }
})();
