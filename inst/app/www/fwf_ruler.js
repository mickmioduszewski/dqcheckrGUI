/**
 * dqcheckr FWF Visual Ruler
 * Provides interactive column-boundary placement over a shinyAce editor.
 * spec-client.md §22
 */
(function() {
  'use strict';

  var _charWidth = null;
  var _boundaries = [];  // array of 0-based char positions
  var _lineHeight = 18;  // px; recalculated on init
  var _svgEl = null;
  var _rulerActive = false;

  /* ── Character width measurement ─────────────────────────────── */
  function measureCharWidth() {
    var probe = document.createElement('span');
    probe.style.cssText = [
      "font-family:'Courier New',Courier,monospace",
      "font-size:13px",
      "line-height:1.4",
      "white-space:pre",
      "position:absolute",
      "visibility:hidden",
      "top:-9999px",
      "left:0"
    ].join(';');
    probe.textContent = new Array(201).join('M');
    document.body.appendChild(probe);
    var w = probe.getBoundingClientRect().width / 200;
    document.body.removeChild(probe);
    return w;
  }

  /* ── SVG overlay management ─────────────────────────────────── */
  function getOrCreateSvg(wrap) {
    var existing = wrap.querySelector('#fwf-ruler-svg');
    if (existing) return existing;

    var svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    svg.setAttribute('id', 'fwf-ruler-svg');
    svg.style.cssText = 'position:absolute;top:0;left:0;overflow:visible;pointer-events:none;';
    wrap.style.position = 'relative';
    wrap.appendChild(svg);
    return svg;
  }

  function syncSvgSize() {
    var wrap = document.getElementById('fwf-ruler-wrap');
    if (!wrap || !_svgEl) return;
    var editor = wrap.querySelector('.ace_editor');
    if (!editor) return;
    var r = editor.getBoundingClientRect();
    var wr = wrap.getBoundingClientRect();
    _svgEl.setAttribute('width',  r.width);
    _svgEl.setAttribute('height', r.height);
    _svgEl.style.left = (r.left - wr.left) + 'px';
    _svgEl.style.top  = (r.top  - wr.top)  + 'px';
  }

  /* ── Boundary line creation ─────────────────────────────────── */
  function charPosToX(charPos) {
    return charPos * _charWidth;
  }

  function createLine(charPos) {
    var svg = _svgEl;
    if (!svg) return;
    var x = charPosToX(charPos);
    var h = parseFloat(svg.getAttribute('height') || 600);

    var line = document.createElementNS('http://www.w3.org/2000/svg', 'line');
    line.setAttribute('class', 'fwf-boundary');
    line.setAttribute('x1', x); line.setAttribute('y1', 0);
    line.setAttribute('x2', x); line.setAttribute('y2', h);
    line.setAttribute('data-char', charPos);
    line.style.pointerEvents = 'all';
    line.style.cursor = 'ew-resize';
    svg.appendChild(line);

    // drag via interact.js
    if (window.interact) {
      window.interact(line).draggable({
        axis: 'x',
        modifiers: [
          window.interact.modifiers.snap({
            targets: [window.interact.snappers.grid({ x: _charWidth, y: 0 })],
            range: Infinity,
            relativePoints: [{ x: 0, y: 0 }]
          }),
          window.interact.modifiers.restrict({ restriction: svg.parentElement })
        ],
        listeners: {
          move: function(evt) {
            var curX = parseFloat(evt.target.getAttribute('x1')) + evt.dx;
            curX = Math.max(_charWidth, curX);
            var newChar = Math.round(curX / _charWidth);
            evt.target.setAttribute('x1', charPosToX(newChar));
            evt.target.setAttribute('x2', charPosToX(newChar));
            evt.target.setAttribute('data-char', newChar);
          },
          end: function() {
            sendBoundariesToShiny();
          }
        }
      });
    }

    // double-click to remove
    line.addEventListener('dblclick', function(e) {
      e.stopPropagation();
      svg.removeChild(line);
      sendBoundariesToShiny();
    });
  }

  function addBoundary(charPos) {
    if (charPos <= 0) return;
    // Avoid duplicates within 1 char
    var existing = getCharPositions();
    for (var i = 0; i < existing.length; i++) {
      if (Math.abs(existing[i] - charPos) < 1) return;
    }
    createLine(charPos);
    sendBoundariesToShiny();
  }

  function getCharPositions() {
    if (!_svgEl) return [];
    var lines = _svgEl.querySelectorAll('.fwf-boundary');
    var positions = [];
    lines.forEach(function(l) {
      positions.push(parseInt(l.getAttribute('data-char'), 10));
    });
    return positions.sort(function(a, b) { return a - b; });
  }

  function sendBoundariesToShiny() {
    var positions = getCharPositions();
    if (window.Shiny) {
      Shiny.setInputValue('fwf_boundary_positions', positions, { priority: 'event' });
    }
  }

  function clearBoundaries() {
    if (!_svgEl) return;
    var lines = _svgEl.querySelectorAll('.fwf-boundary');
    lines.forEach(function(l) { _svgEl.removeChild(l); });
  }

  /* ── Click-to-add ───────────────────────────────────────────── */
  function onEditorClick(e) {
    if (!_rulerActive || !_charWidth) return;
    var wrap = document.getElementById('fwf-ruler-wrap');
    if (!wrap) return;
    var scroller = wrap.querySelector('.ace_scroller');
    if (!scroller) return;
    var rect = scroller.getBoundingClientRect();
    var x = e.clientX - rect.left + scroller.scrollLeft;
    var charPos = Math.round(x / _charWidth);
    if (charPos > 0) addBoundary(charPos);
  }

  /* ── Ruler scroll sync ──────────────────────────────────────── */
  function setupScrollSync() {
    var wrap = document.getElementById('fwf-ruler-wrap');
    if (!wrap) return;
    var scroller = wrap.querySelector('.ace_scroller');
    var ruler    = document.getElementById('fwf-char-ruler');
    if (!scroller || !ruler) return;
    scroller.addEventListener('scroll', function() {
      ruler.scrollLeft = scroller.scrollLeft;
      // Also shift SVG lines
      if (_svgEl) {
        _svgEl.style.left = (-scroller.scrollLeft + (parseFloat(_svgEl.style.left) || 0)) + 'px';
        // Simpler: just re-anchor SVG to editor bounding rect
        syncSvgSize();
        // update each line x based on scroll
        _svgEl.querySelectorAll('.fwf-boundary').forEach(function(l) {
          var charPos = parseInt(l.getAttribute('data-char'), 10);
          var x = charPosToX(charPos) - scroller.scrollLeft;
          l.setAttribute('x1', x);
          l.setAttribute('x2', x);
        });
      }
    });
  }

  /* ── Shiny message handlers ─────────────────────────────────── */
  if (window.Shiny) {
    // Restore boundaries after re-render
    Shiny.addCustomMessageHandler('fwf_restore_boundaries', function(msg) {
      clearBoundaries();
      if (msg && msg.positions) {
        msg.positions.forEach(function(p) { createLine(p); });
      }
      sendBoundariesToShiny();
    });

    // Activate ruler (called when FWF format is selected)
    Shiny.addCustomMessageHandler('fwf_ruler_activate', function(msg) {
      _rulerActive = true;
      initRuler();
    });

    // Deactivate ruler
    Shiny.addCustomMessageHandler('fwf_ruler_deactivate', function(msg) {
      _rulerActive = false;
    });
  }

  /* ── Init ───────────────────────────────────────────────────── */
  function initRuler() {
    _charWidth = measureCharWidth();

    var wrap = document.getElementById('fwf-ruler-wrap');
    if (!wrap) return;

    _svgEl = getOrCreateSvg(wrap);
    syncSvgSize();

    // Click handler on ace_scroller
    var scroller = wrap.querySelector('.ace_scroller');
    if (scroller && !scroller._dqRulerClick) {
      scroller.addEventListener('click', onEditorClick);
      scroller._dqRulerClick = true;
    }

    setupScrollSync();

    // Resize observer to keep SVG sized correctly
    if (window.ResizeObserver) {
      new ResizeObserver(syncSvgSize).observe(wrap);
    }
  }

  // Re-init when Shiny reconnects
  if (window.Shiny) {
    Shiny.addCustomMessageHandler('fwf_reinit', function() {
      setTimeout(initRuler, 100);
    });
  }

  // Auto-init on document ready (in case ruler is already on page)
  document.addEventListener('DOMContentLoaded', function() {
    _charWidth = measureCharWidth();
  });

})();
