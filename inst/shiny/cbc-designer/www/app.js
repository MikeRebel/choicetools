// Client side of the CBC design studio. It does three small things:
//  1. event bus: elements with data-evt report clicks and changes to the server as one
//     input ("ui_event"); the server decides what they mean (see R/events.R),
//  2. instant, purely visual switches that need no server round trip (page, settings tab),
//  3. a few messages from the server (disable controls, page, study name, scroll).
(function () {
  'use strict';
  var FORM = /^(INPUT|SELECT|TEXTAREA)$/;
  var RESERVED = ['evt', 'confirm', 'source', 'field', 'click', 'file', 'tab', 'tabGo'];

  function send(payload) {
    if (!window.Shiny || !window.Shiny.setInputValue) return;
    var msg = { nonce: Math.random() };
    for (var k in payload) msg[k] = payload[k];
    window.Shiny.setInputValue('ui_event', msg, { priority: 'event' });
  }

  function payloadFrom(el) {
    var p = {};
    Object.keys(el.dataset).forEach(function (k) {
      if (RESERVED.indexOf(k) < 0) p[k] = el.dataset[k];
    });
    p.type = el.dataset.evt;
    return p;
  }

  // Shiny only re-checks which outputs are visible when it hears a "shown" event.
  // We switch pages and tabs with CSS, so announce it, or hidden outputs stay asleep.
  function announce(el) {
    if (el && window.jQuery) window.jQuery(el).trigger('shown');
  }

  function goPage(page) {
    var app = document.querySelector('.app');
    if (app) app.setAttribute('data-page', page);
    document.querySelectorAll('.rail-item').forEach(function (b) {
      var on = b.getAttribute('data-page') === page;
      b.classList.toggle('active', on);
      if (on) b.setAttribute('aria-current', 'page'); else b.removeAttribute('aria-current');
    });
    var pages = document.querySelector('.pages');
    if (pages) pages.scrollTop = 0;
    announce(document.getElementById('page-' + page));
  }

  function showTab(name, box) {
    box = box || document.querySelector('.tabbed');
    if (!box) return;
    box.querySelectorAll('[data-tab]').forEach(function (b) {
      var on = b.getAttribute('data-tab') === name;
      b.classList.toggle('on', on);
      b.setAttribute('aria-selected', on ? 'true' : 'false');
    });
    box.querySelectorAll('[data-panel]').forEach(function (p) {
      p.hidden = p.getAttribute('data-panel') !== name;
    });
    announce(box.querySelector('[data-panel="' + name + '"]'));
  }

  document.addEventListener('click', function (e) {
    var t = e.target;
    if (!t || !t.closest) return;
    var tab = t.closest('[data-tab]');
    if (tab) { showTab(tab.getAttribute('data-tab'), tab.closest('.tabbed')); return; }
    var click = t.closest('[data-click]');
    if (click) { var b = document.getElementById(click.getAttribute('data-click')); if (b) b.click(); return; }
    var file = t.closest('[data-file]');
    if (file) { var f = document.getElementById(file.getAttribute('data-file')); if (f) f.click(); return; }
    var el = t.closest('[data-evt]');
    if (!el || FORM.test(el.tagName) || el.disabled) return;
    if (el.dataset.confirm && !window.confirm(el.dataset.confirm)) return;
    var p = payloadFrom(el);
    if (el.dataset.source === 'sibling') {
      var ta = el.parentElement.querySelector('textarea');
      p.value = ta ? ta.value : '';
    }
    if (p.type === 'goto') {
      goPage(p.page);
      if (el.dataset.tabGo) showTab(el.dataset.tabGo);
    }
    send(p);
  });

  document.addEventListener('change', function (e) {
    var el = e.target && e.target.closest ? e.target.closest('[data-evt]') : null;
    if (!el || !FORM.test(el.tagName)) return;
    var p = payloadFrom(el);
    p.value = el.type === 'checkbox' ? el.checked : el.value;
    if (el.dataset.field) p[el.dataset.field] = p.value;
    send(p);
  });

  // Enter commits a text or number field (the change event fires on blur).
  document.addEventListener('keydown', function (e) {
    if (e.key === 'Enter' && e.target && e.target.matches && e.target.matches('input[data-evt]')) e.target.blur();
  });

  function register() {
    var S = window.Shiny;
    S.addCustomMessageHandler('page', goPage);
    S.addCustomMessageHandler('studyName', function (v) {
      var el = document.querySelector('.study-name');
      if (el) el.value = v;
    });
    S.addCustomMessageHandler('disable', function (m) {
      [].concat(m.ids).forEach(function (id, i) {
        var el = document.getElementById(id);
        if (!el) return;
        var off = !![].concat(m.disabled)[i];
        var controls = el.classList.contains('shiny-input-radiogroup') ? el.querySelectorAll('input') : [el];
        [].forEach.call(controls, function (c) { c.disabled = off; });
        var wrap = el.closest('.fld, .check, .opts');
        if (wrap) wrap.classList.toggle('is-off', off);
      });
    });
    S.addCustomMessageHandler('reveal', function (sel) {
      var el = document.querySelector(sel);
      if (el && el.scrollIntoView) el.scrollIntoView({ behavior: 'smooth', block: 'start' });
    });
  }

  var wait = setInterval(function () {
    if (window.Shiny && window.Shiny.addCustomMessageHandler) { clearInterval(wait); register(); }
  }, 20);
})();
