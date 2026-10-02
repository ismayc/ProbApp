// Shiny output binding that draws the app's plot with plotly.js.
//
// The server sends the plot as plain JSON, { data, layout, config } (built in
// functions.R, see finish_prob_plot()), and this binding hands it to
// Plotly.react(). Talking to plotly.js directly means the app does not need the
// R `plotly` package, whose dependency tree was most of the download and
// startup time of the shinylive / webR build.
//
// Events sent back to the server, for an output with id "distribPlot":
//   input$distribPlot_click    = { x }      x of the clicked point
//   input$distribPlot_selected = { x: [] }  the brushed x-range (continuous
//                                           plots) or the x of each brushed bar
//
// ui.R inlines this file into the page <head>.
(function () {
  // The "basic" bundle has the bar and scatter traces, which is all the app
  // draws, at about a quarter of the size of the full bundle. jsDelivr serves it
  // with a one-year immutable cache lifetime.
  var PLOTLY_URL = "https://cdn.jsdelivr.net/npm/plotly.js-basic-dist-min@2.35.2/plotly-basic.min.js";

  // plotly.js loads in parallel with the rest of the page; plots that arrive
  // before it is ready wait in `queue`.
  var queue = [];
  function whenPlotly(fn) {
    if (window.Plotly) fn(); else queue.push(fn);
  }
  var script = document.createElement("script");
  script.src = PLOTLY_URL;
  script.async = true;
  script.onload = function () {
    var fns = queue;
    queue = [];
    fns.forEach(function (fn) { fn(); });
  };
  document.head.appendChild(script);

  // plotly.js keeps what it is given and adds to it (the selection outline, for
  // one), so every draw gets its own copy of the last plot from the server.
  function draw(el) {
    var spec = JSON.parse(el.probPlotSpec);
    return window.Plotly.react(el, spec.data, spec.layout, spec.config);
  }

  function clear(el) {
    el.probPlotSpec = null;
    if (el.probPlotBound) {
      window.Plotly.purge(el);
      el.probPlotBound = false;
    }
  }

  function bindEvents(el) {
    el.on("plotly_click", function (ev) {
      var pt = ev && ev.points && ev.points[0];
      if (!pt) return;
      Shiny.setInputValue(el.id + "_click", { x: pt.x }, { priority: "event" });
    });

    el.on("plotly_selected", function (ev) {
      if (!ev) return;   // a plain click with the brush tool ends an empty selection
      var categorical = el._fullLayout.xaxis.type === "category";
      var xs = !categorical && ev.range && ev.range.x
        ? [ev.range.x[0], ev.range.x[1]]
        : (ev.points || []).map(function (pt) { return pt.x; });
      Shiny.setInputValue(el.id + "_selected", { x: xs }, { priority: "event" });
      // Drop the brush outline and the dimming of the points outside it: the
      // brush is a way to enter two numbers, and the server redraws the shaded
      // region from them. Deferred so plotly.js finishes its own selection
      // handling first.
      setTimeout(function () { if (el.probPlotSpec) draw(el); }, 0);
    });
  }

  // Follow the container when it changes size without a window resize
  // (collapsing the sidebar, showing a hidden card).
  function observeSize(el) {
    if (el.probPlotObserved || !window.ResizeObserver) return;
    el.probPlotObserved = true;
    new ResizeObserver(function () {
      if (el.probPlotBound && el.offsetWidth > 0) window.Plotly.Plots.resize(el);
    }).observe(el);
  }

  var binding = new Shiny.OutputBinding();
  $.extend(binding, {
    find: function (scope) {
      return $(scope).find(".prob-plot");
    },
    renderValue: function (el, spec) {
      if (!spec) { clear(el); return; }
      el.probPlotSpec = JSON.stringify(spec);
      whenPlotly(function () {
        if (!el.probPlotSpec) return;   // cleared while plotly.js was loading
        var first = !el.probPlotBound;
        el.probPlotBound = true;
        draw(el).then(function () {
          if (first) bindEvents(el);
          observeSize(el);
        });
      });
    },
    // An invalid input blanks the plot (the server stops with a silent error).
    renderError: function (el, err) {
      clear(el);
      if (err.message) $(el).addClass("shiny-output-error").text(err.message);
    },
    clearError: function (el) {
      if ($(el).hasClass("shiny-output-error")) $(el).removeClass("shiny-output-error").empty();
    }
  });
  Shiny.outputBindings.register(binding, "probapp.probPlot");
})();
