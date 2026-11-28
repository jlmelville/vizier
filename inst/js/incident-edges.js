function(el, widget, graph) {
  function showIncidentEdges(event) {
    const point = event.points[0];
    if (!point || !Number.isInteger(point.customdata)) return;
    const row = point.customdata - 1;
    if (row < 0 || row >= graph.x.length) return;

    const x = [];
    const y = [];
    for (let i = graph.offsets[row]; i < graph.offsets[row + 1]; i++) {
      const neighbor = graph.neighbors[i];
      x.push(graph.x[row], graph.x[neighbor], null);
      y.push(graph.y[row], graph.y[neighbor], null);
    }
    Plotly.restyle(el, {x: [x], y: [y]}, [0]);
  }

  if (el.vizierIncidentClick) {
    el.removeListener("plotly_click", el.vizierIncidentClick);
  }
  el.vizierIncidentClick = showIncidentEdges;
  el.on("plotly_click", showIncidentEdges);
}
