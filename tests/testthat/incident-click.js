// Exercise the installed widget callback without a browser or an R server.
const assert = require("assert");
const fs = require("fs");
const vm = require("vm");
const EventEmitter = require("events");

const attach = vm.runInThisContext("(" + fs.readFileSync(process.argv[2], "utf8") + ")");
const el = new EventEmitter();
const updates = [];
global.Plotly = {
  restyle(target, update, traces) {
    assert.strictEqual(target, el);
    assert.deepStrictEqual(traces, [0]);
    updates.push(update);
  }
};
const graph = {
  x: [1, 2, 3, 4], y: [5, 6, 7, 8],
  offsets: [0, 1, 3, 4, 4], neighbors: [1, 0, 2, 1]
};
attach(el, {}, graph);
attach(el, {}, graph);
assert.strictEqual(el.listenerCount("plotly_click"), 1);
for (const row of [2, 1, 4]) {
  el.emit("plotly_click", {points: [{customdata: row, pointNumber: 0}]});
}
assert.deepStrictEqual(updates, [
  {x: [[2, 1, null, 2, 3, null]], y: [[6, 5, null, 6, 7, null]]},
  {x: [[1, 2, null]], y: [[5, 6, null]]},
  {x: [[]], y: [[]]}
]);
