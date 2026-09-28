# Shared setup and helper functions live here (outside tests/).
# Source this from the notebook with: source(file.path(prefix, "helpers.R"))



.libPaths(c("/srv/rlibs", .libPaths()))

if (!requireNamespace("ottr", quietly = TRUE)) {install.packages("ottr")}
library(ottr)
library(testthat)
# Just run this cell!
# No need to worry about what this code means

IRdisplay::display_javascript(r"[
(() => {
  function pathLooksLikeTyping(e) {
    const path = e.composedPath ? e.composedPath() : [];
    for (const n of path) {
      if (!n) continue;
      if (n.tagName === 'INPUT' || n.tagName === 'TEXTAREA') return true;
      if (n.isContentEditable) return true;
      const role = n.getAttribute?.('role');
      if (role === 'textbox' || role === 'combobox' || role === 'searchbox') return true;
      const ariaMulti = n.getAttribute?.('aria-multiline');
      if (ariaMulti === 'true') return true;
      const cls = (n.className || "").toString().toLowerCase();
    }
    return false;
  }
  function handler(e) {
    if (e.key !== 'o' && e.key !== 'O') return;
    if (pathLooksLikeTyping(e)) {
      e.stopPropagation();
      if (e.stopImmediatePropagation) e.stopImmediatePropagation();
      if (e.nativeEvent?.stopImmediatePropagation) e.nativeEvent.stopImmediatePropagation();
      return;
    }
    e.preventDefault();
    e.stopPropagation();
    if (e.stopImmediatePropagation) e.stopImmediatePropagation();
    if (e.nativeEvent?.stopImmediatePropagation) e.nativeEvent.stopImmediatePropagation();
  }
  window.addEventListener('keydown', handler, true);
  window.addEventListener('keypress', handler, true);
  console.log("Installed: 'o' wont toggle output; 'o' should type inside any textbox-like UI (including JupyTutor).");
})();
]")