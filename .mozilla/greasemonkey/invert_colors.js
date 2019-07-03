// ==UserScript==
// @name     Invert Colors
// @version  1
// ==/UserScript==

(function() {
  document.addEventListener('keydown', function(e) {
    // pressed ctrl+i
    if (e.keyCode == 73 && !e.shiftKey && e.ctrlKey && !e.altKey && !e.metaKey) {
      document.documentElement.classList.toggle("invert-colors");
      e.preventDefault();
    }
  }, false);
})();
