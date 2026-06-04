# Loaded before ui.R/server.R, so objects defined here are visible to both.
#
# is_webr is TRUE in the shinylive / GitHub Pages build (R is compiled to
# WebAssembly there). In that build the app runs inside an iframe, so URL
# bookmarking writes to the iframe's URL rather than the browser address bar and
# "Share link" can't produce a copyable URL — meaningless. So we enable
# bookmarking (and render the Share-link button, see ui.R) only off-webR.
is_webr <- identical(R.version[["arch"]], "wasm32")

# Enable URL bookmarking so a configuration can be captured in a shareable link
# (e.g. instructors posting a specific example) — server build only.
if (!is_webr) enableBookmarking(store = "url")
