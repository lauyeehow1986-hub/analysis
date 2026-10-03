# synthetic fixture: a clean compile() (never sourced)
route <- list(
  compile = function(spec) {
    if (spec$sap$rule == "x") "a" else "b"
    if (isTRUE(spec$design$clustered)) "mixed" else "plain"
  },
  present = function(res) {
    shapiro.test(res$x)
  }
)

# diagnostics outside compile() are allowed: present() may report them
diagnose <- function(x) {
  if (shapiro.test(x)$p.value < .05) "non-normal" else "normal"
}
