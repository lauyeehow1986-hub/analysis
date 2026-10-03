# synthetic fixture: a compile() that branches on a diagnostic test (never sourced)
route <- list(compile = function(spec) { if (shapiro.test(x)$p.value < .05) "wilcox" else "t" })
