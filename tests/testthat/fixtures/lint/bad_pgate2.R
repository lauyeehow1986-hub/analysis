# synthetic fixture: a compile() that branches on a diagnostic test (never sourced)
compile <- function(spec) { pv <- 1; switch(if (d$p_value > .05) "a" else "b", a = 1) }
