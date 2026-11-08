# Test helpers for the router: toy nodes registered for one test and removed again.

#' Register toy_a (project), toy_b (depends on toy_a, rule), toy_c (table, depends on toy_b).
.fx_toy_nodes <- function(env = parent.frame()) {
  a <- sr_node("toy_a", "Toy A", c("a1", "a2"),
               question = function(ctx) list(instructions = "Pick the better of the two toy options.",
                                             criteria = list(a1 = "first", a2 = "second")))
  b <- sr_node("toy_b", "Toy B", c("b1", "b2"), depends = "toy_a",
               rule = function(ctx) {
                 a <- ctx$dep$toy_a
                 if (is.null(a) || is.na(a)) return(NULL)
                 list(choice = paste0("b_", substring(a, 2L)) |> sub(pattern = "^b_", replacement = "b"),
                      reason = "copied from toy_a")
               })
  cc <- sr_node("toy_c", "Toy C", c("c1", "c2"), scope = "table", depends = "toy_b")
  for (n in list(a, b, cc)) sr_node_register(n)
  withr::defer(for (id in c("toy_c", "toy_b", "toy_a")) .sr_node_unregister(id), envir = env)
  invisible(c("toy_a", "toy_b", "toy_c"))
}
