# Tests for router/graph.R: node contract, DAG registry, instances and the input hash.

test_that("a node may depend only on registered nodes", {
  .fx_toy_nodes()
  expect_error(sr_node_register(sr_node("x", "X", c("p", "q"), depends = "nope")), "nope")
  expect_false("x" %in% sr_nodes())
  b2 <- sr_node("toy_b", "Toy B", c("b1", "b2"), depends = character())
  expect_error(sr_node_register(b2), "depends")
  same <- sr_node("toy_b", "Toy B v2", c("b1", "b2"), depends = "toy_a")
  expect_silent(sr_node_register(same))
  expect_identical(sr_node_get("toy_b")$label, "Toy B v2")
  expect_identical(sum(sr_nodes() == "toy_b"), 1L)
})

test_that("sr_node validates its arguments", {
  expect_error(sr_node("Bad-Id", "x", c("a", "b")), "id")
  expect_error(sr_node("cov", "x", c("a", "b"), scope = "column"), "columns")
  expect_s3_class(sr_node("cov", "x", c("a", "b"), scope = "column", columns = function(ctx) "z"), "sr_node")
})

test_that("sr_nodes is a topological order", {
  .fx_toy_nodes()
  ids <- sr_nodes()
  expect_lt(match("toy_a", ids), match("toy_b", ids))
  expect_lt(match("toy_b", ids), match("toy_c", ids))
  expect_error(sr_node_get("nope"), "nope")
})

test_that("keys", {
  expect_identical(sr_router_key("toy_a"), "toy_a")
  expect_identical(sr_router_key("toy_c", "t"), "toy_c@t")
  expect_identical(sr_router_key("cov", "t", "age group"), "cov@t:age group")
})

test_that("instances follow the tables with confirmed roles", {
  .fx_toy_nodes()
  cov <- sr_node("toy_cov", "Cov", c("k1", "k2"), scope = "column", depends = "toy_c",
                 columns = function(ctx) c("x", "y"))
  sr_node_register(cov)
  withr::defer(.sr_node_unregister("toy_cov"))
  p <- .fx_project()
  p$state$roles <- list(t2 = c(a = "outcome"), t1 = c(b = "exposure"))
  inst <- .sr_router_instances(p)
  expect_identical(names(inst), c("key", "node", "table", "column"))
  expect_identical(inst$key[inst$node == "toy_c"], c("toy_c@t1", "toy_c@t2"))
  expect_identical(inst$key[inst$node == "toy_a"], "toy_a")
  expect_identical(sum(inst$node == "toy_cov"), 4L)
  expect_identical(inst$key[inst$node == "toy_cov"],
                   c("toy_cov@t1:x", "toy_cov@t1:y", "toy_cov@t2:x", "toy_cov@t2:y"))
  # topological: every node's rows come in sr_nodes() order
  expect_identical(unique(inst$node), sr_nodes()[sr_nodes() %in% inst$node])
  p0 <- .fx_project()
  expect_identical(nrow(.sr_router_instances(p0)), sum(sr_nodes() %in% c("toy_a", "toy_b")))
})

test_that("the context resolves dependencies at the widest scope that fits", {
  .fx_toy_nodes()
  p <- .fx_project()
  p$state$roles <- list(t = c(a = "outcome"))
  ctx <- .sr_router_ctx(p, sr_node_get("toy_c"), "t", NA)
  expect_identical(names(ctx), c("project", "node", "table", "column", "pico", "dep"))
  expect_identical(ctx$dep, list(toy_b = NA_character_))
  expect_null(ctx$pico)
  expect_identical(.sr_router_current(p, "toy_a"), NA_character_)
  p <- sr_router_run(sr_router_decide(p, "toy_a", "a2"))
  expect_identical(.sr_router_ctx(p, sr_node_get("toy_c"), "t", NA)$dep$toy_b, "b2")
  expect_identical(.sr_router_current(p, "toy_b"), "b2")
  expect_identical(.sr_router_current(p, "toy_a"), "a2")
  expect_identical(.sr_router_current(p, "toy_c@t"), NA_character_)
})

test_that("the input hash changes with facts and dependency choices only", {
  .fx_toy_nodes()
  f <- sr_node("toy_f", "F", c("f1", "f2"), depends = "toy_a",
               facts = function(ctx) list(pop = ctx$pico$population))
  sr_node_register(f)
  withr::defer(.sr_node_unregister("toy_f"))
  p <- .fx_project()
  p <- sr_pico_set(p, population = "adults", outcome = "death")
  ctx <- .sr_router_ctx(p, sr_node_get("toy_f"), NA, NA)
  h <- sr_router_input_hash(ctx)
  expect_match(h, "^[0-9a-f]{64}$")
  expect_identical(sr_router_input_hash(ctx), h)
  ctx2 <- ctx
  ctx2$dep$toy_a <- "a1"
  expect_false(identical(sr_router_input_hash(ctx2), h))
  p2 <- sr_pico_set(p, population = "adults", outcome = "readmission")
  expect_identical(sr_router_input_hash(.sr_router_ctx(p2, sr_node_get("toy_f"), NA, NA)), h)
  p3 <- sr_pico_set(p, population = "children", outcome = "death")
  expect_false(identical(sr_router_input_hash(.sr_router_ctx(p3, sr_node_get("toy_f"), NA, NA)), h))
  sr_node_register(sr_node("toy_f", "F", c("f1", "f2"), depends = "toy_a", version = 2L,
                           facts = function(ctx) list(pop = ctx$pico$population)))
  expect_false(identical(sr_router_input_hash(.sr_router_ctx(p, sr_node_get("toy_f"), NA, NA)), h))
})
