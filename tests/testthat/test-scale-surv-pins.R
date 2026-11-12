# P1e.2 Task 1: the memory-engine code of the survival routes is frozen. The sha256 of the compiled code of
# each spec below was recorded from the code before P1e.2 and must never change (an old project must keep its
# SAP hash and its compiled code). Synthetic spec values only; no data is read.

.sp_layers <- function(...) {
  x <- list(missing = "cc", weights = "none", matching = FALSE, resampling = "none",
            framework = "frequentist", clustered = FALSE)
  m <- list(...)
  for (n in names(m)) x[[n]] <- m[[n]]
  x
}

.sp_cox <- function(purpose = "explain-causal", ...) {
  s <- list(purpose = purpose, time = "time", status = "status", status_event = 2, exposure = "sex",
            layers = .sp_layers(), seed = 7L)
  m <- list(...)
  for (n in names(m)) s[n] <- list(m[[n]])
  s
}

.sp_rmst <- function(...) {
  s <- list(purpose = "explain-causal", time = "time", status = "status", status_event = 2, exposure = "sex",
            tau = 365, layers = .sp_layers(), seed = 3L)
  m <- list(...)
  for (n in names(m)) s[n] <- list(m[[n]])
  s
}

.sp_hash <- function(id, spec) {
  mk <- list(cox = .sr_cox_route, cox_describe = .sr_cox_describe_route, rmst = .sr_rmst_route)
  if (!(.sr_route_key(id, 1L) %in% ls(.sr_routes, all.names = TRUE))) sr_route_register(mk[[id]]())
  .sr_sha256(charToRaw(sr_compile(sr_route_get(id, 1L), spec)))
}

test_that("memory code of survival specs is unchanged", {
  pins <- list(
    cox_model = list("cox", .sp_cox(terms = list(list(var = "age"))),
                     "935766086bcd33cc07785b707974f4be65881318cba0a02998b006a6d9849b3f"),
    cox_cluster = list("cox", .sp_cox(cluster = "inst", layers = .sp_layers(clustered = TRUE)),
                       "b7da4ed1aa071da8b43e2b0fb7f4053d80c20228b292f7ab3a09adfa72eba896"),
    cox_strata2 = list("cox", .sp_cox(strata = c("ph.ecog", "inst")),
                       "41b5458a41469ffeb3f173c0e5c98ea7163d357e165719955c157a548944f153"),
    cox_breslow = list("cox", .sp_cox(ties = "breslow"),
                       "0859fdc57279a5c46ce6d910b378b73246d9f54cb00ed7f26eb88b7d71b7b076"),
    cox_interaction = list("cox", .sp_cox(terms = list(list(var = "age")), interactions = list(c("sex", "age"))),
                           "c3f6f5991c532d8cb33f4cb4563f33ba8d8eb9cdf2b8626f3feb18633d0c822a"),
    cox_rcs = list("cox", .sp_cox(terms = list(list(var = "age", type = "rcs", knots = 3))),
                   "ff01c981a3e98b2f30aa38bc7bd4df9d7734b4191c24e3d89ea5e21e8d83e97c"),
    cox_mi = list("cox", .sp_cox(layers = .sp_layers(missing = "mi"), mi = list(m = 3, maxit = 2)),
                  "932965c4e8af24f2310657df97dce37d6b9a6676fb18e54616b61f8338247da9"),
    cox_boot = list("cox", .sp_cox(layers = .sp_layers(resampling = "bootstrap"), boot = list(B = 20)),
                    "13e3cdb937b6e52a8848b56e0c77fcd9c8aa39dda19004ad1e12f6b92e3ee1e0"),
    cox_describe = list("cox_describe", .sp_cox(purpose = "describe"),
                        "275cfc514b100b95027613b655450f2d736ea5e5a9f47609d435dd6fe3ff3cc4"),
    rmst_plain = list("rmst", .sp_rmst(),
                      "6b8ec1ba7a4e00859bc7d83782fd75f4ad7312428a089548f9a975be91e7e81e"),
    rmst_mi = list("rmst", .sp_rmst(layers = .sp_layers(missing = "mi"), mi = list(m = 3, maxit = 2)),
                   "42a4a37c32dfdb780897bc2da1fcd1f1b5da9a3d80b9914f750d802ed929f74a"))
  for (nm in names(pins)) {
    p <- pins[[nm]]
    expect_identical(.sp_hash(p[[1]], p[[2]]), p[[3]], info = nm)
  }
  # An explicit cox_method = "exact" on the memory engine compiles to the same code.
  expect_identical(.sp_hash("cox", .sp_cox(terms = list(list(var = "age")), cox_method = "exact")),
                   pins$cox_model[[3]])
})
