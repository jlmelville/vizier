#include <Rcpp.h>

#include "hammer.h"

// [[Rcpp::export(rng = false)]]
Rcpp::DataFrame hammer_paths(const Rcpp::NumericMatrix &coords,
                             const Rcpp::IntegerVector &from,
                             const Rcpp::IntegerVector &to,
                             const Rcpp::IntegerVector &edge_id,
                             int n_threads) {
  namespace hammer = vizier::hammer;
  const auto edges = from.size();
  if (coords.ncol() != 2 || edges != to.size() || edges != edge_id.size() ||
      n_threads < 0 || edges > R_XLEN_T_MAX / hammer::samples) {
    Rcpp::stop("Invalid Hammer routing input");
  }
  const auto count = edges * hammer::samples;
  Rcpp::NumericVector x(Rcpp::no_init(count)), y(Rcpp::no_init(count));
  hammer::route({coords.begin(), coords.begin() + coords.nrow(), coords.nrow()},
                {from.begin(), to.begin(), static_cast<std::size_t>(edges)},
                {x.begin(), y.begin()}, n_threads, Rcpp::checkUserInterrupt);
  Rcpp::IntegerVector ids(Rcpp::no_init(count));
  for (R_xlen_t e = 0; e < edges; ++e) {
    std::fill_n(ids.begin() + e * hammer::samples, hammer::samples, edge_id[e]);
  }
  return Rcpp::DataFrame::create(Rcpp::_["x"] = x, Rcpp::_["y"] = y,
                                 Rcpp::_["edge_id"] = ids);
}
