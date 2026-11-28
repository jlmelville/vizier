#include "hammer.h"

#include <algorithm>
#include <array>
#include <cmath>
#include <cstdlib>
#include <vector>

int main() {
  namespace hammer = vizier::hammer;
  const std::array<double, 6> x{1, 3, -2, -1, 3, 0}, y{1, 0, 1, -2, 0, -2};
  const std::array<int, 7> from{1, 2, 3, 4, 5, 1, 2}, to{4, 5, 4, 5, 6, 2, 6};
  const hammer::Layout layout{x.data(), y.data(), static_cast<int>(x.size())};
  const hammer::Edges edges{from.data(), to.data(), from.size()};
  const auto count = edges.size * hammer::samples;
  std::vector<double> sx(count), sy(count), px(count), py(count);
  hammer::route(layout, edges, {sx.data(), sy.data()}, 1, [] {});
  hammer::route(layout, edges, {px.data(), py.data()}, 3, [] {});
  if (sx != px || sy != py) {
    return EXIT_FAILURE;
  }
  for (std::size_t e = 0; e < edges.size; ++e) {
    const auto first = e * hammer::samples;
    const auto last = first + hammer::samples - 1;
    if (std::abs(sx[first] - x[from[e] - 1]) > 1e-10 ||
        std::abs(sy[first] - y[from[e] - 1]) > 1e-10 ||
        std::abs(sx[last] - x[to[e] - 1]) > 1e-10 ||
        std::abs(sy[last] - y[to[e] - 1]) > 1e-10) {
      return EXIT_FAILURE;
    }
  }
  return EXIT_SUCCESS;
}
