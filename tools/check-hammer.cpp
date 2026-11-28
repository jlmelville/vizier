#include "hammer.h"

#include <algorithm>
#include <array>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <vector>

namespace {
bool check_gaussian_blur() {
  namespace hammer = vizier::hammer;
  constexpr int side = hammer::grid_size;
  std::vector<double> input(hammer::cells);
  for (int i = 0; i < hammer::cells; ++i) {
    input[i] = (i % 17 == 0) ? (i % 199) + 0.25 : 0.0;
  }

  // A scalar two-pass reference also exercises the replicated borders.
  constexpr int radius = 3;
  std::array<double, 2 * radius + 1> weights{};
  double sum = 0;
  for (int k = -radius; k <= radius; ++k) {
    weights[k + radius] = std::exp(-0.5 * k * k);
    sum += weights[k + radius];
  }
  for (double &weight : weights) {
    weight /= sum;
  }
  auto convolve = [&](const std::vector<double> &source, bool horizontal) {
    std::vector<double> result(hammer::cells);
    for (int y = 0; y < side; ++y) {
      for (int x = 0; x < side; ++x) {
        double value = 0;
        for (int k = -radius; k <= radius; ++k) {
          const int xx = horizontal ? std::clamp(x + k, 0, side - 1) : x;
          const int yy = horizontal ? y : std::clamp(y + k, 0, side - 1);
          value += source[yy * side + xx] * weights[k + radius];
        }
        result[y * side + x] = value;
      }
    }
    return result;
  };
  const auto expected = convolve(convolve(input, true), false);
  for (const auto threads : {1, 3}) {
    auto actual = input;
    hammer::gaussian_blur(actual, 0.49, threads);
    if (actual != input) {
      return false;
    }
    hammer::gaussian_blur(actual, 1.0, threads);
    if (std::memcmp(actual.data(), expected.data(),
                    expected.size() * sizeof(double)) != 0) {
      return false;
    }
  }
  return true;
}

bool check_density_gradient() {
  namespace hammer = vizier::hammer;
  std::vector<double> density(hammer::cells);
  for (int i = 0; i < hammer::cells; ++i) {
    density[i] = (i % 37) * 1.25 + (i % 997) / 41.0;
  }
  std::vector<double> probes{-1.25, 0.125, 1.25, 27.875, 254.75, 256.25};
  for (int i = 0; i < hammer::grid_size; ++i) {
    const double v = i;
    probes.insert(probes.end(),
                  {std::nextafter(v, v - 1), v, std::nextafter(v, v + 1)});
  }
  for (double x : probes) {
    for (double y : probes) {
      const auto [gx, gy] = hammer::density_gradient(density.data(), x, y);
      if (gx != hammer::sample_density(density.data(), x + 1, y) -
                    hammer::sample_density(density.data(), x - 1, y) ||
          gy != hammer::sample_density(density.data(), x, y + 1) -
                    hammer::sample_density(density.data(), x, y - 1)) {
        return false;
      }
    }
  }
  return true;
}
} // namespace

int main() {
  if (!check_gaussian_blur() || !check_density_gradient()) {
    return EXIT_FAILURE;
  }
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
