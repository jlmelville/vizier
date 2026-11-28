// Kernel-density edge bundling, adapted from edgebundle 1.0.1 (MIT).
// Copyright (c) 2020 edgebundle authors. See inst/COPYRIGHTS.
#ifndef VIZIER_HAMMER_H
#define VIZIER_HAMMER_H

#include <algorithm>
#include <array>
#include <cmath>
#include <cstddef>
#include <limits>
#include <stdexcept>
#include <thread>
#include <vector>

#include "pforr.h"

namespace vizier::hammer {
constexpr int samples = 50;
constexpr int grid_size = 256;
constexpr int cells = grid_size * grid_size;
constexpr double padding = 0.05;

struct Layout {
  const double *x;
  const double *y;
  int rows;
};

struct Edges {
  const int *from;
  const int *to;
  std::size_t size;
};

struct PathColumns {
  double *x;
  double *y;
};

struct Extent {
  double xmin;
  double ymin;
  double span;
};

inline Extent endpoint_extent(Layout layout, Edges edges) {
  double xmin = std::numeric_limits<double>::infinity();
  double ymin = xmin;
  double xmax = -xmin;
  double ymax = -ymin;
  for (std::size_t e = 0; e < edges.size; ++e) {
    for (const int row : {edges.from[e], edges.to[e]}) {
      if (row < 1 || row > layout.rows) {
        throw std::invalid_argument("Hammer endpoint outside coordinate rows");
      }
      const auto x = layout.x[row - 1];
      const auto y = layout.y[row - 1];
      if (!std::isfinite(x) || !std::isfinite(y)) {
        throw std::invalid_argument("Hammer endpoints must be finite");
      }
      xmin = std::min(xmin, x);
      xmax = std::max(xmax, x);
      ymin = std::min(ymin, y);
      ymax = std::max(ymax, y);
    }
  }
  const double span = std::max(xmax - xmin, ymax - ymin);
  if (!std::isfinite(span)) {
    throw std::invalid_argument("Hammer coordinate span exceeds numeric range");
  }
  return {xmin, ymin, span == 0 ? 1.0 : span};
}

inline void gaussian_blur(std::vector<double> &density, double sigma) {
  if (sigma < 0.5) {
    return;
  }
  const auto radius = static_cast<int>(std::ceil(3 * sigma));
  std::vector<double> kernel(2 * radius + 1);
  double sum = 0;
  for (int i = -radius; i <= radius; ++i) {
    kernel[i + radius] = std::exp(-(i * i) / (2 * sigma * sigma));
    sum += kernel[i + radius];
  }
  for (auto &k : kernel) {
    k /= sum;
  }
  std::vector<double> horizontal(cells);
  for (int y = 0; y < grid_size; ++y) {
    for (int x = 0; x < grid_size; ++x) {
      double value = 0;
      for (int i = -radius; i <= radius; ++i) {
        const int col = std::clamp(x + i, 0, grid_size - 1);
        value += density[y * grid_size + col] * kernel[i + radius];
      }
      horizontal[y * grid_size + x] = value;
    }
  }
  for (int y = 0; y < grid_size; ++y) {
    for (int x = 0; x < grid_size; ++x) {
      double value = 0;
      for (int i = -radius; i <= radius; ++i) {
        const int row = std::clamp(y + i, 0, grid_size - 1);
        value += horizontal[row * grid_size + x] * kernel[i + radius];
      }
      density[y * grid_size + x] = value;
    }
  }
}

inline double sample_density(const double *density, double x, double y) {
  const auto col = static_cast<int>(std::floor(x));
  const auto row = static_cast<int>(std::floor(y));
  const double fx = x - col;
  const double fy = y - row;
  const auto left = std::clamp(col, 0, grid_size - 1);
  const auto right = std::clamp(col + 1, 0, grid_size - 1);
  const auto top = std::clamp(row, 0, grid_size - 1) * grid_size;
  const auto bottom = std::clamp(row + 1, 0, grid_size - 1) * grid_size;
  return density[top + left] * (1 - fx) * (1 - fy) +
         density[top + right] * fx * (1 - fy) +
         density[bottom + left] * (1 - fx) * fy +
         density[bottom + right] * fx * fy;
}

inline void update_path(double *x, double *y, const double *density,
                        double bandwidth) {
  for (int i = 1; i < samples - 1; ++i) {
    const double cx = x[i] * (grid_size - 1);
    const double cy = y[i] * (grid_size - 1);
    const double gx = sample_density(density, cx + 1, cy) -
                      sample_density(density, cx - 1, cy);
    const double gy = sample_density(density, cx, cy + 1) -
                      sample_density(density, cx, cy - 1);
    const double norm = std::sqrt(gx * gx + gy * gy);
    if (norm > 1e-12) {
      x[i] += 0.6 * bandwidth * gx / norm;
      y[i] += 0.6 * bandwidth * gy / norm;
    }
  }

  std::array<double, samples> sx, sy, length;
  std::copy_n(x, samples, sx.begin());
  std::copy_n(y, samples, sy.begin());
  for (int i = 1; i < samples - 1; ++i) {
    x[i] = 0.5 * sx[i] + 0.25 * (sx[i - 1] + sx[i + 1]);
    y[i] = 0.5 * sy[i] + 0.25 * (sy[i - 1] + sy[i + 1]);
  }
  length[0] = 0;
  for (int i = 1; i < samples; ++i) {
    const double dx = x[i] - x[i - 1];
    const double dy = y[i] - y[i - 1];
    length[i] = length[i - 1] + std::sqrt(dx * dx + dy * dy);
  }
  int segment = 0;
  for (int i = 0; i < samples; ++i) {
    const double target = length.back() * i / (samples - 1);
    while (segment < samples - 2 && length[segment + 1] < target) {
      ++segment;
    }
    const double dist = length[segment + 1] - length[segment];
    const double t = dist > 0 ? (target - length[segment]) / dist : 0;
    sx[i] = x[segment] + t * (x[segment + 1] - x[segment]);
    sy[i] = y[segment] + t * (y[segment + 1] - y[segment]);
  }
  std::copy_n(sx.begin(), samples, x);
  std::copy_n(sy.begin(), samples, y);
}

inline void route(Layout layout, Edges edges, PathColumns paths,
                  std::size_t requested_threads, void (*check_interrupt)()) {
  if (edges.size == 0) {
    return;
  }
  const auto extent = endpoint_extent(layout, edges);
  const auto hardware = std::max(1U, std::thread::hardware_concurrency());
  const auto threads = std::clamp<std::size_t>(
      requested_threads, 1, std::min<std::size_t>(hardware, edges.size));
  auto initialize = [&](std::size_t begin, std::size_t end) {
    auto normalize = [&](double v, double origin) {
      return (v - origin) / extent.span * (1 - 2 * padding) + padding;
    };
    for (auto e = begin; e < end; ++e) {
      const int from = edges.from[e] - 1;
      const int to = edges.to[e] - 1;
      const double x0 = normalize(layout.x[from], extent.xmin);
      const double y0 = normalize(layout.y[from], extent.ymin);
      const double x1 = normalize(layout.x[to], extent.xmin);
      const double y1 = normalize(layout.y[to], extent.ymin);
      for (int i = 0; i < samples; ++i) {
        const double t = static_cast<double>(i) / (samples - 1);
        paths.x[e * samples + i] = x0 + t * (x1 - x0);
        paths.y[e * samples + i] = y0 + t * (y1 - y0);
      }
    }
  };
  pforr::parallel_for(edges.size, initialize, threads);

  const auto count = edges.size * samples;
  std::vector<double> grids(threads * cells);
  std::vector<double> density(cells);
  for (int iteration = 0; iteration < 12; ++iteration) {
    check_interrupt();
    const double bandwidth = 0.05 * std::pow(0.7, iteration);
    std::fill(grids.begin(), grids.end(), 0.0);
    // Each worker owns its histogram. Counts are exact integers in doubles.
    auto deposit = [&](std::size_t begin, std::size_t end, std::size_t chunk) {
      auto *hist = grids.data() + chunk * cells;
      auto cell = [](double value) {
        return std::clamp(static_cast<int>(std::floor(value * (grid_size - 1))),
                          0, grid_size - 1);
      };
      for (auto j = begin; j < end; ++j) {
        ++hist[cell(paths.y[j]) * grid_size + cell(paths.x[j])];
      }
    };
    pforr::parallel_for_indexed(count, deposit, threads);
    std::copy_n(grids.begin(), cells, density.begin());
    for (std::size_t worker = 1; worker < threads; ++worker) {
      for (int i = 0; i < cells; ++i) {
        density[i] += grids[worker * cells + i];
      }
    }
    gaussian_blur(density, bandwidth * grid_size);
    auto update = [&](std::size_t begin, std::size_t end) {
      for (auto e = begin; e < end; ++e) {
        update_path(paths.x + e * samples, paths.y + e * samples,
                    density.data(), bandwidth);
      }
    };
    pforr::parallel_for(edges.size, update, threads);
  }

  auto restore = [&](std::size_t begin, std::size_t end) {
    for (auto i = begin; i < end; ++i) {
      paths.x[i] = (paths.x[i] - padding) / (1 - 2 * padding) * extent.span +
                   extent.xmin;
      paths.y[i] = (paths.y[i] - padding) / (1 - 2 * padding) * extent.span +
                   extent.ymin;
    }
  };
  pforr::parallel_for(count, restore, threads);
}

} // namespace vizier::hammer
#endif
