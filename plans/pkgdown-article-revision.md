# ExecPlan: Revise the pkgdown articles

Artifact type: `execplan`

Status: Complete. Both revised articles, the reproduction companion, rendering,
and the two-stage review have passed.

Active phase: Complete.

Next action: None. The implementation is ready for user review.

## Purpose / Big Picture

Revise Vizier's two pkgdown articles so a reader can reach a useful first plot
quickly, choose an appropriate color mapping, and understand the visible
results without wading through implementation detail. Preserve the current
public behavior and useful examples while making the 24 committed article
figures reproducible from tracked source.

The finished work should retain two articles, not create a third. “Getting
Started” should serve the first-use workflow; “Color Schemes” should serve the
specialist palette-selection and interpolation workflow. The plan is a
coverage ledger, not a prescribed prose outline: the documentation guidance
and the named reader tasks control the final visible order and wording.

## Current State

- `_pkgdown.yml` lists `articles/getting-started` and
  `articles/color-schemes` under Guides.
- `vignettes/articles/getting-started.Rmd` now has six task-oriented sections,
  13 displayed R blocks, and 12 figures.
- `vignettes/articles/color-schemes.Rmd` now has four task-oriented top-level
  sections, 13 displayed R blocks, and 12 figures.
- `vignettes/articles/reproduce-figures.R` regenerates 22 static PNGs and the
  two Plotly widget sources. Its printed manual recipe owns the two hovered
  Plotly screenshot captures.
- `vignettes/articles/img/` still contains the same 24 tracked PNG files; no
  image was replaced merely because of local graphics-device differences.
- The displayed blocks are ordinary fenced code, not executable knitr chunks.
  A pkgdown render therefore checks markup and links but does not execute those
  examples or prove that the PNGs match them.
- On 2026-08-25, a fresh temporary installation successfully built both final
  configured articles with `pkgdown::build_article()`. All 26 final displayed
  R blocks also ran successfully in article order after their declared setup.
- Existing tests already cover the important public semantics described by the
  articles: named palettes, palette reversal, default categorical and numeric
  palettes, `top`, conservative character inference, equal axes, ggplot2
  scales, and Plotly output. Article work should not duplicate that suite.
- `vignettes/articles` and `docs` are excluded from the package build by
  `.Rbuildignore`. A reproduction companion in `vignettes/articles` is
  therefore website-maintenance infrastructure, not an installed user asset.
- The worktree was clean before this plan was created.

## Progress

- [x] 2026-08-25: Audit the current article sources, rendered output, image
  inventory, public implementation, and relevant tests.
- [x] 2026-08-25: Validate both configured article builds in a fresh temporary
  package library.
- [x] 2026-08-25: Execute all 21 displayed code blocks against the current
  package.
- [x] 2026-08-25: Create this durable ExecPlan.
- [x] 2026-08-25: Build the content-and-figure coverage ledger and resolve the
  reproduction boundary.
- [x] 2026-08-25: Revise both article sources around their primary reader
  tasks.
- [x] 2026-08-25: Add and exercise tracked reproduction code for the committed
  figures.
- [x] 2026-08-25: Render, inspect, independently review, reconcile, and validate
  the final articles.
- [x] 2026-08-25: Close the plan with final evidence and no deferred scope.

Update this section whenever a phase completes or the next action changes.

## Confirmed Findings and Disposition Crosswalk

- `F1` — “Getting Started” has a weak navigation hierarchy and delays a clear
  first-use payoff.
  Disposition: accepted into Phase 2. Preserve the examples that support the
  reader task, but move or link away detail that belongs to the specialist
  article or API reference.
- `F2` — The 24 precomputed figures have no tracked end-to-end generation
  path.
  Disposition: accepted into Phases 1 and 3. Add one website-only companion
  that covers all retained artifacts; do not make every pkgdown render pay the
  optional plotting and screenshot cost.
- `F3` — Several actions, outputs, and interpretations are separated. The
  `topo.colors` result lacks visible code, the named-palette example lacks a
  visible result, and most color-scheme comparisons omit their commands.
  Disposition: accepted into Phase 2. Each retained example must have a clear
  reader-facing action, observation, and consequence, with long reproduction
  detail kept off the main path.
- `F4` — The color article's opening omits supported input forms and the claim
  that Vizier “makes no distinction” among paletteer types is misleading.
  Disposition: accepted into Phase 2. Reconcile the prose with
  `R/palettes.R::make_palette_function()` and the exported API documentation.
- `F5` — Small consistency issues remain: no declared `library(vizier)` setup,
  inconsistent function notation and “data frame” spelling, trailing
  whitespace, long calls, vague interpolation language, and generic alt text.
  Disposition: accepted into Phases 2 and 4 as bounded editorial cleanup.

Before closing, re-read this crosswalk and give every item final evidence or an
explicit changed/deferred/declined disposition.

## Surprises & Discoveries

- A green article render currently does not exercise any displayed code because
  the sources use ordinary fenced blocks.
- The article directory is intentionally outside the built package. This makes
  a website-only companion the natural distribution boundary and avoids
  exposing a maintenance script as installed package API.
- The examples themselves are currently executable. The main defects are
  reader-path design, visible evidence alignment, and artifact provenance, not
  broken public calls.
- The configured names for focused pkgdown builds include the `articles/`
  prefix: use `articles/getting-started` and `articles/color-schemes`, not only
  the source basenames.
- A fresh pkgdown destination may need network access for standard CDN assets
  even when all package dependencies are already installed. This is an
  environment prerequisite, not an article failure.
- The installed Plotly static-export path requires a separately provisioned
  Python Plotly/Kaleido environment, and a normal static export would still not
  reproduce the hovered tooltip that gives the two screenshots their lesson.
  Generating the widgets automatically and documenting two bounded hover
  captures is therefore the proportionate reproduction boundary.

## Decision Log

- Decision: Keep the existing two-article split and add no new tutorial.
  Rationale: the coverage gap is navigation and integrity within the current
  surfaces; a third article would add maintenance without a distinct reader
  task.
  Date/Author: 2026-08-25 / Codex

- Decision: Treat “Getting Started” as the path from an attached package to a
  useful 2D plot and an informed renderer choice.
  Rationale: that is the shortest first-use payoff promised by its title and by
  the README link.
  Date/Author: 2026-08-25 / Codex

- Decision: Treat “Color Schemes” as the path for choosing categorical versus
  continuous color behavior and understanding interpolation consequences.
  Rationale: palette catalogues and interpolation detail are specialist
  concerns that otherwise overwhelm the quick start.
  Date/Author: 2026-08-25 / Codex

- Decision: Prefer one tracked reproduction companion under
  `vignettes/articles/` over converting all displayed code to live knitr
  chunks.
  Rationale: the figures use base graphics, ggplot2, Plotly, and palette
  swatches; regenerating all of them during every site build would increase
  dependency and maintenance cost. A companion preserves evidence without
  slowing the normal reader path.
  Date/Author: 2026-08-25 / Codex

- Decision: Keep semantic API assertions in package tests unless an invariant
  independently helps the article reader.
  Rationale: the existing tests own contract coverage; article validation
  should prove displayed actions and artifacts without becoming a parallel
  schema test suite.
  Date/Author: 2026-08-25 / Codex

- Decision: Use a cold reader-path review followed by a plan-aware technical
  reconciliation, with at most one bounded correction and re-review pass.
  Rationale: editorial compression and technical coverage need separate
  evidence, and an unbounded prose-review loop would be disproportionate.
  Date/Author: 2026-08-25 / Codex

- Decision: Retain all 24 committed PNGs, but move palette-selection examples
  out of “Getting Started” and into “Color Schemes.”
  Rationale: every figure still demonstrates a supported user action, while
  grouping the palette figures by their specialist task prevents them from
  delaying the first-use path.
  Date/Author: 2026-08-25 / Codex

- Decision: Reproduce 22 static figures automatically and the two Plotly
  screenshots through generated HTML plus an explicit manual hover recipe.
  Rationale: base R can replace the old swatch-only dependency, ggplot2 is
  already suggested, and static Plotly export would add Python/Kaleido tooling
  without preserving the tooltip state shown by the screenshots.
  Date/Author: 2026-08-25 / Codex

## Context and Orientation

Read these files before implementation:

- `vignettes/articles/getting-started.Rmd`: first-use article and 18 of the
  committed article figures.
- `vignettes/articles/color-schemes.Rmd`: specialist palette article and six
  committed article figures.
- `vignettes/articles/img/`: existing static artifacts and target filenames.
- `_pkgdown.yml`: configured article names and navigation.
- `.Rbuildignore`: website-only distribution boundary.
- `R/vizier.R`, `R/ggplot.R`, and `R/plotly.R`: renderer contracts.
- `R/palettes.R` and `R/colors.R`: palette dispatch, defaults, interpolation,
  named mapping, reversal, numeric mapping, and inference behavior.
- `tests/testthat/test-colors.R`, `test-embed-plot.R`, `test-ggplot.R`, and
  `test-plotly.R`: authoritative semantic witnesses already owned by tests.
- `README.md`: current quick-start routing; change it only if an article URL or
  responsibility changes.

Apply the installed `r-package-workflow` and `r-docs-pkgdown` guidance when
executing this plan. For planned technical articles, preserve this document's
scope and factual obligations while allowing the reader task to control visible
order, terminology, and explanatory depth.

## Coverage Obligations

Use these IDs as a reconciliation ledger, not as required headings.

- `C1` — Declare the minimum setup needed to run displayed calls, including
  attaching `vizier` and identifying optional ggplot2 or Plotly requirements at
  the point of use.
- `C2` — Demonstrate a successful first plot before advanced palette or
  inference detail.
- `C3` — Preserve an accurate explanation of categorical defaults, numeric
  defaults, explicit per-row colors, unnamed palettes, named category mappings,
  and `rev` where those behaviors affect a retained example.
- `C4` — Preserve the useful demonstrations of numeric coloring, `top`, equal
  axes, labels, ggplot2 extension, and Plotly interaction, but route contract
  minutiae to the API reference when it does not change the reader's action or
  interpretation.
- `C5` — Describe every supported `color_scheme` form used by the specialist
  article, including palette functions, unnamed and named vectors, paletteer
  names, and built-in R palette names.
- `C6` — Explain paletteer dispatch and the `::c`/`::continuous` override
  precisely. State the observable consequence of interpolating a discrete
  palette rather than relying on vague language such as “got lucky” or “looks
  bad.”
- `C7` — Keep every retained visible figure adjacent to the action that creates
  it and prose that explains what the reader should observe or conclude.
- `C8` — Give every retained PNG a tracked reproduction recipe. Any artifact
  that cannot be automated without disproportionate tooling must have an
  explicit, bounded manual capture recipe rather than silent provenance.
- `C9` — Preserve useful cross-links between the quick-start, specialist
  article, and API reference without adding speculative tables, plots, or
  tutorials.
- `C10` — Keep alt text and captions aligned with the visible lesson, and
  inspect the rendered page at its intended content width.

## Plan of Work

### Phase 1: Reconcile content and figure provenance

1. Inventory every code block and PNG by filename. Record the creating public
   call, required package, input object, article location, and intended visible
   lesson.
2. Classify each current paragraph and example as quick-start path, specialist
   palette path, API-reference detail, reproduction-only detail, or removable
   duplication. Do not add content simply to balance sections.
3. Map each technical claim retained in prose to visible code/output or an
   existing package test. Remove unmatched internal schema assertions from the
   article or move them to the owning reference/test surface.
4. Confirm the reproduction dependency set. Prefer base graphics for palette
   swatches if doing so avoids adding a website-only dependency solely to
   reproduce the existing `swatches` images.
5. Resolve Plotly capture before editing its prose: determine whether the
   current environment supports a deterministic headless screenshot. If that
   requires disproportionate new tooling, retain a documented manual capture
   step and generate the underlying widget automatically.
6. Update `Progress`, `Surprises & Discoveries`, and the decision log with the
   resulting artifact boundary before Phase 2.

### Phase 2: Revise the reader-facing articles

1. Draft the shortest question-to-payoff path for each named reader task before
   mapping `C1`–`C10` onto it.
2. Revise “Getting Started” so setup and the first useful plot precede advanced
   choices. Keep only palette material necessary to make or interpret a
   first-use decision, and link to “Color Schemes” for specialist detail.
3. Revise “Color Schemes” so the reader first understands the categorical
   versus continuous decision, then the accepted input forms and interpolation
   consequence. Reconcile wording against current public implementation and
   reference documentation.
4. Repair action/output/interpretation gaps, especially the current
   `topo.colors`, named-palette, and palette-comparison sequences. Use concise
   visible code or collapsed reproduction detail as appropriate.
5. Apply bounded consistency cleanup: declared setup, function parentheses,
   “data frame” spelling, line wrapping, trailing whitespace, precise technical
   wording, HTTPS links where supported, and lesson-oriented alt text.
6. Do not change `README.md`, `NEWS.md`, roxygen, generated help, or public R
   code unless reconciliation uncovers a real public-contract defect. If that
   occurs, stop and record it as a separate decision rather than expanding this
   documentation plan silently.

### Phase 3: Add the reproduction companion

1. Add `vignettes/articles/reproduce-figures.R` with a configurable output
   directory so validation can generate into a temporary location before any
   committed PNG is replaced.
2. Use package-qualified calls or an explicit setup and the public Vizier API.
   Give optional-package failures clear install messages. Do not make the script
   part of the installed package or public API.
3. Organize the script by article and ensure every retained PNG filename has
   exactly one recipe. Keep the `iris` PCA input and any other upstream inputs
   adjacent to the figures they generate.
4. Make automated generation idempotent. A second run into a fresh directory
   should produce the same file set and semantically identical plots; exact
   byte identity is required only where the graphics stack makes it a stable
   contract in the current environment.
5. Expose the companion from the relevant articles without putting its long
   body on the main reading path. Validate the chosen link or collapsed-details
   mechanism in rendered HTML.
6. Generate to a temporary directory first. Compare filenames, dimensions,
   visible annotations, legends, clipping, and article order before accepting
   any refreshed committed images.

### Phase 4: Validate and review

1. Parse and smoke the companion independently. Run its full temporary-output
   path and confirm all expected files are present; do not use a successful
   article render as its only witness.
2. Execute each displayed code block from its declared setup in a fresh
   environment. If a block is presented as independently copyable, run it
   alone rather than relying on earlier article state.
3. Build both configured articles from a fresh temporary installation with a
   writable temporary cache and destination. Use the exact configured names
   `articles/getting-started` and `articles/color-schemes`.
4. Render the complete site separately and check internal links separately.
   Network failures for pkgdown CDN assets or external URL checks are
   environment results and may require narrowly scoped approval.
5. Inspect the rendered pages at the intended content width. Check navigation,
   code/figure order, captions, alt text, legends, labels, clipping, and the
   static representation of interactive Plotly behavior.
6. Freeze the exact rendered article pair for a cold review using a content
   digest or other immutable identity. Give the reviewer only the audience,
   reader task, article role, neighboring-guide responsibilities, scope
   boundary, and rendered artifacts. Withhold this plan, source implementation,
   and prior critique.
7. Apply at most one bounded correction pass, then obtain one re-review if the
   cold review finds a correctness or reader-path gap. Stop on a pass or
   escalate repeated/disputed findings rather than growing the review loop.
8. Perform the plan-aware technical reconciliation: re-check `F1`–`F5` and
   `C1`–`C10`, confirm editorial compression preserved current contracts and
   executable examples, and confirm every reproduction assertion has a visible
   or test-owned witness.
9. Run final repository hygiene checks and update this plan's outcome and
   validation evidence.

## Concrete Steps

Run commands from the repository root. Adapt temporary directory names to avoid
collisions and clean them after inspection.

1. Reconfirm state and planning visibility:

   ```sh
   git --no-optional-locks status --short --untracked-files=all
   rg --files -uu plans vignettes/articles
   ```

2. After the companion exists, format and smoke it:

   ```sh
   air format vignettes/articles/reproduce-figures.R
   air format --check vignettes/articles/reproduce-figures.R
   Rscript --vanilla vignettes/articles/reproduce-figures.R --output-dir /tmp/vizier-article-figures
   ```

   If the final script uses another argument shape, update this plan and its
   usage text together before relying on the command.

3. Run focused article builds through a direct temporary installation. The R
   harness must create a temporary library, install `.` there with
   `R CMD INSTALL -l`, prepend it to `.libPaths()`, set `R_LIBS_USER`, set
   `XDG_CACHE_HOME` before pkgdown initializes, and call:

   ```r
   pkgdown::build_article(
     "articles/getting-started",
     new_process = FALSE,
     override = list(destination = destination)
   )
   pkgdown::build_article(
     "articles/color-schemes",
     new_process = FALSE,
     override = list(destination = destination)
   )
   ```

   Expected observation: both article files are written without an article,
   package-loading, or rendering diagnostic.

4. Build the whole site into a temporary destination:

   ```sh
   XDG_CACHE_HOME=/tmp/vizier-pkgdown-cache Rscript --vanilla -e 'pkgdown::build_site(new_process = FALSE, override = list(destination = "/tmp/vizier-pkgdown-site"))'
   ```

5. Run link checks separately from rendering. Use the repository's available
   deterministic local-link checker or inspect rendered `href`/`src` targets;
   run external URL checks only when changed external links require them.

6. Check plan and article whitespace explicitly, because a new untracked plan
   may not be covered by `git diff --check`:

   ```sh
   rg -n '[ \t]+$' plans/pkgdown-article-revision.md vignettes/articles/getting-started.Rmd vignettes/articles/color-schemes.Rmd vignettes/articles/reproduce-figures.R
   git diff --check
   ```

7. Run package tests only if package source or public documentation contracts
   change. A website-only prose/image/script revision does not by itself
   justify a full package check; record that scoped decision rather than
   reporting package validation that adds no evidence.

## Validation and Acceptance

The work is complete only when all of the following are true:

- A new reader can identify the required setup and reach a useful first plot
  before encountering specialist palette machinery.
- The color article enables a reader to choose categorical or continuous
  behavior and accurately predicts the consequence of discrete-palette
  interpolation.
- `F1`–`F5` and `C1`–`C10` have final dispositions with evidence.
- Every retained displayed call runs from its declared setup; independently
  copyable examples also run independently.
- Every retained PNG has a tracked automated recipe or an explicit bounded
  manual capture recipe, and the companion succeeds independently into a
  temporary directory.
- Both focused articles and the complete pkgdown site render successfully from
  the current source.
- Rendered internal links and image sources resolve. Changed external links are
  checked separately when network access is available.
- Final figures are inspected at page width for legibility, clipping, captions,
  and figure-text order.
- Cold reader review passes after no more than one correction and re-review
  cycle, followed by successful plan-aware technical reconciliation.
- No unintended changes appear in package source, generated help, `NEWS.md`,
  `README.md`, or unrelated files.
- `git diff --check`, the explicit whitespace scan, and any applicable Air
  check pass.

## Idempotence and Recovery

- Generate figures into a new temporary directory first. Do not overwrite the
  committed image directory until the full file set and visual results have
  been inspected.
- If regenerated images differ because of graphics-device or dependency
  versions, record versions and compare the lesson-bearing content before
  accepting churn. Do not mass-replace images merely to obtain byte identity.
- Keep article edits and regenerated artifacts together only when the visible
  prose and figures describe the same accepted result.
- If an optional plotting or screenshot dependency is unavailable, complete
  all unaffected article and figure work, record the exact remaining artifact,
  and stop short of claiming full reproduction coverage.
- If review occurs against dirty or untracked content, freeze the exact target
  with content hashes. Reconcile stale findings against the current identity;
  do not apply them blindly.
- If compaction occurs before a passing cold-review verdict, restart the bounded
  review explicitly. Do not infer acceptance from an interrupted cycle.
- On resume, re-read this plan, current worktree status, both article sources,
  and the companion; update Current State and Progress before continuing.

## Interfaces and Dependencies

- No exported Vizier interface is expected to change.
- The planned companion's only development interface is a documented output
  directory argument. It should fail clearly when optional packages are absent
  and should not write outside the requested directory.
- Existing optional dependencies include ggplot2 and Plotly. Avoid adding a new
  package dependency solely for palette swatches or screenshots unless the
  reproduction benefit clearly outweighs its installation and maintenance
  cost; record that decision first.
- The article sources, companion, and generated images remain website-only
  under the existing `.Rbuildignore` boundary.

## Artifacts and Notes

The reconciled figure ledger is:

| Artifact | Article role | Reproduction recipe and lesson |
| --- | --- | --- |
| `embed_ex.png` | Getting Started | `embed_plot()` with the `iris` data frame; categorical default and first payoff. |
| `embed_ex_title.png` | Getting Started | `embed_plot()` with factor, function palette, title, and subtitle. |
| `embed_ex_alpha.png` | Getting Started | `embed_plot(alpha_scale = 0.5)`; transparency. |
| `embed_ex_colors.png` | Getting Started | `embed_plot(colors = ...)`; per-row identity colors. |
| `embed_ex_quant.png` | Getting Started | `embed_plot()` with numeric `x`; continuous mapping. |
| `embed_ex_top.png` | Getting Started | `embed_plot(top = 10)`; retain highest numeric values. |
| `embed_ex_ax.png` | Getting Started | `embed_plot(equal_axes = TRUE)`; equal physical units. |
| `embed_ex_text.png` | Getting Started | `embed_plot(text = ...)`; labels instead of points. |
| `embed_ex_ggplot_ellipse.png` | Getting Started | `embed_ggplot()` plus ggplot2 layers; extensibility. |
| `embed_ex_ggplot_numeric.png` | Getting Started | `embed_ggplot()` with numeric colorbar. |
| `embed_ex_plotly.png` | Getting Started | Generated `embed_plotly()` HTML, then manual hover capture at the documented viewport; legend and default tooltip. |
| `embed_ex_plotly_tooltip.png` | Getting Started | Generated `embed_plotly()` HTML, then manual hover capture; custom tooltip without legend. |
| `embed_ex_topo.png` | Color Schemes | `embed_plot(color_scheme = topo.colors)`; palette function. |
| `embed_ex_custom.png` | Color Schemes | `embed_plot()` with an unnamed or equivalently ordered named vector; custom categorical mapping. |
| `embed_ex_okabe_ito.png` | Color Schemes | `embed_plot(color_scheme = "Okabe-Ito")`; built-in R palette name. |
| `embed_ex_cb.png` | Color Schemes | `embed_plot(color_scheme = "RColorBrewer::Dark2")`; paletteer name. |
| `embed_ex_turbo.png` | Color Schemes | `embed_plot(color_scheme = turbo)`; generated palette. |
| `embed_ex_turbo_rev.png` | Color Schemes | The preceding call with `rev = TRUE`; reversal. |
| `dark2_swatch.png` | Color Schemes | Base-graphics swatch of `paletteer_d("RColorBrewer::Dark2")`; full discrete palette. |
| `embed_dark2.png` | Color Schemes | Default discrete `Dark2`; first colors selected. |
| `rainbow_swatch.png` | Color Schemes | Base-graphics swatch of `paletteer_d("jcolors::rainbow")`; ordered palette extent. |
| `embed_jcrainbow.png` | Color Schemes | Default discrete jcolors rainbow; first colors selected. |
| `embed_jcrainbowc.png` | Color Schemes | The same palette with `::c`; sample the full extent. |
| `embed_dark2c.png` | Color Schemes | `Dark2::c`; synthesized midpoint illustrates an unsuitable continuous override. |

All base plots share the deterministic `iris` PCA input. The two swatches use
base rectangles rather than adding the former `swatches` package. The two
ggplot2 figures use the existing suggested dependency; the two Plotly widget
sources use the other existing suggested dependency and its `htmlwidgets`
dependency.

- This plan lives at `plans/pkgdown-article-revision.md`. The `plans/`
  directory is excluded from R package builds by `.Rbuildignore`; verify and
  report its Git tracking state after creation because a new plan may be
  untracked even though it is durable in the workspace.
- The originating audit was performed interactively on 2026-08-25 and has no
  separate repository artifact. Its confirmed findings and dispositions are
  preserved in `F1`–`F5` above so implementation does not depend on chat
  history.
- Do not store workflow papercuts or skill-retrospective records in this plan.
  Those belong to the configured external state root when separately
  authorized and qualified.

## Outcomes & Retrospective

“Getting Started” now reaches a base `embed_plot()` payoff immediately after
one declared setup block, then follows reader questions about color meaning,
coordinates, ggplot2, Plotly, and specialist palette guidance. “Color Schemes”
now begins with the categorical-versus-continuous decision, covers every
supported palette form, and uses visible calls and figures to explain discrete
sampling, continuous overrides, and interpolation consequences.

The original findings are closed as follows:

- `F1`: resolved by the task-oriented hierarchy and quick first plot; the cold
  reader review passed the resulting path.
- `F2`: resolved by `vignettes/articles/reproduce-figures.R`, which creates 22
  PNGs and two local Plotly widget sources, plus the bounded manual hover recipe
  for the two screenshot PNGs.
- `F3`: resolved by moving every retained figure next to its creating call and
  interpretation. This includes `topo.colors`, the named custom mapping, and
  each discrete-versus-continuous palette comparison.
- `F4`: resolved by listing functions, unnamed vectors, named mappings,
  paletteer names, and built-in R names, and by describing the implementation's
  continuous, discrete, and dynamic dispatch accurately.
- `F5`: resolved by explicit `library(vizier)` setup, consistent function
  notation and “data frame” spelling, wrapped code, clean whitespace, precise
  interpolation language, current HTTPS links, and task-specific alt text.

Coverage reconciliation:

- `C1`–`C2`: the setup and first plot lead “Getting Started”; optional ggplot2
  and Plotly requirements appear at their first use.
- `C3`: defaults, explicit colors, unnamed and named palettes, and reversal are
  visible in the two articles; package tests remain the semantic owner.
- `C4`: numeric coloring, `top`, equal axes, labels, ggplot2, and Plotly are on
  the quick-start path, with detailed contracts linked to API references.
- `C5`–`C6`: “Color Schemes” covers every input form and accurately explains
  paletteer type dispatch, `::c`/`::continuous`, and the synthesized Dark2
  midpoint.
- `C7`: each retained image follows the action and explanation it supports.
- `C8`: all 24 PNGs have a recipe in the companion or its bounded Plotly
  capture instructions.
- `C9`: the two articles cross-link and route exhaustive contracts to the API
  reference without adding another guide, table, or plot.
- `C10`: all 24 rendered images have lesson-oriented alt text; committed and
  regenerated figures were inspected for labels, legends, clipping, and order.

Validation evidence:

- Air formatting and parsing passed for the 402-line companion.
- Against a fresh temporary Vizier install, the companion ran twice into the
  same temporary output directory and each run produced 22 PNGs and two Plotly
  HTML widgets.
- All 13 R blocks in each article executed in article order after the declared
  setup.
- Both focused article builds passed from a fresh temporary install. A complete
  pkgdown site build also passed into `/tmp`; its only warning was the unchanged
  Plotly `@examplesIf interactive()` condition evaluating false.
- Deterministic local-link checks passed for both rendered pages, and all 24
  rendered images had non-empty alt text. `urlchecker::url_check()` reported no
  article URL issue; its only finding was an existing README Codecov redirect.
- The frozen rendered pair and 24 images had digest
  `dcf76382d391aab7290894ee5322f37fa8c5159ca3ecfdf75ec06f0e852523a0`.
  A cold reviewer received only the rendered artifacts, audience, tasks, roles,
  and scope boundary and returned `PASS` with no correction cycle.
- Final plan-aware reconciliation confirmed `F1`–`F5` and `C1`–`C10` against
  the article sources, public implementation, existing package tests, rendered
  pages, and companion outputs.

No public API, package source, generated help, README, NEWS, dependency, or
committed PNG changed. No scope was deferred or declined.
