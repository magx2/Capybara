# Performance Results

Generated from `results.csv`. Do not edit by hand.

Legend: 🟢 faster, 🔵 roughly unchanged, 🟡 small regression, 🔴 regression.

## 2026-06-29 [80fbebd](https://github.com/magx2/Capybara/commit/80fbebde0415fd6945aab64cb05c29e5bfe3f2d3)

[Changes since previous benchmark check](https://github.com/magx2/Capybara/compare/77cb03c00cf8d3f7c75bd323c86960d4486186c3...80fbebde0415fd6945aab64cb05c29e5bfe3f2d3)

| Backend | compile | gen java | gen python | gen js | test |
| --- | ---: | ---: | ---: | ---: | ---: |
| `Java` | 2s (2.3 s)<br>🟢 Δ-0s / -13% | 2s (1.631 s)<br>🟢 Δ-1m 24s / -98% | 1s (1.107 s)<br>🟢 Δ-1m 24s / -99% | 1s (0.954 s)<br>🟢 Δ-1m 22s / -99% | 11s (11.13 s)<br>🟢 Δ-5s / -33% |
| `Python` | 2s (2.088 s)<br>🟢 Δ-0s / -17% | 2s (1.575 s)<br>🟢 Δ-1m 23s / -98% | 1s (1.061 s)<br>🟢 Δ-1m 22s / -99% | 1s (0.98 s)<br>🟢 Δ-1m 21s / -99% | 11s (11.358 s)<br>🟢 Δ-6s / -33% |
| `JS` | 2s (1.916 s)<br>🟢 Δ-1s / -32% | 2s (1.534 s)<br>🟢 Δ-1m 23s / -98% | 1s (1.106 s)<br>🟢 Δ-1m 22s / -99% | 1s (0.977 s)<br>🟢 Δ-1m 23s / -99% | 11s (11.246 s)<br>🟢 Δ-6s / -35% |

## 2026-06-20 [77cb03c](https://github.com/magx2/Capybara/commit/77cb03c00cf8d3f7c75bd323c86960d4486186c3)

[Changes since previous benchmark check](https://github.com/magx2/Capybara/compare/4c4e64a6fd0d83eb5718527f4f3d9f57211323b6...77cb03c00cf8d3f7c75bd323c86960d4486186c3)

| Backend | compile | gen java | gen python | gen js | test |
| --- | ---: | ---: | ---: | ---: | ---: |
| `Java` | 3s (2.632 s)<br>🟢 Δ-0s / -0% | 1m 26s (85.751 s)<br>🟡 Δ+5s / +6% | 1m 25s (84.86 s)<br>🟡 Δ+5s / +6% | 1m 23s (83.159 s)<br>🟡 Δ+3s / +4% | 17s (16.597 s)<br>🟢 Δ-1s / -4% |
| `Python` | 3s (2.524 s)<br>🟢 Δ-0s / -10% | 1m 25s (84.972 s)<br>🟢 Δ-1s / -1% | 1m 23s (83.485 s)<br>🔵 Δ+1s / +1% | 1m 22s (82.276 s)<br>🔵 Δ+1s / +1% | 17s (16.867 s)<br>🟢 Δ-1s / -6% |
| `JS` | 3s (2.811 s)<br>🟢 Δ-0s / -5% | 1m 25s (84.977 s)<br>🔵 Δ+1s / +1% | 1m 23s (82.86 s)<br>🔵 Δ+1s / +1% | 1m 24s (84.17 s)<br>🟡 Δ+3s / +4% | 17s (17.205 s)<br>🔵 Δ+0s / +0% |

## 2026-06-19 [4c4e64a](https://github.com/magx2/Capybara/commit/4c4e64a6fd0d83eb5718527f4f3d9f57211323b6)

[Changes since previous benchmark check](https://github.com/magx2/Capybara/compare/5d6201373d073065277f10e8a0b9a37c5c918bf0...4c4e64a6fd0d83eb5718527f4f3d9f57211323b6)

| Backend | compile | gen java | gen python | gen js | test |
| --- | ---: | ---: | ---: | ---: | ---: |
| `Java` | 3s (2.638 s)<br>🟡 Δ+0s / +5% | 1m 21s (80.621 s)<br>🟡 Δ+5s / +7% | 1m 20s (79.758 s)<br>🟡 Δ+5s / +7% | 1m 20s (79.849 s)<br>🟡 Δ+6s / +8% | 17s (17.31 s)<br>🟡 Δ+1s / +6% |
| `Python` | 3s (2.789 s)<br>🔴 Δ+0s / +18% | 1m 26s (85.936 s)<br>🔴 Δ+10s / +13% | 1m 22s (82.359 s)<br>🟡 Δ+7s / +10% | 1m 21s (81.267 s)<br>🟡 Δ+7s / +9% | 18s (17.941 s)<br>🟡 Δ+1s / +8% |
| `JS` | 3s (2.955 s)<br>🔴 Δ+0s / +19% | 1m 24s (83.967 s)<br>🔴 Δ+8s / +11% | 1m 22s (82.067 s)<br>🟡 Δ+7s / +9% | 1m 21s (81.2 s)<br>🟡 Δ+7s / +9% | 17s (17.133 s)<br>🔵 Δ+0s / +1% |

## 2026-06-19 [5d62013](https://github.com/magx2/Capybara/commit/5d6201373d073065277f10e8a0b9a37c5c918bf0)

[Changes since previous benchmark check](https://github.com/magx2/Capybara/compare/349b20aa03c402bb010d856747a6bb5891e6ae4c...5d6201373d073065277f10e8a0b9a37c5c918bf0)

| Backend | compile | gen java | gen python | gen js | test |
| --- | ---: | ---: | ---: | ---: | ---: |
| `Java` | 3s (2.502 s)<br>🟢 Δ-0s / -4% | 1m 16s (75.64 s)<br>🟡 Δ+2s / +3% | 1m 14s (74.39 s)<br>🟡 Δ+3s / +4% | 1m 14s (74.116 s)<br>🟡 Δ+2s / +3% | 16s (16.395 s)<br>🟡 Δ+0s / +2% |
| `Python` | 2s (2.358 s)<br>🟡 Δ+0s / +9% | 1m 16s (76.009 s)<br>🟡 Δ+3s / +4% | 1m 15s (75.123 s)<br>🟡 Δ+3s / +4% | 1m 14s (74.306 s)<br>🟡 Δ+3s / +4% | 17s (16.623 s)<br>🟡 Δ+0s / +2% |
| `JS` | 2s (2.475 s)<br>🔵 Δ+0s / +2% | 1m 16s (75.969 s)<br>🟡 Δ+2s / +3% | 1m 15s (75.016 s)<br>🟡 Δ+3s / +5% | 1m 14s (74.339 s)<br>🟡 Δ+3s / +4% | 17s (16.889 s)<br>🟡 Δ+1s / +4% |

## 2026-06-19 [349b20a](https://github.com/magx2/Capybara/commit/349b20aa03c402bb010d856747a6bb5891e6ae4c)

[Changes since previous benchmark check](https://github.com/magx2/Capybara/compare/f46942aa64ec07cf07baefcc59bb83d06d70421e...349b20aa03c402bb010d856747a6bb5891e6ae4c)

| Backend | compile | gen java | gen python | gen js | test |
| --- | ---: | ---: | ---: | ---: | ---: |
| `Java` | 3s (2.6 s)<br>🔴 Δ+0s / +16% | 1m 14s (73.735 s)<br>🟢 Δ-1m 47s / -59% | 1m 12s (71.665 s)<br>🟢 Δ-1m 49s / -60% | 1m 12s (71.723 s)<br>🟢 Δ-1m 49s / -60% | 16s (16.01 s)<br>🟢 Δ-1s / -7% |
| `Python` | 2s (2.168 s)<br>🟢 Δ-0s / -16% | 1m 13s (73.204 s)<br>🟢 Δ-1m 49s / -60% | 1m 12s (71.927 s)<br>🟢 Δ-1m 48s / -60% | 1m 12s (71.662 s)<br>🟢 Δ-1m 46s / -60% | 16s (16.244 s)<br>🟢 Δ-0s / -2% |
| `JS` | 2s (2.429 s)<br>🟢 Δ-0s / -7% | 1m 14s (73.651 s)<br>🟢 Δ-1m 46s / -59% | 1m 12s (71.648 s)<br>🟢 Δ-1m 47s / -60% | 1m 11s (71.306 s)<br>🟢 Δ-1m 46s / -60% | 16s (16.273 s)<br>🟢 Δ-0s / -1% |

## 2026-06-19 [f46942a](https://github.com/magx2/Capybara/commit/f46942aa64ec07cf07baefcc59bb83d06d70421e)

[Changes since previous benchmark check](https://github.com/magx2/Capybara/compare/176dd6b54b5660641e58da2187cc6ef623bf4d72...f46942aa64ec07cf07baefcc59bb83d06d70421e)

| Backend | compile | gen java | gen python | gen js | test |
| --- | ---: | ---: | ---: | ---: | ---: |
| `Java` | 2s (2.25 s)<br>🟢 Δ-1s / -23% | 3m 1s (181.223 s)<br>🔴 Δ+1m 36s / +114% | 3m 1s (180.957 s)<br>🔴 Δ+1m 37s / +115% | 3m 1s (180.961 s)<br>🔴 Δ+1m 37s / +117% | 17s (17.155 s)<br>🟢 Δ-1s / -3% |
| `Python` | 3s (2.58 s)<br>🟢 Δ-0s / -13% | 3m 3s (182.672 s)<br>🔴 Δ+1m 37s / +112% | 3m 0s (180.386 s)<br>🔴 Δ+1m 37s / +117% | 2m 58s (177.961 s)<br>🔴 Δ+1m 36s / +116% | 17s (16.629 s)<br>🟢 Δ-1s / -5% |
| `JS` | 3s (2.623 s)<br>🟢 Δ-0s / -10% | 3m 0s (179.667 s)<br>🔴 Δ+1m 36s / +114% | 2m 59s (178.752 s)<br>🔴 Δ+1m 35s / +114% | 2m 58s (177.787 s)<br>🔴 Δ+1m 35s / +114% | 16s (16.419 s)<br>🟢 Δ-2s / -10% |

## 2026-06-19 [176dd6b](https://github.com/magx2/Capybara/commit/176dd6b54b5660641e58da2187cc6ef623bf4d72)

| Backend | compile | gen java | gen python | gen js | test |
| --- | ---: | ---: | ---: | ---: | ---: |
| `Java` | 3s (2.905 s)<br>no previous run | 1m 25s (84.774 s)<br>no previous run | 1m 24s (84.087 s)<br>no previous run | 1m 24s (83.533 s)<br>no previous run | 18s (17.732 s)<br>no previous run |
| `Python` | 3s (2.961 s)<br>no previous run | 1m 26s (86.139 s)<br>no previous run | 1m 23s (82.978 s)<br>no previous run | 1m 22s (82.319 s)<br>no previous run | 18s (17.561 s)<br>no previous run |
| `JS` | 3s (2.92 s)<br>no previous run | 1m 24s (83.893 s)<br>no previous run | 1m 23s (83.487 s)<br>no previous run | 1m 23s (83.173 s)<br>no previous run | 18s (18.314 s)<br>no previous run |
