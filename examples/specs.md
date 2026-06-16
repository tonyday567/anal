# Speculations and next experiments

A wrap-up card for the `anal` daily-return analysis after adding
magnitude models, sign-bucket forecasts, signal strategies, Monte-Carlo
simulation, information coefficients and strategy gradients.

## What we found

- **Returns are close to white noise; volatility is not.**
  Autocorrelation of raw returns is near zero, while autocorrelation of
  `|r|` and of the Mealy `std` is strong and persistent.

- **Magnitude is forecastable, but only just.**
  The best one-step model we have is a multi-feature regression
  `|r_t| ~ |r_{t-1}| + weekly-kernel + monthly-kernel + yearly-kernel`,
  with `R² ≈ 0.225` over the full sample and `R² ≈ 0.287` since 2000.
  GARCH(1,1) conditional standard deviation is almost as good.

- **The lagged absolute-return coefficient is close to 1.**
  Across the full sample `β ≈ 0.90`; since 2000 it is `β ≈ 0.89`.
  This suggests the magnitude process is locally close to an AR(1) in
  `|r|`, and the moving-average-difference kernels are measuring
  deviations from that local level.

- **Tail reversal is real but regime-dependent.**
  The most negative previous-day bucket has a positive next-day mean
  return. Over the full sample the effect is ~8.5 bps; since 2000 it is
  ~4.9 bps. The signal strategy beats buy-and-hold raw only in the
  post-2000 subsample.

- **Turnover is high.**
  The current bucket strategy changes position by ~37 percentage points
  per day on average, while running at only ~50% average exposure.
  Transaction costs would eat materially into the edge.

- **Information coefficients confirm the volatility link.**
  Normalised mutual information between `|r_t|` and lagged GARCH std is
  the strongest we measured (`NMI ≈ 0.033` full sample, `0.044` since
  2000), but the overall dependence is still weak in absolute terms.

- **Gradients are available and useful.**
  Bucket weights, online-decay rates and regression coefficients are
  differentiable or finite-difference friendly. Quantile thresholds are
  piecewise-constant, so finite-difference sensitivities are more
  informative than true derivatives.

## Speculations I currently favour

1. **The magnitude process is a locally mean-reverting AR(1) in `|r|`.**
   The coefficient near 1 says today's absolute return is the best
   predictor of tomorrow's; the negative coefficients on the weekly
   `maDiff` kernel say that when `|r|` is *above* its fast moving average
   it tends to fall back. A richer model might be an ARMA-style update
   rather than a regression on separate features.

2. **The post-2000 outperformance is a volatility-regime story.**
   Higher volatility clustering since 2000 gives the reversal signal more
   room to work. In the long 1982-2000 bull market there were fewer
   extreme down days, so the strategy spent less time in the high-weight
   bucket.

3. **Bucket thresholds are risk-control knobs, not just statistical cuts.**
   The large finite-difference sensitivities at the 0.5/0.6/0.9
   quantiles, and the high sensitivity to the `digitize` decay rate,
   suggest the *definition* of the buckets matters as much as the
   position weights. They control when the strategy goes flat versus
   fully long.

4. **Turnover is the binding constraint.**
   The strategy's risk-adjusted profile is similar to buy-and-hold, but
   the raw exposure is half. The scaled strategy looks attractive, but
   the ~37 pp/day turnover means implementation shortfall could dominate.
   The next big gains probably come from smoothing the signal, not from
   a better magnitude forecast.

5. **GARCH and the Mealy statistics are capturing the same latent factor.**
   The fact that GARCH std wins on NMI while the multi-feature model
   matches it on `R²` suggests there is one dominant volatility factor.
   A model that combines GARCH with the `maDiff` kernels might reach
   `R² ≈ 0.30` without much extra work.

## Potential experiments

### Near-term, low-risk

- **Optimise bucket weights under a turnover penalty.**
  Use the analytical weight gradient and add a cost proportional to
  `|Δweight|`. Solve a simple constrained optimisation.

- **Grid-search the continuous decay rates.**
  The `std` decay, the `digitize` decay, and the weekly/monthly/yearly
  `maDiff` decays all affect performance. A coarse grid would show which
  ones matter most.

- **Rolling / expanding-window backtest.**
  Estimate quantiles and regression coefficients on a trailing window,
  then trade forward. This is the real out-of-sample test; the current
  numbers are in-sample.

- **Transaction-cost sweep.**
  Apply a fixed bp cost per trade and recompute annualised returns.
  Find the break-even cost where the strategy loses its edge.

### Medium-term

- **Soft / differentiable digitisation.**
  Replace the hard bucket assignment with a softmax over distances to
  thresholds. This would give true gradients w.r.t. thresholds and allow
  gradient-based optimisation of the whole pipeline.

- **Use `Data.Mealy.Diff` for end-to-end gradients.**
  Make the decay rates and regression coefficients parameters in a
  differentiable Mealy network and backprop the final return or a risk
  objective through the scan.

- **Add external signals.**
  VIX, realised-volatility changes, volume, yield-curve slope, or macro
  regime indicators could raise the information coefficient.

- **Multi-horizon targets.**
  Predict 5-day, 10-day and 20-day cumulative return sign/magnitude.
  The short-term reversal may be stronger or weaker at longer horizons.

### Longer / more speculative

- **Explicit residual distribution modelling.**
  Fit a GARCH-like variance with Student-t or skew-t innovations, then
  use it for a proper maximum-likelihood forecast rather than `R²`.

- **Compare with ML baselines.**
  Logistic regression, gradient-boosted trees or a tiny MLP on lagged
  return buckets and volatility features. This tells us how much of the
  signal is linear / explainable.

- **Cross-asset test.**
  Run the same pipeline on Nasdaq, Russell 2000, bond futures, FX or
  crypto to see which markets share the same AR(1)-in-volatility
  structure.

- **Build a differentiable portfolio-weight Mealy.**
  Use the `Diff'` machinery to learn the mapping from signal to
  position size directly, with a Sharpe-ratio or drawdown objective.
