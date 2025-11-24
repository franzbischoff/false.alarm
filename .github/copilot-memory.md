# Copilot Memory

## 2025-11-19 — Timestamp adjustment for FLOSS detections

- Finding: FLOSS `regime_landmark` represents the delay (in seconds) between the time an event is evaluated and the moment the algorithm reports it. When a landmark is e.g. 2 seconds the detection was evaluated 2s after the streaming time where the change actually occurred.

- Required change: Adjust predicted timestamps by subtracting the landmark delay (seconds → samples) so the saved `pred` corresponds to the timestamp at the stream origin (time=0), not the evaluation moment. In other words, if `regime_landmark == 2`, then all predicted samples returned by `floss_predict()` must be shifted back by `2 * sample_freq` samples.

- Where to implement:
  - Primary place: `scripts/regime_detection/30_predict_grid_search.R` — after `cleaned_pred <- clean_pred(raw_pred, min_gap)` insert a small adjustment: `adjusted_pred <- cleaned_pred - floor(rl * const_sample_freq)` before saving.
  - Note: `rl` (regime_landmark) is in seconds in `base_grid` while the `floss_predict()` code converts it to samples inside `floss_predict()` (see `floss_predict` > `regime_landmark = floor(regime_landmark * sample_freq)`). Keep consistency and use `const_sample_freq` (250 Hz) from config if present.

- Other implications and tests:
  - Ensure `adjusted_pred` is non-negative — clamp to `>= 1` if necessary.
  - Update `clean_pred()` or the calling code if `clean_pred()` expects sample indices relative to an offset.
  - Add unit tests in `tests/` verifying shift is performed: use a fixed `floss_list` for which the predicted `idxs` are known and check the adjusted value equals `original_idx - landmark_samples`.

- Follow-up action: Implement small patch in `30_predict_grid_search.R` and add tests; then re-run `30_predict_grid_search_test.R` to validate.

---

(Added by Copilot on 2025-11-19)

## 2025-11-19 — End of day summary

- Status: Grid search checkpointing implemented in `scripts/regime_detection/30_predict_grid_search.R` and a small TEST script `30_predict_grid_search_test.R` was added to validate the checkpoint logic.
- Test run: The test script was updated to use `future::multicore` per request and launched — currently running (or will continue as next steps). If interrupted, the checkpoint will allow resumation.
- Memory note: Added instruction to adjust `pred` timestamps by subtracting `regime_landmark` (seconds → samples) so predictions align with original stream timestamps.

Tasks pending to finish tomorrow:
- Collect test outputs and verify `predictions_w{size}.rds` files were created as expected. Then merge to `predictions_grid.rds` and validate row counts.
- Implement the timestamp adjustment in `30_predict_grid_search.R` (simple, one-line shift before saving) and add unit tests in `tests/` to ensure it behaves as expected.
- Run full pipeline with all 16 windows and monitor for `externalptr` or large global export issues.

Notes for next run:
- Use the command below to resume or run the full script once tests pass:
  Rscript scripts/regime_detection/30_predict_grid_search.R 2>&1 | tee /tmp/predict_full.log

End-of-day: The main blocker was instability with `future` when trying to parallelize across records (externalptr serialization) — we avoided this by parallelizing across windows and using per-worker `readRDS()` for matrix profiles.
