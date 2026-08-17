# Test Matrix Acceleration Design

## 1. Purpose

Accelerate both the local full validation and the remote `R tests` workflow
without removing test files, assertions, browser coverage, failure artifacts,
or process isolation.

The current suite is complete and green, but its scheduling is based on file
count rather than runtime. That produces large load imbalances:

- local validation runs all 11 test shards serially and takes 36 minutes 34
  seconds wall time, including 32 minutes 19 seconds of tests;
- remote `R tests` runs the shards in parallel but still takes 11 minutes 40
  seconds because the slowest shard determines the workflow wall time;
- logic shards range from 62 to 310 seconds locally;
- browser shards range from 96 to 279 seconds locally.

The primary objective is therefore better scheduling, not fewer tests.

## 2. Success Criteria

The change is successful when all of the following are true:

1. All 145 current test files are assigned exactly once, with no omissions or
   duplicates.
2. The same files, weights, shard count, and strategy always produce identical
   assignments.
3. Remote CI retains four logic shards, one independent process-sensitive job,
   six browser shards, browser failure artifacts, and the protected summary
   check named `R tests / test`.
4. Two consecutive local full test-matrix runs pass with default concurrency.
5. The final remote `R tests`, `R-CMD-check`, and `pkgdown` workflows pass.
6. The target remote `R tests` wall time is 9 minutes 30 seconds or less, or at
   least 15 percent faster than the 11 minute 40 second baseline if runner
   variability prevents the absolute target.
7. The target local full validation wall time is 18 minutes or less on the
   reference Apple Silicon Mac when it is not under unrelated heavy load.

Timing targets are performance goals, not reasons to weaken correctness or
hide failures.

## 3. Scope and Non-goals

### In scope

- deterministic runtime-weighted shard assignment;
- a versioned file-runtime registry seeded from the completed local run;
- a repository-owned local validation runner with bounded concurrency;
- per-shard logs, durations, exit codes, and a final fail-late summary;
- tests for assignment correctness and local scheduling behavior;
- before-and-after local and remote timing measurements.

### Out of scope

- changing test assertions or selectors;
- sharing one AppDriver or Shiny process between test files;
- automatically retrying failed tests;
- increasing the number of GitHub Actions jobs;
- changing `R-CMD-check` or `pkgdown` semantics;
- editing the user's existing uncommitted `scripts/precheck.sh` changes;
- killing user processes automatically.

## 4. Chosen Approach

Use two complementary changes:

1. **Remote and local: deterministic runtime-weighted sharding.**
2. **Local only: bounded parallel execution of the weighted shards.**

This preserves test semantics and attacks the two measured causes directly:
remote load imbalance and local serial execution.

### Alternatives considered

#### Historical auto-balancing

CI could download timings from previous runs and create a new assignment on
each run. This adapts automatically, but makes a failing assignment harder to
reproduce and adds workflow state and artifact dependencies. It is deferred.

#### Shared browser processes

Reusing AppDriver or Shiny processes could remove much of the browser startup
cost, but it changes isolation boundaries across 25 browser test files. The
risk is disproportionate to the first-stage target and is explicitly deferred.

#### More CI shards

More jobs could reduce wall time without rebalancing, but would increase setup
work and Actions consumption while leaving the underlying scheduling defect.
The existing four-plus-six matrix remains unchanged.

## 5. Runtime Weight Registry

Add a versioned registry under `scripts/` with one record per known test file.
Each record contains:

- group: `logic`, `process-sensitive`, or `browser`;
- test filename;
- runtime weight in seconds;
- basis: `measured` or `estimated`.

The initial values come from the completed 4c62b4ec local verification logs.
Where a reliable file-level duration cannot be recovered, use the median of
the measured files in that group and mark the value `estimated`.

Registry rules:

- filenames must use `test-*.R` and exist in the current test plan;
- entries must be unique and weights must be finite positive numbers;
- stale, duplicate, or cross-group entries are errors;
- a newly added valid test file does not break CI: it receives the current
  median weight for its group until the registry is refreshed;
- the registry is advisory scheduling data and never controls whether a test
  runs.

The registry is updated intentionally after a representative complete run,
not automatically during ordinary CI.

## 6. Deterministic Weighted Assignment

Keep the existing round-robin implementation available as a compatibility and
rollback strategy. Add weighted assignment using longest-processing-time-first
scheduling:

1. Resolve a positive weight for every file.
2. Sort files by descending weight, then ascending filename.
3. Assign each file to the shard with the smallest current total weight.
4. Break shard-load ties by the lowest shard number.
5. Sort filenames within each completed shard for stable test filters and
   readable logs.

The command-line runner gains an explicit strategy option. Its default remains
`round-robin` for backward compatibility. GitHub Actions and the new local
runner explicitly request `weighted`. A one-line workflow change can return CI to
round-robin if rollback is needed.

The workflow matrix sizes, job names, environment variables, artifact paths,
and summary-job dependencies do not change.

## 7. Local Parallel Validation Runner

Add a separate repository-owned local validation entry point. It does not
modify or wrap the user's current `scripts/precheck.sh` working copy.

The runner executes these phases in order:

1. logic shards with a default maximum concurrency of three;
2. process-sensitive by itself, after all logic processes exit;
3. browser shards with a default maximum concurrency of two;
4. for full mode, `R CMD check` and pkgdown sequentially after tests.

The logic and browser concurrency limits are configurable, with allowed ranges
of one to four and one to three respectively. Defaults favor stability on the
reference Mac rather than maximum CPU saturation.

Each child command receives:

- the same weighted strategy and shard counts used by CI;
- a unique log path;
- a unique browser artifact directory;
- `CEREBRO_RUN_BROWSER_TESTS=true` for browser shards;
- the repository source path required by copied Builder workers.

The runner is fail-late: it records a failed exit code but continues launching
the remaining scheduled work. Its own final exit code is nonzero if any phase
failed.

Before launching browser work, the runner checks for recognizable stray Shiny
or Cerebro launcher processes. If found, it stops with a diagnostic listing;
it does not kill them.

The final report contains phase and shard start/end times, wall time, exit
codes, log locations, and the slowest shards. Logs and transient artifacts go
to a unique temporary root rather than the repository by default.

## 8. Failure Handling

- Invalid timing data fails during plan validation before tests start.
- A missing weight uses the documented group median and is reported in plan
  output; it does not omit the file.
- A child launch failure is recorded as a shard failure and does not prevent
  unrelated shards from running.
- Browser failures retain independent artifacts and logs per shard.
- No failure is retried automatically.
- Interrupting the local runner stops its child processes and prints the
  partial result locations.

## 9. Test Strategy

### Shard-plan tests

Add or extend static tests to prove:

- weighted assignment is deterministic;
- every discovered file appears exactly once;
- load and filename ties are stable;
- invalid, duplicate, stale, or non-positive registry entries fail clearly;
- new unregistered files receive the group default;
- round-robin remains available;
- workflow shard counts, job names, environment, artifacts, and summary needs
  remain unchanged;
- workflow commands explicitly select weighted assignment.

### Local-runner tests

Exercise the scheduler with short synthetic child commands to prove:

- concurrency caps are respected;
- process-sensitive never overlaps another phase;
- browser shards receive distinct environment and artifact paths;
- failures are collected without stopping later work;
- the aggregate exit code and summary are correct;
- interruption cleans up owned children without touching unrelated processes.

### End-to-end validation

1. Validate and print the old and new assignments with predicted shard loads.
2. Run the complete local matrix twice with default concurrency.
3. Run local `R CMD check` and pkgdown in full mode.
4. Push to the duocang branch and dispatch the three workflows.
5. Monitor every job to completion.
6. Compare per-shard and total wall time with the recorded baseline.

## 10. Expected Outcome

The expected first-stage result is:

- local full validation reduced from 36 minutes 34 seconds to approximately
  15–18 minutes;
- remote `R tests` reduced from 11 minutes 40 seconds to approximately 8–9
  minutes;
- no change to test count, assertions, browser coverage, isolation boundaries,
  protected check names, or failure diagnostics;
- a repeatable local command that produces timing statistics without ad-hoc
  shell orchestration;
- a low-risk rollback to round-robin scheduling.

If measured performance misses the target while correctness remains green,
the timing report will identify whether the next step should be adjusted local
concurrency, refreshed weights, or a separately reviewed browser-process reuse
design. The first-stage implementation will not silently expand into that
higher-risk work.

## 11. Implementation Order

1. Recover file-level timing data and create the validated registry.
2. Add weighted assignment tests, then implement deterministic assignment.
3. Update CLI selection and workflow static contracts.
4. Add scheduler tests, then implement the local parallel runner.
5. Run focused plan and scheduler tests.
6. Run two complete local validations and collect statistics.
7. Perform specification and quality reviews.
8. Commit and push implementation changes.
9. Dispatch and monitor all remote workflows.
10. Report actual before-and-after timings and any target variance.
