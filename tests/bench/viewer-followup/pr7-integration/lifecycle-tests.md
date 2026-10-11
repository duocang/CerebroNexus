# Gene UI lifecycle validation

Sources: PR7 `2102524976629a7e04786653a96b46b62e965c30` and
`e9a28c9a6adb4ebe1229c159df5dd22152e8d237`.

Baseline `2dbd6668` production bodies fail six of fifteen assertions after
correcting the test harness to read mutable state from the live server scope
(testServer's copied scalar bindings do not track later `<<-` assignments).
The initial harness failures were not treated as production evidence.
`baseline-tests.txt` contains the corrected reproduction, alongside the
separate six canonical selection failures.

Four lifecycle failures are: hidden-page progress remains open; recalculating
an already-painted key reopens progress; no-request calculation opens progress;
hidden calculation opens progress. Two group failures are: an obsolete group
column reaches the plotting call; a non-vector column with the wrong length
reaches it. The candidate passes all fifteen assertions, including existing
unpainted progress, obsolete acknowledgement rejection, no expression
recalculation on sidebar/request changes, valid replacement group rendering,
and a regular validation error that clears the old output instead of using
`cancelOutput=TRUE` to retain it.

The port isolates the new sidebar/request reads; PR7's direct reactive
dependencies are not copied. No expression cache/backend, primary emission,
renderer, acknowledgement or painted-ready gate changes. Closing progress on
navigation and avoiding an already-painted duplicate are UI correctness only.

Combined focused suites: 198 passing expectations, no failures/warnings/skips
(`targeted-tests.txt`). Native Brave is blocked by the URL safety failure
recorded in `selection-tests.txt`; these tests do not establish browser acceptance.
