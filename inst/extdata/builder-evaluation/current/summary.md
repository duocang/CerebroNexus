# Builder Evaluation Summary

Overall: **PASS**

## Correctness gates

- PASS — `protocol_schema`
- PASS — `capability_detection`
- PASS — `plan_immutability`
- PASS — `artifact_fidelity`
- PASS — `build_success`
- PASS — `handled_recovery`
- PASS — `incremental_scope`

## Quantitative evidence

- Capability detection: 100/100 correct; TP=21, FP=0, FN=0, TN=79; precision=1.000, recall=1.000.
- Plan immutability: 126/126 boundary checks passed.
- Artifact fidelity: 1.000; omission=0.000; unintended inclusion=0.000.
- End-to-end builds: 42/42 succeeded; median elapsed=2.391 s.
- Release recovery: 9/9 trials reopened the prior CRB and then published cleanly (5 independent process exits).
- Incremental rebuild: 2/2 eligible datasets reused and 1/1 changed datasets rebuilt; measured time saved=0.634.

Raw trial-level evidence is stored in the CSV files in this directory.
