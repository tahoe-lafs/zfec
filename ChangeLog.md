# Changelog for fec

## 2.0.0 (2025-03-17)

`fec_init` completely removed because the global tables are initialized data, i.e., the values in the tables are listed in C.

## 1.0 0 (2025-03-17)

* Haskell wrapper returns IO throughout to avoid unsafePerformIO

## 0.2.0 (2023-10-06)

* Application code must now execute the `Codec.FEC.initialize` action at least
  once before using other `Codec.FEC` APIs.

* `Codec.FEC.fec`, `Codec.FEC.encode`, and `Codec.FEC.decode` are now thread-safe.

* `Codec.FEC.fec` now supports n == k.
