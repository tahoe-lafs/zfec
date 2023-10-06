# Changelog for fec

## 0.2.0 (2023-10-06)

* Application code must now execute the `Codec.FEC.initialize` action at least
  once before using other `Codec.FEC` APIs.

* `Codec.FEC.fec`, `Codec.FEC.encode`, and `Codec.FEC.decode` are now thread-safe.

* `Codec.FEC.fec` now supports n == k.
