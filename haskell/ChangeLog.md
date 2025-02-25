# Changelog for fec

## 1.1.1 (2025-02-25)

* `Codec.FEC.initialize` no longer exported but called internally from `Codec.FEC.fec`.

* All functions now correctly return IO instead of using `unsafePerformIO` internally.
 
* Major version bumped due to removal of `initialize`. 

## 0.2.0 (2023-10-06)

* Application code must now execute the `Codec.FEC.initialize` action at least
  once before using other `Codec.FEC` APIs.

* `Codec.FEC.fec`, `Codec.FEC.encode`, and `Codec.FEC.decode` are now thread-safe.

* `Codec.FEC.fec` now supports n == k.
