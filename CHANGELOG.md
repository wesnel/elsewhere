# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [1.3.1]

### Added

* Added variable `elsewhere-version`.

### Changed

* Improved documentation.

## [1.3.0]

### Added

* Add ability to select a remote.

## [1.2.1]

### Changed

* Improve logic for selecting a Git revision.
* Use `rx` to build regexps.
* Improve tests for permalinks which contain Git commit hashes.
* Add error handling for if a Git remote is not configured.

## [1.2.0]

### Added

* Add support for Sourcehut

### Fixed

* Fix broken start and end arguments for `elsewhere-build-url`

## [1.1.0]

### Added

* Add headless? argument for `elsewhere-open`
* Add silent? argument for `elsewhere-build-url`
* Add headless? argument for `elsewhere-build-url`
* Add Eldev as the development tool for this package
* Add tests for `elsewhere-build-url`

### Changed

* Bump minimum Emacs version to 29.1
* Switch to external Git command for fetching the current revision
* Switch to `vc-responsible-backend` for fetching the VC backend

### Removed

* Remove interactive? argument for `elsewhere-open`
* Remove interactive? argument for `elsewhere-build-url`

## [1.0.0]

### Added

* Support `Git` backend from `vc-handled-backends`
* Support GitHub and GitLab
* Support generating URL in echo area
* Support opening generated URL in browser using `browse-url`
* Support choosing a revision for URLs using `completing-read`

[Unreleased]: https://github.com/wesnel/elsewhere/compare/v1.3.1...HEAD
[1.3.1]: https://github.com/wesnel/elsewhere/compare/v1.3.0...v1.3.1
[1.3.0]: https://github.com/wesnel/elsewhere/compare/v1.2.1...v1.3.0
[1.2.1]: https://github.com/wesnel/elsewhere/compare/v1.2.0...v1.2.1
[1.2.0]: https://github.com/wesnel/elsewhere/compare/v1.1.0...v1.2.0
[1.1.0]: https://github.com/wesnel/elsewhere/compare/v1.0.0...v1.1.0
[1.0.0]: https://github.com/wesnel/elsewhere/releases/tag/v1.0.0
