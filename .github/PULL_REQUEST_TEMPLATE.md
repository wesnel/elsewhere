# Description of changes

<!--
In this section, please write a brief summary of the changes in this pull request.
-->

---
# Things done

<!--
Please check what applies. Note that these are not hard requirements, but instead merely serve as information for reviewers and reminders to yourself.
-->

## Legal

- [ ] Ensured that all work in this pull request satisfies the license in `LICENSE.txt`.

## Git

- [ ] Made each commit a logically separate changeset.
- [ ] Wrote commit messages that follow the style guide recommended in the `Commit Guidelines` section of [this chapter](https://git-scm.com/book/en/v2/Distributed-Git-Contributing-to-a-Project) of the Pro Git book.

## Code

- [ ] Used `rx` instead of strings for any regular expressions in `elsewhere.el`.

## Documentation

- [ ] Added documentation strings to any new functions in `elsewhere.el`.

If the pull request adds additional support for a new version control back end or website, then edit the following pieces of documentation:

- [ ] Add this new version control back end to the list of supported systems under the `Currently-Supported VC Back Ends` header of `README.md`.
- [ ] Regenerate the table of contents in `README.md` using the [`markdown-toc`](https://github.com/ardumont/markdown-toc) Emacs package.
- [ ] Add this new version control back end to the list of supported systems under the `version-control` block of `.github/ISSUE_TEMPLATE/BUG.yml`.

## Testing

- [ ] Added one or more test to `elsewhere-test.el` for any new code.

## Version numbers

<!--
This "Version numbers" section is mostly for maintainers.
-->

If this pull request should result in a new version-numbered release, the version numbers in the code should first be updated by this pull request.

The version numbering scheme should follow [semantic versioning](https://semver.org/spec/v2.0.0.html).

- [ ] Updated version number in the `Version` key on line 11 of `elsewhere.el`.
- [ ] Updated version number in the `elsewhere-version` variable on line 40 of `elsewhere.el`.
- [ ] Updated version number in the `Version` key on line 11 of `elsewhere-test.el`.
- [ ] Updated version number for the `elsewhere` dependency on line 12 of `elsewhere-test.el`.

## Changelog

- [ ] Add one or more bullet points under the `[Unreleased]` level 2 header on line 8 of `CHANGELOG.md` containing a brief summary of your changes.

  Each of these bullet points should be underneath a level 3 header of `Added`, `Removed`, `Changed`, or `Fixed` --- whichever most appropriately categorizes that bullet point.

<!--
The remainder of this "Changelog" section is mostly for maintainers.
-->

If this pull request will result in a new version-numbered release (see the "Version numbers" section above), then also do the following:

- [ ] Add a new level 2 header to `CHANGELOG.md` just above the header of most recent previous version-numbered release.

  The header should follow the following format, where `version_number` is your new version number from the "Version numbers" section above:

        [${version_number}]

- [ ] Move all unreleased changes under the `[Unreleased]` header on line 8 of `CHANGELOG.md` to instead be under your new header from the previous step.
- [ ] Near the bottom of `CHANGELOG.md` underneath the line which begins with `[Unreleased]:`, add a line which follows the following format, where `version_number` is the same as the previous step and `previous_version_number` is the most recent version number prior to your new one:

        [${version_number}]: https://github.com/wesnel/elsewhere/compare/v${previous_version_number}...v${version_number}

- [ ] Near the bottom of `CHANGELOG.md`, edit the line which begins with `[Unreleased]:` to contain your new `version_number` from the previous step:

        [Unreleased]: https://github.com/wesnel/elsewhere/compare/v${version_number}...HEAD
