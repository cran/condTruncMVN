
# condTruncMVN 0.0.3

## Major changes

- As **condTruncMVN** extends **condMVNorm**, **condMVNorm** is moved to
  DEPENDS.

- Changed downstream dependence on matrixNormal to v. \>= 0.1.0 and
  tmvtnorm(\>= 1.5) to reflect their changes

## Minor changes

- Updated links

- Fixes CRAN errors

  - Recompile News

  - There is no specific data that the package uses. Added Lazy Data:
    false to DESCRIPTION.

  <!-- https://stackoverflow.com/questions/66860659/lazydata-is-specified-without-a-data-directory-error-when-submitting-r-packa
  -->

  - Add mvtnorm to “Suggests”: Package is used as a reference in
    condtMVN.

- Minor documentation updates

# condTruncMVN 0.0.2

- This is a new submission.

- Added a `NEWS.md` file to track changes to the package.

- First Release of the Package

- Successfully passed windows check.

<!-- + Added lines of code in case `given.ind` or `X.given` is an empty vector.-->
