# Revision history for scidb-hquery

## 2.8.0.437 -- 2020-03-23

* Added the --wait-on-stdin (-W) option to resolve issue #1.  Removed the now unnecessary base-compat constraint given regex-1.0.*-r1.  Removed extra 'import Crypto.Hash' line in UtilsUnsafe.hs.  Corrected some small documentation issues.  Updated the scidb-hquery.cabal and stack.yaml files and added a stack.yaml file.  These changes did not change the primitive API of hquery.  Tested lightly with SciDB 19.11 community edition.

## 2.8.0.436 -- 2019-11-13

* Added a base-compat constraint so that regex always compiles and added tested-with to the cabal file. Changed the stack resolver to lts-14.14.  Small improvement in the documentation.

## 2.8.0.435 -- 2019-07-27

* Now works with stack.

## 2.8.0.434 -- 2019-07-12

* Improved README.md and description documentation.

## 2.8.0.433 -- 2019-07-11

* Added README.md to extra-source-files.

## 2.8.0.432 -- 2019-07-08

* Corrected problems discovered with cabal candidate package 2.8.0.431.

## 2.8.0.431 -- 2019-07-06

* Ready to be uploaded to hackage.

## 2.8.0.429 -- 2019-07-06

* First packaged version of SciDB hquery released to git and cabal.
