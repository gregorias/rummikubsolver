# 🛠️ Developer documentation

This is a documentation file for Rummikub Solver's developers.

## Dev environment setup

This project requires the following tools:

- [Commitlint]
- [Lefthook]

Install lefthook:

```shell
lefthook install
```

## Release

A version release consists of producing the following artifacts:

1. A tagged commit with a version string (`vx.y.z`).
2. Uploading the build binaries to GitHub releases (for now, linux only).

To create a release, run the following steps:

1. Create a new tagged commit with `dev/bin/release-new-version`.
2. Build a GitHub release:
    1. Build a release for linux/amd64 with `build/build-linux-amd64.sh`.
    2. Build a release for Windows/amd64 with `build/build-win.sh`.
    3. Create a GitHub release:
       `gh release create v$VERSION release/rummikubsolver-linux-amd64.zip release/rummikubsolver-win64.zip`.

## ADRs

### On convenient distribution

I don't provide a convenient built distribution, because:

- On Windows, I don't want to bother researching how to do this.
- On Linux, the executable should be fine. Researching AppImage is probably not needed.
- On macOS, I don't want to pay $99 to be able to sign the package.

[Commitlint]: https://github.com/conventional-changelog/commitlint
[Lefthook]: https://github.com/evilmartians/lefthook
