# See obsidian://open?vault=STEM&file=Zettelkasten%2FBuilding%20Rummikub%20Solver%20on%20Windows
/c/ghcup/bin/stack.exe build
zip -r release/rummikubsolver-win64.zip \
  resources \
  .stack-work/install/xxx/bin/rummikubsolver.exe \
  /mingw64/libgcc_s_seh-1.dll \
  /mingw64/libglpk-40.dll \
  /mingw64/libgmp-10.dll \
  /mingw64/libwinpthread-1.dll
