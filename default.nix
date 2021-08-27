{ haskell-nix ? (import ./nix/pkgs.nix).haskell-nix
, withProfiling ? false
}:
let
  this =
    haskell-nix.project
      {
        src = haskell-nix.cleanSourceHaskell {
          src = ./.;
          name = "mediabus-rtp";
        };
        projectFileName = "cabal.project";
        compiler-nix-name = "ghc8105";
        pkg-def-extras = [];
        modules =
          [
            {
              packages.mediabus.components.tests.tests.build-tools = [
              # HACK make 'cabal test' work
              # https://github.com/input-output-hk/haskell.nix/issues/231#issuecomment-731699727
                this.hsPkgs.hspec-discover
              # END OF HACK
              ];
              packages.mediabus-rtp.components.library = {
                enableExecutableProfiling = withProfiling;
                enableLibraryProfiling = withProfiling;
                ghcOptions = if withProfiling then ["-fprof-auto"] else [];
              };
              packages.mediabus-rtp.components.exes.mediabus-demo-rtp-alaw-player = {
                enableExecutableProfiling = withProfiling;
                ghcOptions = if withProfiling then ["-fprof-auto"] else [];
              };
              packages.mediabus-rtp.components.tests.tests = {
                enableExecutableProfiling = withProfiling;
                ghcOptions = if withProfiling then ["-fprof-auto"] else [];
                # HACK make 'cabal test' work
                # https://github.com/input-output-hk/haskell.nix/issues/231#issuecomment-731699727
                build-tools = [
                  this.hsPkgs.hspec-discover
                ];
                # END OF HACK
              };
            }
          ];
      };
in
this
