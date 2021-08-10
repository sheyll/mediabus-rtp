{ haskell-nix ? (import ./nix/pkgs.nix).haskell-nix
, withProfiling ? false
}:
let this =
  haskell-nix.project
    {
      src = haskell-nix.cleanSourceHaskell {
        src = ./.;
        name = "mediabus-rtp";
      };
      projectFileName = "cabal.project";
      compiler-nix-name = "ghc8105";
      pkg-def-extras = [ ];
      modules =
        [
          {
            # HACK make 'cabal test' work
            # https://github.com/input-output-hk/haskell.nix/issues/231#issuecomment-731699727
            packages.mediabus-rtp.components.tests.tests.build-tools = [
              this.hsPkgs.hspec-discover
            ];
            packages.mediabus.components.tests.tests.build-tools = [
              this.hsPkgs.hspec-discover
            ];
            packages.mediabus-rtp.allComponent = {
              enableExecutableProfiling = withProfiling;
              enableLibraryProfiling = withProfiling;
            };

            # END OF HACK
          }
        ]
        ++
        (if withProfiling then
          [
            {
              packages.mediabus-rtp.package.ghcOptions = "-fprof-auto";
            }
          ] else [{ }]);
    };
in this

