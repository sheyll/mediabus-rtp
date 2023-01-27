{
  description = "RTP decoder for mediabus";
  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    # Flake for better Haskell builds
    # For updating to a newer fersion comment this line
    haskellNix.url = "github:input-output-hk/haskell.nix";
    # --------------------------------
    # This is required for ./shell.nix
    # And the ./shell.nix is required for the vscode
    # nix-environment-selector plugin
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
    # --------------------------------
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = {flake-utils, nixpkgs, haskellNix, ...}:
   flake-utils.lib.eachSystem [flake-utils.lib.system.x86_64-linux] (system:
        let
          # Set the line below to 'true' to enable profiling builds
          withProfiling = false;

          pkgs = import nixpkgs {
            inherit system overlays;
            inherit (haskellNix) config;
          };
          overlays = [haskellNix.overlay (final: prev:
            {
              mediabus-rtp = final.haskell-nix.project {
                src = final.haskell-nix.cleanSourceHaskell {
                  src = ./.;
                  name = "mediabus-rtp";
                };
                projectFileName = "cabal.project";
                compiler-nix-name = "ghc944"; # "ghc925";
                pkg-def-extras = [];
                modules =
                  [
                    {
                      packages.mediabus.components.tests.tests.build-tools = [
                      # HACK make 'cabal test' work
                      # https://github.com/input-output-hk/haskell.nix/issues/231#issuecomment-731699727
                        final.mediabus-rtp.hsPkgs.hspec-discover
                      # END OF HACK
                      ];
                      packages.mediabus-rtp.components.library = {
                        enableLibraryProfiling = withProfiling;
                        ghcOptions = if withProfiling then ["-fprof-auto"] else [];
                      };
                      packages.mediabus-rtp.components.exes.mediabus-rtp-demo = {
                        enableProfiling = withProfiling;
                        ghcOptions = if withProfiling then ["-fprof-auto"] else [];
                      };
                      packages.mediabus-rtp.components.tests.tests = {
                        enableProfiling = withProfiling;
                        ghcOptions = if withProfiling then ["-fprof-auto"] else [];
                        # HACK make 'cabal test' work
                        # https://github.com/input-output-hk/haskell.nix/issues/231#issuecomment-731699727
                        build-tools = [
                          final.mediabus-rtp.hsPkgs.hspec-discover
                        ];
                        # END OF HACK
                      };
                    }
                  ];
                 shell.tools = {
                    cabal = {};
                    hlint = {};
                    ormolu = {};
                    haskell-language-server = {};
                  };
                 # Non-Haskell shell tools go here
                 shell.buildInputs = with pkgs; [
                    alejandra
                    sox
                    gst_all_1.gstreamer
                    gst_all_1.gst-plugins-good
                    gst_all_1.gst-plugins-base
                 ];
              };
            })];
        in
        pkgs.mediabus-rtp.flake {}
    );
}
