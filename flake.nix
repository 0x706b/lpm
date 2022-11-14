{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }@inputs:
  flake-utils.lib.eachSystem [ "x86_64-darwin" "x86_64-linux" ] (system:
    let
      pkgs = import nixpkgs { inherit system; };
      compilerVersion = "924";
      compiler = pkgs.haskell.packages."ghc${compilerVersion}";
      addBuildTools = pkgs.haskell.lib.addBuildTools;
      buildTools = with compiler;
        [ cabal-install
          pkgs.zlib
        ];
    in rec {
      project = devTools: compiler.developPackage {
        root = ./.;
        overrides = self: super: {
          semver = self.callPackage ./semver.nix {};
        };
        modifier = drv: addBuildTools drv (buildTools ++ devTools);
        returnShellEnv = !(devTools == [ ]);
      };

      packages.default = project [ ];

      devShell = project (with compiler;
        [ haskell-language-server
         cabal2nix
        ]
      );
    }
  );
}
