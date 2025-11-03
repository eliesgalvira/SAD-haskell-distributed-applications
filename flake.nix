{
  description = "SAD Haskell Distributed Applications";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskell.packages.ghc96;
        
        # Build your project
        myProject = haskellPackages.callCabal2nix "SAD-haskell-distributed-applications" ./. {};
        
        # Development shell with HUnit, QuickCheck, SmallCheck
        ghcWithPackages = haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
          HUnit
          QuickCheck
          smallcheck
        ]);
      in
      {
        # The main package - this is what users will install/run
        packages.default = myProject;

        # Temporary default app: make `nix run` print Hello World
        apps.default = {
          type = "app";
          program = "${pkgs.writeShellScriptBin "sad-hello" ''echo Hello World''}/bin/sad-hello";
        };
        
        # Development shell (replaces shell.nix)
        devShells.default = pkgs.mkShell {
          buildInputs = [ ghcWithPackages pkgs.cabal-install ];
          shellHook = ''
            echo "=== Haskell Project Shell with HUnit ==="
            echo "GHC version: $(ghc --version | head -n1)"
            echo "HUnit preloaded—Test.HUnit import works in GHCi!"
            echo ""
            echo "Available commands:"
            echo "  nix build        # Build the project"
            echo "  nix run          # Run the executable"
            echo "  cabal test       # Run tests"
          '';
        };
      }
    );
}
