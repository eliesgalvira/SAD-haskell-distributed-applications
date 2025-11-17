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

        # Test runner for TCPSegment tests
        testRunner = pkgs.stdenv.mkDerivation {
          name = "sad-tests";
          buildInputs = [ pkgs.makeWrapper ];
          buildCommand = ''
            mkdir -p $out/bin
            cat > $out/bin/sad-tests <<EOF
            #!${pkgs.bash}/bin/bash
            set -e
            SOURCE_DIR="${toString self}"
            WORK_DIR=\$(mktemp -d)
            trap "rm -rf \$WORK_DIR 2>/dev/null || true" EXIT
            cp -r "\$SOURCE_DIR"/* "\$WORK_DIR"/
            cd "\$WORK_DIR"
            cabal test codec-tcp-tests
            EOF
            chmod +x $out/bin/sad-tests
            wrapProgram $out/bin/sad-tests \
              --prefix PATH : ${pkgs.cabal-install}/bin:${ghcWithPackages}/bin
          '';
        };
      in
      {
        # The main package - this is what users will install/run
        packages.default = myProject;

        apps.default = {
          type = "app";
          program = "${testRunner}/bin/sad-tests";
        };
        
        # Development shell (replaces shell.nix)
        devShells.default = pkgs.mkShell {
          buildInputs = [ ghcWithPackages pkgs.cabal-install haskellPackages.haskell-language-server ];
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
