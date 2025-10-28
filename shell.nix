{ pkgs ? import <nixpkgs> {} }:

let
  # Use GHC 9.6.x packages ecosystem
  haskellPackages = pkgs.haskell.packages.ghc96;

  # Create GHC binary with HUnit preloaded in its package DB (for GHCi imports)
  ghcWithHUnit = haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
    HUnit  # Now available in GHCi: import Test.HUnit
    # Add more here later, e.g., QuickCheck, mtl
  ]);
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    # Core: Bundled GHC + HUnit
    ghcWithHUnit
    cabal-install  # For Cabal
    # ghcid          # For auto-reload
  ];

  # Runs when you enter the shell
  shellHook = ''
    echo "=== Haskell Project Shell with HUnit ==="
    echo "GHC version: $(ghc --version | head -n1)"
    echo "HUnit preloaded—Test.HUnit import works in GHCi!"
    echo "Verify: ghc-pkg list | grep HUnit"
    echo ""
    echo "To use:"
    echo "  ghci lab3/1-tipus.hs lab3/tests.hs  # Load code + tests (modules!)"
    echo "  runTestTT mainSuite                  # Run all tests"
    echo "  :q to exit GHCi; 'exit' to leave shell"
    echo ""
    echo "System unchanged—isolated env."
  '';
}
