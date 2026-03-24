{
  description = "An efficient, portable erasure coding tool";

  inputs = {
    # Nix Inputs
    nixpkgs.url = github:nixos/nixpkgs/?ref=nixos-25.11;
    nixpkgs-old.url = github:nixos/nixpkgs/?ref=nixos-23.11;
    flake-utils.url = github:numtide/flake-utils;
    hs-flake-utils.url = "git+https://whetstone.private.storage/jcalderone/hs-flake-utils.git?ref=main";
    hs-flake-utils.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    nixpkgs-old,
    flake-utils,
    hs-flake-utils,
  }: let
    ulib = flake-utils.lib;
  in
    ulib.eachSystem ["x86_64-linux"] (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      oldpkgs = nixpkgs-old.legacyPackages.${system};
      hsPkgs = pkgs.haskell.packages.ghc9103;
      hslib = hs-flake-utils.lib {
        pkgs = pkgs;
        src = ./.;
        compilerVersion = "ghc9103";
        packageName = "fec";
      };
    in {
      checks = hslib.checks {};

      devShells.default = hsPkgs.shellFor {
        packages = p: [];
        buildInputs = [
          hsPkgs.cabal-install
          hsPkgs.ghcid
          hsPkgs.haskell-language-server
          pkgs.gawk
          pkgs.gnused

          # Python/PyPy matrix (current channel)
          pkgs.python311
          pkgs.python312
          pkgs.python313
          pkgs.pypy310

          # Python/PyPy matrix extras from older channel
          oldpkgs.python39
          oldpkgs.python310
          oldpkgs.pypy39

          # Extra CI/release tools
          pkgs.python3Packages.build
          pkgs.twine
          pkgs.python3Packages.twisted
          pkgs.python3Packages.tox
        ];
      };

      packages = hslib.packages {};
      apps = {
        hlint = hslib.apps.hlint {argv = ["haskell/"];};
        cabal-test = hslib.apps.cabal-test {
          extraRuntimeInputs = pkgs: [
            # Some build-time dependencies of old-time, a transitive
            # dependency of ours...
            pkgs.gnused
            pkgs.gawk
          ];
          testTargetName = "test:tests";
        };
      };
    });
}
