{
  description = "An efficient, portable erasure coding tool";

  inputs = {
    # Nix Inputs
    nixpkgs.url = github:nixos/nixpkgs/?ref=nixos-22.11;
    flake-utils.url = github:numtide/flake-utils;
    hs-flake-utils.url = "git+https://whetstone.private.storage/jcalderone/hs-flake-utils.git?ref=main";
    hs-flake-utils.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    hs-flake-utils,
  }: let
    ulib = flake-utils.lib;
  in
    ulib.eachSystem ["x86_64-linux"] (system: let
      hslib = hs-flake-utils.lib {
        pkgs = nixpkgs.legacyPackages.${system};
        src = ./.;
        compilerVersion = "ghc8107";
        packageName = "fec";
      };
    in {
      checks = hslib.checks {};
      devShells = hslib.devShells {};
      packages = hslib.packages {};
      apps = {
        hlint = hslib.apps.hlint {argv = ["haskell/"];};
        cabal-test = hslib.apps.cabal-test {
          preBuild = ''
            cabal get old-time-1.1.0.3
            cd old-time-1.1.0.3
            cabal build
          '';
          testTargetName = "test:tests";
        };
      };
    });
}
