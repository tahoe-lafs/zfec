{
  description = "An efficient, portable erasure coding tool";

  inputs = {
    # Nix Inputs
    nixpkgs.url = "github:nixos/nixpkgs/?ref=nixos-26.05";
    nixpkgs-old.url = "github:nixos/nixpkgs/?ref=nixos-23.11";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      nixpkgs-old,
      flake-parts,
      haskell-flake,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      # Systems we support
      systems = [ "x86_64-linux" ];

      perSystem =
        { system, pkgs, ... }:
        let
          # Bring in the older python packages we need
          pkgsOld = nixpkgs-old.legacyPackages.${system};
          pkgs' = pkgs.extend (
            self: super: {
              python39Packages = pkgsOld.python39Packages;
              python39 = pkgsOld.python39;
              python310 = pkgsOld.python310;
              pypy39 = pkgsOld.pypy39;
            }
          );
          # Haskell utilities from haskell-flake
          hs = haskell-flake.lib { inherit pkgs'; };
        in
        {
          # Development shell – automatically picks up the Haskell project in this repo
          devShells.default =
            let
              hspkgs = pkgs'.haskell.packages.ghc9103;
            in
            pkgs'.mkShell {
              buildInputs = [
                hspkgs.cabal-install
                hspkgs.ghcid
                hspkgs.haskell-language-server

                pkgs'.gawk
                pkgs'.gnused

                # Python/PyPy matrix from the current channel.
                pkgs'.python311
                pkgs'.python312
                pkgs'.python313
                pkgs'.python314
                pkgs'.pypy310
                pkgs'.python3Packages.tox

                # Python/PyPy matrix extras from older channel.
                pkgs'.python39
                pkgs'.python310
                pkgs'.pypy39
              ];
            };

          # Expose the Haskell packages (empty placeholder for now)
          packages = { };
          checks = { };
          apps = { };
        };
    };
}
