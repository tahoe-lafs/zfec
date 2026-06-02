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
      imports = [ haskell-flake.flakeModule ];

      # I have been testing things only on x86_64-linux.  Hopefully
      # the other systems also should Just Work (tm).
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      perSystem =
        {
          system,
          pkgs,
          config,
          ...
        }:
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
          ghc = pkgs'.haskell.packages.ghc9103;
          python = pkgs'.python312;
          haskellFec = config.haskellProjects.default.outputs.packages.fec.package;
          haskellVersion = haskellFec.version;
          haskellSdist =
            pkgs'.runCommand "fec-${haskellVersion}-sdist"
              {
                nativeBuildInputs = [
                  ghc.cabal-install
                  ghc.ghc
                ];
              }
              ''
                cp -R ${self} source
                chmod -R u+w source
                cd source

                export HOME=$TMPDIR/home
                export CABAL_CONFIG=$TMPDIR/cabal/config
                mkdir -p "$HOME" "$(dirname "$CABAL_CONFIG")"
                touch "$CABAL_CONFIG"

                mkdir -p $out
                cabal sdist --output-dir=$out
              '';
          pythonZfec = python.pkgs.buildPythonPackage {
            pname = "zfec";
            version = "dev";
            pyproject = true;

            src = self;

            nativeBuildInputs = [ python.pkgs.setuptools ];
            propagatedBuildInputs = [ python.pkgs.pyutil ];
            nativeCheckInputs = [
              python.pkgs.hypothesis
              python.pkgs.twisted
            ];

            checkPhase = ''
              trial zfec
            '';

            pythonImportsCheck = [ "zfec" ];
          };
          pythonSdist =
            pkgs'.runCommand "zfec-python-sdist"
              {
                nativeBuildInputs = [
                  python
                  python.pkgs.setuptools
                ];
              }
              ''
                cp -R ${self} source
                chmod -R u+w source
                cd source

                mkdir -p $out
                python setup.py sdist --dist-dir $out
              '';
        in
        {
          # Haskell utilities via haskell-flake
          haskellProjects.default = {
            basePackages = ghc;

            # Automatically discover packages and expose them
            autoWire = [
              "packages"
              "checks"
              "apps"
            ];
          };

          # Development shell automatically picks up the Haskell
          # project in this repo.
          devShells.default = pkgs'.mkShell {
            buildInputs = [
              ghc.cabal-install
              ghc.ghcid
              ghc.haskell-language-server

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

          packages.haskellSdist = haskellSdist;
          packages.python = pythonZfec;
          packages.pythonWheel = pythonZfec.dist;
          packages.pythonSdist = pythonSdist;
          packages.default = pkgs'.runCommand "zfec-all" { } ''
            mkdir -p $out/dist
            ln -s ${haskellFec} $out/haskell
            ln -s ${pythonZfec} $out/python
            ln -s ${haskellSdist}/*.tar.gz $out/dist/
            ln -s ${pythonZfec.dist}/*.whl $out/dist/
            ln -s ${pythonSdist}/*.tar.gz $out/dist/

            {
              echo "zfec build artifacts"
              echo
              echo "Haskell package:"
              echo "  haskell -> ${haskellFec}"
              echo
              echo "Python package:"
              echo "  python -> ${pythonZfec}"
              echo
              echo "Distribution artifacts:"
              for artifact in $out/dist/*; do
                echo "  dist/$(basename "$artifact")"
              done
            } > $out/ARTIFACTS.txt
          '';

          apps = (config.haskellProjects.default.outputs.apps or { }) // {
            build-artifacts = {
              type = "app";
              meta.description = "Build zfec artifacts and print the manifest";
              program = "${
                pkgs'.writeShellApplication {
                  name = "build-artifacts";
                  runtimeInputs = [
                    pkgs'.coreutils
                    pkgs'.nix
                  ];
                  text = ''
                    nix build "$@"

                    if [ -f result/ARTIFACTS.txt ]; then
                      printf '\n'
                      cat result/ARTIFACTS.txt
                    else
                      printf '\nBuild finished, but result/ARTIFACTS.txt was not found.\n'
                    fi
                  '';
                }
              }/bin/build-artifacts";
            };
            hlint = {
              type = "app";
              meta.description = "Run hlint on the Haskell sources";
              program = "${
                pkgs'.writeShellApplication {
                  name = "hlint";
                  runtimeInputs = [ ghc.hlint ];
                  text = "hlint haskell/";
                }
              }/bin/hlint";
            };
            cabal-test = {
              type = "app";
              meta.description = "Run the Cabal test suite";
              program = "${
                pkgs'.writeShellApplication {
                  name = "cabal-test";
                  runtimeInputs = [
                    ghc.ghc
                    ghc.cabal-install
                  ];
                  text = "cabal test --enable-tests";
                }
              }/bin/cabal-test";
            };
          };
        };
    };
}
