{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
  };

  outputs =
    inputs@{
      flake-parts,
      nixpkgs,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];

      perSystem =
        { pkgs, system, ... }:
        let
          bisect_ppx_ng = pkgs.ocamlPackages.buildDunePackage rec {
            pname = "bisect_ppx_ng";
            version = "3.0.0";
            src = pkgs.fetchFromGitHub {
              owner = "Kakadu";
              repo = pname;
              rev = "${version}";
              hash = "sha256-OGXfXh/qQ6Mybv6kzQQB60iZ1pdYXCOh20tE+ptilEU=";
            };
            propagatedBuildInputs = with pkgs; [
                ocamlPackages.ppxlib
                ocamlPackages.cmdliner
            ];
          };
          logger = pkgs.ocamlPackages.callPackage ./logger.nix {};
          ppx_inline_test_nobase = pkgs.ocamlPackages.buildDunePackage rec {
            pname = "ppx_inline_test_nobase";
            version = "0.17.0.3";
            src = pkgs.fetchFromGitHub {
              owner = "Kakadu";
              repo = pname;
              rev = "v${version}";
              hash = "sha256-Bsw+jtPfONeiRoAGYhD3Ty9Pu+pUYmg94mGY3cCqqp4=";
            };
            propagatedBuildInputs = with pkgs; [
                ocamlPackages.ppxlib
                ocamlPackages.mtime
            ];
          };
          GT = pkgs.ocamlPackages.buildDunePackage rec {
            pname = "GT";
            version = "0.5.5";
            src = pkgs.fetchFromGitHub {
              owner = "PLTools";
              repo = pname;
              rev = "${version}";
              hash = "sha256-+zP6UHJizynOtPoTyTkUfyQpi5uXz8+jY0G3Jrwplbw=";
            };
            nativeBuildInputs = with pkgs; [
                ocamlPackages.camlp5
                m4
            ];
            propagatedBuildInputs = with pkgs; [
                bisect_ppx_ng
                ppx_inline_test_nobase
                logger
                ocamlPackages.camlp5
                ocamlPackages.dune-configurator
                ocamlPackages.ppxlib
                ocamlPackages.ocamlgraph
            ];
          };
          ostap = pkgs.ocamlPackages.buildDunePackage rec {
            pname = "ostap";
            version = "0.6.1";
            src = pkgs.fetchFromGitHub {
              owner = "PLTools";
              repo = pname;
              rev = "v${version}";
              hash = "sha256-wHmtW9CVipLVGHPelVW8G/gpxJz7jYM3XpXTRT5XfUY=";
            };
            nativeBuildInputs = with pkgs; [
                ocamlPackages.camlp5
            ];
            propagatedBuildInputs = with pkgs; [
              ocamlPackages.camlp5
              ocamlPackages.re
              GT
            ];
          };
        in
        {
          devShells.default = pkgs.mkShell rec {
            buildInputs = with pkgs; [
              pkg-config
              m4
              perl
              pcre2

              ocamlformat
            ];

            shelHook = ''
                eval $(opam env)
            '';

            LD_LIBRARY_PATH = "${pkgs.lib.makeLibraryPath buildInputs}";
          };
        };
    };
}
