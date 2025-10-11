# flake.nix -- Emacs configuration..

{
  description = "Emacs configuration.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.inputs.nixpkgs-stable.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, emacs-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs {
            inherit system;
            overlays = [ emacs-overlay.overlays.default ];
          };
      in {
        packages = {
          kaimacs = pkgs.emacsWithPackagesFromPackageRequires {
            package = (pkgs.emacs-git-pgtk.override {
              withNativeCompilation = true;
              withWebP = true;
            }).overrideAttrs(old: rec {
              configureFlags = old.configureFlags ++ ["--without-compress-install"];
            });
            packageElisp = builtins.readFile ./.github/package-list.el;
            extraEmacsPackages = epkgs: with epkgs; [
              goto-chg # Somehow missing.
              treesit-grammars.with-all-grammars
            ];
          };
          kaimacs-config = pkgs.callPackage ./source.nix {};
        };
      });
}

# flake.nix ends here
