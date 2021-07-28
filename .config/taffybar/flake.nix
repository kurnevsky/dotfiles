{
  inputs = {
    taffybar.url = github:taffybar/taffybar/master;
    flake-utils.url = github:numtide/flake-utils/master;
  };
  outputs = { self, flake-utils, taffybar, nixpkgs }:
    let
      overlay = final: prev: {
        haskellPackages = prev.haskellPackages.override (old: {
          overrides = prev.lib.composeExtensions (old.overrides or (_: _: {}))
          (hself: hsuper: {
            kurnevsky-taffybar =
              hself.callCabal2nix "kurnevsky-taffybar" (
                ./.
              ) { };
          });
        });
      };
    overlays = taffybar.overlays ++ [ overlay ];
  in flake-utils.lib.eachDefaultSystem (system:
  let pkgs = import nixpkgs { inherit system overlays; config.allowBroken = true; };
  in
  rec {
    devShell = pkgs.haskellPackages.shellFor {
      packages = p: [ p.kurnevsky-taffybar ];
    };
    defaultPackage = pkgs.haskellPackages.kurnevsky-taffybar;
  }) // { inherit overlay overlays; } ;
}
