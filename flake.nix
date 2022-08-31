{
  description = "haskellgame";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskellNix.url = "github:input-output-hk/haskell.nix/c6b80fe119af19c9c40ffadd41579ff8db675aab";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    treefmt-flake.url = "github:srid/treefmt-flake";
  };

  outputs = { self, flake-parts, nixpkgs, haskellNix, treefmt-flake, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      systems = [ "x86_64-linux" ];
      imports = [
        treefmt-flake.flakeModule
      ];
      perSystem = { config, self', inputs', pkgs, system, ... }:
        let
          projectName = "haskellgame";
          xorgPackages = with pkgs.xorg; [
            pkgs.libGL libX11 libXcursor libXext libXi libXinerama libXrandr libXxf86vm
          ];
          overlays = [
            haskellNix.overlay
            (final: prev: {
              ${projectName} = final.haskell-nix.cabalProject {
                src = ./.;
                compiler-nix-name = "ghc923";
                shell.tools = {
                  cabal = { };
                  hlint = { };
                  ghcid = { };
                  # haskell-language-server = {};
                };
                modules = [
                  { packages.bindings-GLFW.components.library.libs = pkgs.lib.mkForce xorgPackages; }
                  { packages.GLFW-b.components.library.libs = pkgs.lib.mkForce xorgPackages; }
                  { packages.dear-imgui.components.library.libs = pkgs.lib.mkForce xorgPackages; }
                ];
              };
            })
          ];
          pkgs = import nixpkgs { inherit system overlays; };
          haskellNixFlake = pkgs.${projectName}.flake { };
        in
        pkgs.lib.recursiveUpdate
          (builtins.removeAttrs haskellNixFlake [ "devShell" ]) # remove devShell as it's not supported by flake-parts flake
          {
            treefmt.formatters = {
              inherit (pkgs) nixpkgs-fmt;
              inherit (pkgs.haskellPackages)
                cabal-fmt
                fourmolu;
            };
            packages.default = haskellNixFlake.packages."${projectName}:exe:haskellgame-cli";
            devShells.default = haskellNixFlake.devShell;
          };
    };
}
