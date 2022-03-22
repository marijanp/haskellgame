let
  sources = import ./nix/sources.nix { };

  haskellNix = import sources.haskellNix { };
  pkgs = import haskellNix.sources.nixpkgs-2111 haskellNix.nixpkgsArgs;

  xorgPackages = with pkgs.xorg; [
    pkgs.libGL libX11 libXcursor libXext libXi libXinerama libXrandr libXxf86vm
  ];

  haskellPkgs = pkgs.haskell-nix.cabalProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "haskellgame";
      src = ./.;
    };
    compiler-nix-name = "ghc8107";
    index-state = "2022-03-18T00:00:00Z";
    modules = [
      {
        packages.bindings-GLFW.components.library.libs = pkgs.lib.mkForce xorgPackages;
      }
      {
        packages.GLFW-b.components.library.libs = pkgs.lib.mkForce xorgPackages;
      }
      {
        packages.dear-imgui.components.library.libs = pkgs.lib.mkForce xorgPackages;
      }
    ];
  };
in
{
  exes = {
    haskellgame-cli = haskellPkgs.haskellgame.components.exes.haskellgame-cli;
  };

  dev-shell = haskellPkgs.shellFor {
    withHoogle = false;
    tools = {
      cabal = "latest";
      hlint = "latest";
      haskell-language-server = "latest";
    };

    buildInputs = with pkgs; [
      ghcid
    ];
    exactDeps = true;
  };
}
