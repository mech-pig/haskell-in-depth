let
  nixpkgs = builtins.fetchGit {
    name = "nixos-unstable-2022-11-23";
    url = "https://github.com/NixOS/nixpkgs/";
    ref = "refs/heads/nixos-unstable";
    rev = "2788904d26dda6cfa1921c5abb7a2466ffe3cb8c";
  };

  pkgs = import nixpkgs {};
in
  pkgs.mkShell {
    buildInputs = with pkgs; [
      stack
      haskell.compiler.ghc902
      haskell.packages.ghc902.haskell-language-server
    ];
    # https://www.tweag.io/blog/2022-06-02-haskell-stack-nix-shell/
    NIX_PATH = "nixpkgs=" + pkgs.path;
  }