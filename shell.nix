let

  pkgs = import <nixpkgs> { };
  hp = pkgs.haskellPackages;

  normal = with pkgs; [
      (hp.ghcWithPackages (hpkgs: [
        hpkgs.pretty-simple
      ]))
      pkgs.graphviz
    ];

in pkgs.mkShell {
  buildInputs = normal;
}
