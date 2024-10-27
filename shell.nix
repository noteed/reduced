let

  pkgs = import <nixpkgs> { };
  hp = pkgs.haskellPackages;

  normal = with pkgs; [
      (hp.ghcWithPackages (hpkgs: [
      ]))
      pkgs.graphviz
    ];

in pkgs.mkShell {
  buildInputs = normal;
}
