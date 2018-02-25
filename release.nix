let config = {
    packageOverrides = pkgs: rec {
        haskellPackages = pkgs.haskellPackages.override {
            overrides = self: super: rec {

                obsidian-mainframe = self.callPackage ./default.nix {};

            };
        };
    };
};

pkgs = import <nixpkgs> {inherit config;};

in {
    obsidian-mainframe = pkgs.haskellPackages.obsidian-mainframe;
}
