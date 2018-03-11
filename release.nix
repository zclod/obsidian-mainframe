let config = {
    packageOverrides = pkgs: rec {
        haskellPackages = pkgs.haskellPackages.override {
            overrides = self: super: rec {

                obsidian-mainframe = self.callCabal2nix "obsidian-mainframe" ./obsidian-mainframe.cabal {};

            };
        };
    };
};

pkgs = import <nixpkgs> {inherit config;};

in {
    obsidian-mainframe = pkgs.haskellPackages.obsidian-mainframe;
}
