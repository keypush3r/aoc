let
  nixPkgs = import <nixpkgs> {
    overlays = [nixPkgsOverlay];
  };

  nixPkgsOverlay = (nixSelf: nixSuper: {
    myHaskellPackages = nixSelf.haskellPackages.override (oldHaskellPkgs: {
      overrides = nixSelf.lib.composeExtensions (oldHaskellPkgs.overrides or (_: _: {}))  haskellPkgsOverlay;
    });
  });

  haskellPkgsOverlay = (hSelf: hSuper: {
    # "myproject" is the first part of the "myproject.cabal" project definition file
    myProject = hSelf.callCabal2nix "myproject" ./. {};
  });
  
  devTools = with nixPkgs; [
    cabal-install 
    haskellPackages.ghcid
  ];

  myShellHook = ''
    alias repl="cabal new-repl"
  '';
in
nixPkgs.myHaskellPackages.myProject.env.overrideAttrs (oldEnv: {
  nativeBuildInputs = oldEnv.nativeBuildInputs ++ devTools;
  shellHook = myShellHook;
})
