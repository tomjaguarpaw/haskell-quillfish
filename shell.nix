{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:

haskellPackages.cabal.mkDerivation (self: {
  pname = "quillfish";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  buildDepends = with haskellPackages; [ semigroups transformers ];
  buildTools = with haskellPackages; [ cabalInstall ];
  meta = {
    homepage = "https://github.com/tomjaguarpaw/haskell-quillfish";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
