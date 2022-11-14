{ mkDerivation, attoparsec, base, criterion, deepseq, fetchgit
, hashable, lib, tasty, tasty-hunit, template-haskell, text
}:
mkDerivation {
  pname = "semver";
  version = "0.4.0.1";
  src = fetchgit {
    url = "https://github.com/brendanhay/semver.git";
    sha256 = "0xl4pnr042psnh3kxdvxf1616ymg9zs5yp1ix245x8d0bqynaz9w";
    rev = "540990acfd2d4aaffa17a8337cbfa9a38cb3f2aa";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    attoparsec base deepseq hashable template-haskell text
  ];
  testHaskellDepends = [ base tasty tasty-hunit text ];
  benchmarkHaskellDepends = [ base criterion text ];
  homepage = "https://github.com/brendanhay/semver";
  description = "Representation, manipulation, and de/serialisation of Semantic Versions";
  license = lib.licenses.mpl20;
}
