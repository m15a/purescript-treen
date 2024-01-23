{ mkSpagoDerivation
, esbuild
, nodejs
, purs
, purs-backend-es
, spago-unstable
}:

mkSpagoDerivation rec {
  pname = "treen";
  version = "0.0.1";
  src = ./.;
  nativeBuildInputs = [
    esbuild
    purs
    purs-backend-es
    spago-unstable
  ];
  checkInputs = [
    nodejs
  ];
  buildPhase = ''
  spago bundle --pure
  '';
  doCheck = true;
  checkPhase = ''
  spago test --pure
  '';
  installPhase = ''
  app_home=$out/share/${pname}-${version}
  mkdir -p $out/bin $app_home/bin
  for file in package.json bundle.js README.md LICENSE; do
      install -m644 "$file" -t $app_home
  done
  install -m755 bin/treen -t $app_home/bin
  ln -s $app_home/bin/treen $out/bin/treen
  '';
}
