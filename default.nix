{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, bytestring-conversion
      , case-insensitive, hspec, hspec-wai, http-media, http-types, mtl
      , servant, servant-client, servant-server, stdenv, text, wai
      , wai-extra
      }:
      mkDerivation {
        pname = "hspec-wai-servant";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base bytestring bytestring-conversion case-insensitive hspec-wai
          http-media http-types servant servant-client text wai-extra
        ];
        testHaskellDepends = [
          base bytestring hspec mtl servant servant-server text wai
        ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
