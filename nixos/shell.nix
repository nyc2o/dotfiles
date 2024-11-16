{ system ? builtins.currentSystem, pkgs ? import <nixpkgs> { } }:

with pkgs;

stdenv.mkDerivation {
  name = "lisp-shell";

  nativeBuildInputs = [ ];

  buildInputs = [
    emacs
    (sbcl.overrideAttrs (old: {
      src = pkgs.fetchurl {
        url = "https://sourceforge.net/projects/sbcl/files/sbcl/2.4.10/sbcl-2.4.10-source.tar.bz2";
        sha256 = "sha256-zus5a2nSkT7uBIQcKva+ylw0LOFGTD/j5FPy3hDF4vg=";
      };
    }))
    cl-launch
    which
    rlwrap
    file
    getopt
    less
    curl
    git
    openssl
    sqlite
    libfixposix
  ];

  LD_LIBRARY_PATH = lib.strings.makeLibraryPath [ openssl sqlite ];
}
