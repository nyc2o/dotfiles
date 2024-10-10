{ system ? builtins.currentSystem, pkgs ? import <nixpkgs> { } }:

with pkgs;

stdenv.mkDerivation {
  name = "lisp-shell";

  nativeBuildInputs = [ ];

  buildInputs = [
    emacs
    sbcl
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

  LD_LIBRARY_PATH = lib.strings.makeLibraryPath [ openssl sqlite];
}
