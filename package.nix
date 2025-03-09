{ pkgs ? import <nixpkgs> {}, displayrUtils }:

pkgs.rPackages.buildRPackage {
  name = "verbs";
  version = displayrUtils.extractRVersion (builtins.readFile ./DESCRIPTION); 
  src = ./.;
  description = "A collection of operations/actions/"verbs" providing an intuitive interface for many common operations (e.g. Sum, First, Average, Maximum, ...) on various inputs/data structures. Created to simplify common user actions in Displayr with a wide variety of inputs.";
  propagatedBuildInputs = with pkgs.rPackages; [ 
    flipTime
    flipU
    flipFormat
    lubridate
    flipTransformations
  ];
}
