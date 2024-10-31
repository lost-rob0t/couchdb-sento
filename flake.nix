{
  description = "A modern Couchdb 3.x Client. but with actors and agents. seeds for a service.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
  };

  outputs = { self, nixpkgs }:
  let
    pkgs = nixpkgs.legacyPackages.x86_64-linux;
  in {
    devShell.x86_64-linux =
      pkgs.mkShell {
        buildInputs = with pkgs; [
          pkg-config
          roswell
          sbcl
          zap # for inspecting requests

        ];
        shellHook = ''
              export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath([pkgs.openssl])}
            '';
      };
  };
}
