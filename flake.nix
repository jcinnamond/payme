{
  description = "My haskell application";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        haskellPackages = pkgs.haskell.packages.ghc982;
        packageName = "payme";
      in
      {
        packages.${packageName} = haskellPackages.callCabal2nix packageName self { };

        devShells.default = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            ghc
            haskell-language-server
            cabal-install
            cabal-fmt
            fourmolu
            hlint

            pkgs.postgresql
            pkgs.docker-client
          ];
          env =
            let
              dbname = "payme";
              dbuser = "payme";
              dbpass = "payme";

              testdbname = "paymetest";
              testdbuser = "paymetest";
              testdbpass = "paymetest";
              testdbport = "5433";
            in
            {
              PAYME_DB_CONNECTION = "host='localhost' dbname='${dbname}' user='${dbuser}' password='${dbpass}'";
              PAYME_DB_IDLE_TIME = 10;

              PAYME_TEST_DB_CONNECTION = "host='localhost' dbname='${testdbname}' user='${testdbuser}' password='${testdbpass}' port=${testdbport}";

              # Support postgres in docker
              POSTGRES_DB = "${dbname}";
              POSTGRES_USER = "${dbuser}";
              POSTGRES_PW = "${dbpass}";

              POSTGRES_TEST_DB = "${testdbname}";
              POSTGRES_TEST_USER = "${testdbuser}";
              POSTGRES_TEST_PW = "${testdbpass}";
            };
          inputsFrom = [ self.packages.${system}.${packageName}.env ];
        };
      }
    );
}
