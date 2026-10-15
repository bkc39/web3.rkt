{
  description = "web3 - Ethereum JSON-RPC client for Racket, tested against a pinned anvil";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    keccak-rkt = {
      url = "github:bkc39/keccak-rkt";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, keccak-rkt }:
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      version = "0.1";
    in
    {
      packages = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };

          # casemate (ours) and crypto (keccak-rkt's build dep) with their
          # non-distribution closure, as unpacked source trees so the
          # sandboxed build installs them offline.  Bump outputHash when
          # catalog versions or info.rkt deps change; the build prints it.
          racket-deps = pkgs.stdenvNoCC.mkDerivation {
            name = "web3-racket-deps";
            dontUnpack = true;
            nativeBuildInputs = [ pkgs.racket pkgs.cacert pkgs.git pkgs.unzip ];
            buildPhase = ''
              runHook preBuild
              export HOME=$TMPDIR/home
              export PLTUSERHOME=$TMPDIR/plt
              export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
              export GIT_SSL_CAINFO=$SSL_CERT_FILE
              mkdir -p "$PLTUSERHOME"
              raco pkg install --batch --auto --no-setup --scope user casemate crypto
              mapfile -t deps < <(racket -e \
                '(require pkg/lib)(for ([p (installed-pkg-names #:scope (quote user))]) (displayln p))')
              raco pkg archive "$TMPDIR/archive" "''${deps[@]}"
              mkdir -p "$out"
              for z in "$TMPDIR"/archive/pkgs/*.zip; do
                name="$(basename "$z" .zip)"
                mkdir -p "$out/$name"
                unzip -q "$z" -d "$out/$name"
              done
              runHook postBuild
            '';
            dontInstall = true;
            outputHashMode = "recursive";
            outputHashAlgo = "sha256";
            outputHash = "sha256-G/zX3GNRH5RK7ROzmwqKIG7Lwx0R/NapfsvpkK+hpFQ=";
          };

          web3 = pkgs.stdenv.mkDerivation {
            pname = "web3";
            inherit version;
            src = pkgs.lib.cleanSource ./.;
            nativeBuildInputs = [ pkgs.racket pkgs.foundry ];
            buildPhase = ''
              runHook preBuild
              export HOME=$TMPDIR/home
              export PLTUSERHOME=$TMPDIR/racket-home
              mkdir -p "$HOME" "$PLTUSERHOME"
              raco pkg install --batch --copy --no-docs --no-setup --scope user ${racket-deps}/*/
              # keccak-rkt compiles its C core at setup time.
              raco pkg install --batch --deps fail --no-setup --copy --scope user \
                --name keccak ${keccak-rkt}
              raco pkg install --batch --deps fail --no-setup --copy --scope user \
                --name web3 "$PWD"
              raco setup --no-docs --check-pkg-deps --unused-pkg-deps --pkgs keccak web3
              runHook postBuild
            '';
            doCheck = true;
            checkPhase = ''
              runHook preCheck
              export WEB3_ANVIL=${pkgs.foundry}/bin/anvil
              raco test -x -p web3
              runHook postCheck
            '';
            installPhase = ''
              runHook preInstall
              mkdir -p $out/share
              cp -r $PLTUSERHOME $out/share/racket-home
              runHook postInstall
            '';
          };
        in
        {
          default = web3;
          inherit web3 racket-deps;
        });

      checks = forAllSystems (system: {
        inherit (self.packages.${system}) web3;
      });

      devShells = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
          inherit (self.packages.${system}) racket-deps;
        in
        {
          default = pkgs.mkShell {
            buildInputs = [ pkgs.racket pkgs.foundry ];
            shellHook = ''
              export WEB3_ANVIL=${pkgs.foundry}/bin/anvil
              # Outside $PWD: raco pkg install --link rejects overlapping collects dirs.
              cache_root="''${XDG_CACHE_HOME:-$HOME/.cache}/web3-devshell"
              project_id=$(printf '%s' "$PWD" | ${pkgs.coreutils}/bin/sha256sum | cut -c1-12)
              export PLTUSERHOME="$cache_root/$project_id"
              mkdir -p "$PLTUSERHOME"
              stamp_key=$(${pkgs.coreutils}/bin/sha256sum info.rkt | cut -c1-12)
              deps_stamp="$PLTUSERHOME/.setup-$stamp_key-$(basename ${racket-deps})-$(basename ${keccak-rkt})"
              if [ ! -f "$deps_stamp" ]; then
                echo "Setting up web3 in $PLTUSERHOME (deps changed or first run)"
                rm -f "$PLTUSERHOME"/.setup-* 2>/dev/null || true
                raco pkg remove --batch --no-setup --scope user web3 keccak >/dev/null 2>&1 || true
                if raco pkg install --batch --copy --no-docs --no-setup --skip-installed --scope user \
                     ${racket-deps}/*/ \
                  && raco pkg install --batch --copy --deps fail --no-setup --scope user \
                     --name keccak ${keccak-rkt} \
                  && raco pkg install --batch --link --deps fail --no-setup --scope user \
                     --name web3 "$PWD" \
                  && raco setup --no-docs --check-pkg-deps --unused-pkg-deps --pkgs keccak web3; then
                  touch "$deps_stamp"
                  echo "Done. Try: raco test -x -p web3"
                else
                  echo "web3 setup FAILED; stamp not written, will retry next shell entry." >&2
                fi
              fi
            '';
          };
        });
    };
}
