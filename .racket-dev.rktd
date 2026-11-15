;; Gates for the racket-dev plugin's runner (hooks/gate.rkt) and pre-push hook.
;;
;;   racket <plugin>/hooks/gate.rkt . all      ; every gate, in this order
;;   racket <plugin>/hooks/gate.rkt . push     ; the subset the pre-push hook runs
;;   racket <plugin>/hooks/gate.rkt . test     ; one gate by name
;;
;; A command whose first word is `nix` runs as is; every other command is
;; prefixed by `shell`.  nix reads the git-tracked tree, so stage new files
;; before the check gate.

((shell "nix develop --command")
 (base-ref "origin/master")
 (gates
  (compile "bash -c 'git ls-files -z -- \"*.rkt\" | xargs -0 raco make -v'")
  (test    "raco test -x --timeout 120 -p web3")
  (docs    "raco setup --check-pkg-deps --unused-pkg-deps --pkgs web3")
  (check   "nix build -L --no-link"))
 (push-gates (compile test)))
