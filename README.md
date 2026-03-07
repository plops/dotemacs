# Lisp setup for this repo

This repo includes `install-lisp.sh` to bootstrap Quicklisp and register local generator projects.

## What it does

1. Verifies required tools are available: `sbcl`, `git`, and either `wget` or `curl`.
2. Downloads `quicklisp.lisp` into this repo **only if it does not already exist**.
3. Installs Quicklisp to `~/quicklisp` **only if `~/quicklisp/setup.lisp` is missing**.
4. Appends a Quicklisp init block to `~/.sbclrc` **only once** (guarded by marker `;;; Added by dotemacs/install-lisp.sh`).
5. Ensures `~/quicklisp/local-projects` exists.
6. Clones `git@github.com:plops/cl-cpp-generator2.git` into `~/stage/cl-cpp-generator2` **only if that repo is missing**.
7. Links every matching directory `~/stage/cl-*-generator*/` into `~/quicklisp/local-projects`:
   - creates missing symlinks
   - keeps correct existing symlinks
   - skips existing non-symlink paths
   - skips symlinks that already exist but point elsewhere
8. Runs non-interactive SBCL quickloads:
   - `quicklisp-slime-helper`
   - `cl-py-generator` (if linked)
   - `cl-rust-generator` (if linked)
   - `cl-cpp-generator2` (if linked)

## Run

```bash
./install-lisp.sh
```

## Notes

- The script is designed to be rerun safely.
- It does not force-overwrite existing files/folders/symlinks.
- It logs each action with `[install-lisp] ...` so the run can be copied into documentation.
