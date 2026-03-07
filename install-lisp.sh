#!/usr/bin/env bash
set -euo pipefail
shopt -s nullglob

QUICKLISP_URL="http://beta.quicklisp.org/quicklisp.lisp"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
QUICKLISP_BOOTSTRAP="${SCRIPT_DIR}/quicklisp.lisp"
QUICKLISP_HOME="${HOME}/quicklisp"
QUICKLISP_SETUP="${QUICKLISP_HOME}/setup.lisp"
LOCAL_PROJECTS_DIR="${QUICKLISP_HOME}/local-projects"
SBCLRC="${HOME}/.sbclrc"
SBCLRC_MARKER=";;; Added by dotemacs/install-lisp.sh"

log() {
  printf '[install-lisp] %s\n' "$*"
}

require_cmd() {
  if ! command -v "$1" >/dev/null 2>&1; then
    log "missing required command: $1"
    exit 1
  fi
}

require_cmd sbcl
require_cmd git
if command -v wget >/dev/null 2>&1; then
  DOWNLOADER="wget"
elif command -v curl >/dev/null 2>&1; then
  DOWNLOADER="curl"
else
  log "missing required downloader: wget or curl"
  exit 1
fi

if [[ -e "${QUICKLISP_BOOTSTRAP}" ]]; then
  log "keeping existing ${QUICKLISP_BOOTSTRAP}"
else
  log "downloading quicklisp bootstrap to ${QUICKLISP_BOOTSTRAP}"
  if [[ "${DOWNLOADER}" == "wget" ]]; then
    wget -O "${QUICKLISP_BOOTSTRAP}" "${QUICKLISP_URL}"
  else
    curl -fsSL -o "${QUICKLISP_BOOTSTRAP}" "${QUICKLISP_URL}"
  fi
fi

if [[ -f "${QUICKLISP_SETUP}" ]]; then
  log "quicklisp already installed at ${QUICKLISP_HOME}"
else
  log "installing quicklisp into ${QUICKLISP_HOME}"
  sbcl --non-interactive \
    --load "${QUICKLISP_BOOTSTRAP}" \
    --eval '(quicklisp-quickstart:install)'
fi

if [[ -f "${SBCLRC}" ]] && grep -Fq "${SBCLRC_MARKER}" "${SBCLRC}"; then
  log "sbcl init block already present in ${SBCLRC}"
elif [[ -f "${SBCLRC}" ]] && grep -Fq "quicklisp/setup.lisp" "${SBCLRC}"; then
  log "detected existing quicklisp init in ${SBCLRC}; leaving file unchanged"
else
  log "appending quicklisp init block to ${SBCLRC}"
  {
    echo
    echo "${SBCLRC_MARKER}"
    echo "#-quicklisp"
    echo "(let ((quicklisp-init (merge-pathnames \"quicklisp/setup.lisp\""
    echo "                                       (user-homedir-pathname))))"
    echo "  (when (probe-file quicklisp-init)"
    echo "    (load quicklisp-init)))"
  } >> "${SBCLRC}"
fi

log "ensuring quicklisp local-projects directory exists"
mkdir -p "${LOCAL_PROJECTS_DIR}"

if [[ -d "${HOME}/stage/cl-cpp-generator2/.git" ]]; then
  log "keeping existing ${HOME}/stage/cl-cpp-generator2"
else
  log "cloning git@github.com:plops/cl-cpp-generator2.git into ${HOME}/stage/cl-cpp-generator2"
  git -C "${HOME}/stage" clone git@github.com:plops/cl-cpp-generator2.git
fi

for src in "${HOME}"/stage/cl-*-generator*/; do
  [[ -d "${src}" ]] || continue
  name="$(basename "${src%/}")"
  dst="${LOCAL_PROJECTS_DIR}/${name}"
  if [[ -L "${dst}" ]]; then
    current_target="$(readlink "${dst}")"
    if [[ "${current_target}" == "${src%/}" ]] || [[ "${current_target}" == "${src}" ]]; then
      log "link already correct: ${dst} -> ${current_target}"
    else
      log "skipping ${dst}: symlink exists but points elsewhere (${current_target})"
    fi
  elif [[ -e "${dst}" ]]; then
    log "skipping ${dst}: existing file/folder is not a symlink"
  else
    ln -s "${src%/}" "${dst}"
    log "created symlink ${dst} -> ${src%/}"
  fi
done

log "quickloading quicklisp-slime-helper"
sbcl --non-interactive \
  --load "${QUICKLISP_SETUP}" \
  --eval '(ql:quickload "quicklisp-slime-helper")'

for system in cl-py-generator cl-rust-generator cl-cpp-generator2; do
  if [[ -e "${LOCAL_PROJECTS_DIR}/${system}" ]]; then
    log "quickloading ${system}"
    sbcl --non-interactive \
      --load "${QUICKLISP_SETUP}" \
      --eval '(ql:register-local-projects)' \
      --eval "(ql:quickload \"${system}\")"
  else
    log "skipping quickload for ${system}: ${LOCAL_PROJECTS_DIR}/${system} not found"
  fi
done

log "completed"
