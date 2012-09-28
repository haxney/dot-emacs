#!/bin/bash

# Transmutatify the geiser released tarball into an ELPA package!
# Run from within the root directory of the extracted tarball

GEISER_VERSION=0.2.1
GEISER_PKG_BASE=/tmp/geiser-$GEISER_VERSION
read -r -d '' GEISER_PKG_DESC <<EOF
(define-package "geiser" "$GEISER_VERSION"
    "GNU Emacs and Scheme talk to each other")
EOF

# 0. Clean out any old installation remnants
rm -rf /tmp/pth $GEISER_PKG_BASE{,tar}

# 1. Build and install the package to a temporary directory

./configure --prefix=/tmp/pth; make install

# 2. Prepare package target
cd /tmp/pth
mkdir -p $GEISER_PKG_BASE/bin

# 3. Copy over the "geiser-racket" executable, making it use a relative path
sed -e 's/top=.*/top="$(dirname $0)\/.."/' \
    < bin/geiser-racket \
    > $GEISER_PKG_BASE/bin/geiser-racket

# 4. Delete the install code; paths are set up through ELPA
rm share/emacs/site-lisp/geiser-install.el

# 5. Copy files to the package directory
cp share/emacs/site-lisp/*.el $GEISER_PKG_BASE
cp share/info/* $GEISER_PKG_BASE
cp -R share/geiser/* $GEISER_PKG_BASE

# 6. Correct paths in "geiser.el" and set up autoloads
read -r -d '' EDIT_SCRIPT <<'EOF'
(progn
  (defun autoloadize-things (thing)
    "Add an autoload token before each THING in the buffer."
    (goto-char (point-min))
    (while (search-forward thing nil t)
      (beginning-of-line)
      (insert ";;;###autoload\n")
      (forward-line 1)))

  (goto-char (point-min))
  (flush-lines "^(setq geiser")
  (flush-lines "^(add-to-list .load-path geiser-elisp-dir)")

  (search-forward "defvar geiser-elisp-dir")
  (kill-word 1)
  (just-one-space 1)
  (insert "(file-name-directory load-file-name)")

  (search-forward "defvar geiser-scheme-dir")
  (kill-word 1)
  (just-one-space 1)
  (insert "geiser-elisp-dir")

  (autoloadize-things "(add-to-list")
  (autoloadize-things "(mapc")
  (autoloadize-things "(autoload")
  (autoloadize-things "(add-hook")
  (basic-save-buffer))
EOF

emacs -Q --batch $GEISER_PKG_BASE/geiser.el --eval "$EDIT_SCRIPT"

# 7. Create the geiser-pkg.el file
echo $GEISER_PKG_DESC > $GEISER_PKG_BASE/geiser-pkg.el

# 8. Go to the directory above the geiser package basedir
cd $GEISER_PKG_BASE/../

# 9. Tar up the package
tar cf geiser-$GEISER_VERSION.tar geiser-$GEISER_VERSION

# 10. Install the package into Emacs
# M-x package-install-file RET /tmp/geiser-0.2.1.tar
