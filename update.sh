sbcl --eval '(ql:update-dist "quicklisp")'
sbcl --eval '(ql:update-client)'
# i had an issue with swank 2.28 in quicklisp and 2.29.1 in gentoo
# i decided to remove the version coming with gentoo
# sbcl --eval '(ql:quickload "quicklisp-slime-helper")'
# sbcl --eval '(ql:uninstall "swank")'
# sudo emerge -c app-emacs/slime
sbcl --eval "(map nil 'ql-dist:clean (ql-dist:all-dists))"
apt-get update
apt-get upgrade
eix-sync
emerge -avuDU --with-bdeps=y @world
yum update
