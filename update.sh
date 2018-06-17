sbcl --eval "(map nil 'ql-dist:clean (ql-dist:all-dists))"
apt-get update
apt-get upgrade
eix-sync
emerge -avuDU --with-bdeps=y @world
yum update
