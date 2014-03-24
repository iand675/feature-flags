set -x
set -e

if [ ! -e $HOME/installs/bin/ghc ]; then
	cd ~
	wget https://www.haskell.org/ghc/dist/7.8.1-rc2/ghc-7.8.0.20140228-x86_64-unknown-linux-deb7.tar.xz
	tar -xJf ghc-7.8.0.20140228-x86_64-unknown-linux-deb7.tar.xz
	mkdir ~/installs
	cd ghc-7.8.0.20140228
	./configure --prefix=$HOME/installs && make install
fi

if [ ! -e $HOME/.cabal/bin/cabal ]; then
	cd ~
	wget http://hackage.haskell.org/package/cabal-install-1.18.0.3/cabal-install-1.18.0.3.tar.gz
	tar -xzf cabal-install-1.18.0.3.tar.gz
	cd cabal-install-1.18.0.3
	sh ./bootstrap.sh
fi