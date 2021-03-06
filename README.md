# Speakerete

##BlueZ and PulseAudio setup
Follow the guide here for compiling pusleaudio5 and bluez5 for the pi: [link](https://www.raspberrypi.org/forums/viewtopic.php?f=29&t=87138)

##Cross compiling GHC 7.8.4
* Get glibc 2.14: [link] (http://verahill.blogspot.ie/2014/03/562-pulling-in-glibc-214-from-testing.html)
* O2 optimizations break, so use quick-cross instead of perf-cross
* [link](https://github.com/ku-fpg/raspberry-pi/wiki/GHC-Cross-Compiler-for-Raspberry-Pi)

##Cabal stuff
```
cabal --with-ghc=arm-unknown-linux-gnueabihf-ghc --with-ghc-pkg=arm-unknown-linux-gnueabihf-ghc-pkg --with-ld=arm-linux-gnueabihf-ld --with-strip=arm-linux-gnueabihf-strip install --only-dependencies
```
* If cabal can't install the library try this: [link](http://stackoverflow.com/questions/25765893/how-do-i-install-dependencies-when-cross-compiling-haskell-code)

*Had trouble installing pulse-simple library, try this:
```
./setup configure --with-ghc=arm-unknown-linux-gnueabihf-ghc --with-ghc-pkg=arm-unknown-linux-gnueabihf-ghc-pkg --with-ld=arm-linux-gnueabihf-ld --with-strip=arm-linux-gnueabihf-strip --hsc2hs-options=-x --extra-lib-dirs="/home/aj/raspberrypi/rootfs/usr/lib/arm-linux-gnueabihf/" --extra-include-dirs="/home/aj/raspberrypi/rootfs/usr/include/" --extra-include-dirs="/home/aj/raspberrypi/rootfs/usr/include/arm-linux-gnueabihf" --with-hsc2hs=arm-unknown-linux-gnueabihf-hsc2hs --with-gcc=arm-linux-gnueabihf-gcc
```
