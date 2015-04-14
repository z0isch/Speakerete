# Speakerete

##BlueZ and PulseAudio setup
Follow the guide here for compiling pusleaudio5 and bluez5 for the pi: [link](https://www.raspberrypi.org/forums/viewtopic.php?f=29&t=87138)

##Cross compiling GHC 7.8.4
[link](https://github.com/ku-fpg/raspberry-pi/wiki/GHC-Cross-Compiler-for-Raspberry-Pi)

##Cabal stuff
cabal --with-ghc=arm-unknown-linux-gnueabihf-ghc --with-ghc-pkg=arm-unknown-linux-gnueabihf-ghc-pkg --with-ld=arm-linux-gnueabihf-ld --with-strip=arm-linux-gnueabihf-strip install --only-dependencies
If cabal can't install the library try this: [link](http://stackoverflow.com/questions/25765893/how-do-i-install-dependencies-when-cross-compiling-haskell-code)
