mkdir jni/prebuild/
cp /Volumes/android/trunk/out/target/product/shamu/system/lib/libRS.so jni/prebuilt/libRS.so
pushd ../../build && make -j8 && popd && g++ -I ../../build/include/ testrs.cpp -L ../../build/lib/ -lHalide -o testrs -g && LD_LIBRARY_PATH=../../build/bin DYLD_LIBRARY_PATH=../../build/bin HL_DEBUG_CODEGEN=4 WITH_RS=1 HL_TARGET=arm-32-android-armv7s-rs-debug ./testrs 2> rs-out
