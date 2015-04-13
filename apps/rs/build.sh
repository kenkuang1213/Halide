mkdir jni/prebuild/
cp /Volumes/android/trunk/out/target/product/shamu/system/lib/libRS.so jni/prebuilt/libRS.so
pushd ../.. && make -j8 && popd && g++ -I ../../include/ testrs.cpp -L ../../bin/ -lHalide -o testrs -g && LD_LIBRARY_PATH=../../bin DYLD_LIBRARY_PATH=../../bin HL_DEBUG_CODEGEN=4 WITH_RS=1 HL_TARGET=arm-32-android-armv7s-rs-debug ./testrs 2> rs-out
