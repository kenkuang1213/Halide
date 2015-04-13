ndk-build && adb push libs/armeabi-v7a/rstest /mnt/sdcard/ && adb push libs/armeabi-v7a/libstlport_shared.so /mnt/sdcard/ && sh -c 'cat adb-test.txt | adb shell'
