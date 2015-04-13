LOCAL_PATH:= $(call my-dir)

# === RS ===

include $(CLEAR_VARS)

LOCAL_MODULE := RS
LOCAL_SRC_FILES := prebuilt/libRS.so

include $(PREBUILT_SHARED_LIBRARY)

# === rstest ===

include $(CLEAR_VARS)

LOCAL_MODULE           := rstest

LOCAL_SRC_FILES        := testrs-run.cpp

LOCAL_SHARED_LIBRARIES := RS
LOCAL_STATIC_LIBRARIES := android_native_app_glue
LOCAL_LDLIBS           := -lm -llog -landroid rs_halide_generated.o
LOCAL_ARM_MODE         := arm

LOCAL_CPPFLAGS += -std=c++11
LOCAL_LDFLAGS += -llog # -ldl

LOCAL_C_INCLUDES := ./

include $(BUILD_EXECUTABLE)

$(call import-module,android/native_app_glue)
