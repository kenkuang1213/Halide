#include "rs_halide_generated.h"

#include <iostream>

extern "C" int halide_copy_to_host(void *, buffer_t *);

int main(int argc, char **argv) {
    const int width = 768;
    const int height = 1280;
    const int channels = 4;
    uint8_t input_image[width * height * channels];
    const int channels_stride = 1;  // chunky image
    for (int i = 0; i < width * height * channels; i++) {
        input_image[i] = i % 23;
    }

    uint8_t output_image[width * height * channels];

    buffer_t bt_input = { 0 };
    bt_input.host = &input_image[0];
    bt_input.host_dirty = true;
    bt_input.stride[0] = 4;
    bt_input.extent[0] = width;
    bt_input.stride[1] = 4 * width;
    bt_input.extent[1] = height;
    bt_input.stride[2] = 1;
    bt_input.extent[2] = channels;
    bt_input.elem_size = 1;

    buffer_t bt_output = { 0 };
    bt_output.host = &output_image[0];
    bt_output.stride[0] = 4;
    bt_output.extent[0] = width;
    bt_output.stride[1] = 4 * width;
    bt_output.extent[1] = height;
    bt_output.stride[2] = 1;
    bt_output.extent[2] = channels;
    bt_output.elem_size = 1;

    for (int i = 0; i < std::min(bt_input.extent[0], 10); i++) {
        for (int j = 0; j < std::min(bt_input.extent[1], 10); j++) {
            std::cout << " [";
            for (int k = 0; k < bt_input.extent[2]; k++) {
                std::cout.width(2);

                char buffer[33];
                sprintf(buffer, "%d", bt_input.host[i * bt_input.stride[0] +
                                                    j * bt_input.stride[1] +
                                                    k * bt_input.stride[2]]);
                std::cout << buffer;
            }
            std::cout << "]";
        }

        std::cout << std::endl;
    }

    int error = rs_halide_generated(&bt_input, &bt_output);
    if (error) {
        std::cout << "Halide returned error: " << error << std::endl;
    }

    if (bt_output.dev) {
        halide_copy_to_host(NULL, &bt_output);
    }

    std::cout << "---===---===---===---" << std::endl;

    for (int i = 0; i < std::min(bt_output.extent[0], 10); i++) {
        for (int j = 0; j < std::min(bt_output.extent[1], 10); j++) {
            std::cout << " [";
            for (int k = 0; k < bt_output.extent[2]; k++) {
                std::cout.width(2);

                char buffer[33];
                sprintf(buffer, "%d", bt_output.host[i * bt_output.stride[0] +
                                                     j * bt_output.stride[1] +
                                                     k * bt_output.stride[2]]);
                std::cout << buffer;
            }
            std::cout << "]";
        }

        std::cout << std::endl;
    }

    std::cout << "Done!" << std::endl;
}