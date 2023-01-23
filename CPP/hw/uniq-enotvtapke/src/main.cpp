#include <algorithm>
#include <cstring>
#include <fstream>
#include <iostream>
#include <string>

namespace {
    void print(const bool count, const std::size_t n, const std::string &line) {
        if (count) {
            std::cout << "      " << n << " ";
        }
        std::cout << line << "\n";
    }

    void uniq(std::istream &input, const bool count, const bool unique, const bool repeated) {
        std::string line;
        std::string prev_line;
        std::size_t n = 1;
        bool empty = input.peek() == EOF;
        std::getline(input, prev_line);
        while (std::getline(input, line)) {
            if (prev_line == line) {
                n += 1;
            } else {
                if (unique && !repeated && n == 1) {
                    print(count, n, prev_line);
                } else if (repeated && !unique && n != 1) {
                    print(count, n, prev_line);
                } else if (!repeated && !unique) {
                    print(count, n, prev_line);
                }
                n = 1;
            }
            prev_line = line;
        }
        if (!empty) {
            if (unique && !repeated && n == 1) {
                print(count, n, prev_line);
            } else if (repeated && !unique && n != 1) {
                print(count, n, prev_line);
            } else if (!repeated && !unique) {
                print(count, n, prev_line);
            }
        }
    }
} // anonymous namespace

int main(int argc, char **argv) {
    bool count = false;
    bool repeated = false;
    bool unique = false;
    const char *input_name = nullptr;
    for (int i = 1; i < argc; ++i) {
        if (argv[i][0] == '-') {
            if (argv[i][1] != '-') {
                const size_t len = std::strlen(argv[i]);
                for (size_t j = 1; j < len; ++j) {
                    switch (argv[i][j]) {
                        case 'c':
                            count = true;
                            break;
                        case 'u':
                            unique = true;
                            break;
                        case 'd':
                            repeated = true;
                            break;
                    }
                }
            } else {
                if (std::strcmp(argv[i], "--count") == 0) {
                    count = true;
                } else if (std::strcmp(argv[i], "--unique") == 0) {
                    unique = true;
                } else if (std::strcmp(argv[i], "--repeated") == 0) {
                    repeated = true;
                }
            }
        } else {
            input_name = argv[i];
        }
    }

    if (input_name != nullptr && std::strcmp(input_name, "-") != 0) {
        std::ifstream f(input_name);
        uniq(f, count, unique, repeated);
    } else {
        uniq(std::cin, count, unique, repeated);
    }
}