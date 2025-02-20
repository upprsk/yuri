# Yuri

Building for debug:

```bash
cmake -B build -S . -DCMAKE_BUILD_TYPE=Debug
cmake --build build
./build/cmd/yuric
```

Building for release:

```bash
cmake -B build -S .
cmake --build build
./build/cmd/yuric
```
