# TODO: consider moving to build.rs

cd thirdparty
# save current path
export THIRD_PARTY_PATH=$(pwd)

echo "Downloading LLVM 18.1.6 to $THIRD_PARTY_PATH"
if [ -f llvmorg-18.1.6.tar.gz ]; then
    echo "File already exists"
else
    wget https://github.com/llvm/llvm-project/archive/refs/tags/llvmorg-18.1.6.tar.gz
fi
if [ -d llvm-project-llvmorg-18.1.6 ]; then
    echo "LLVM Directory already exists"
else
    tar -xvzf llvmorg-18.1.6.tar.gz 
fi

cd llvm-project-llvmorg-18.1.6/llvm
if [ -d build ]; then
    echo "Build directory already exists, removing it"
    rm -rf build
fi
mkdir build
cd build
cmake .. -DCMAKE_INSTALL_PREFIX=$THIRD_PARTY_PATH -DCMAKE_BUILD_TYPE=release -DLLVM_ENABLE_ASSERTIONS=ON
cmake --build . --target install -j 8