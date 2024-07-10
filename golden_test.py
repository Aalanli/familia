import glob
import os
import sys
import subprocess
import tempfile
import io
import difflib

def green(s: str) -> str:
    return f'\033[92m{s}\033[0m'

def red(s: str) -> str:
    return f'\033[91m{s}\033[0m'


def diff_str(str1: str, str2: str) -> bool:
    diff = difflib.Differ()
    res = list(diff.compare(str1.splitlines(), str2.splitlines()))
    is_diff = False

    for line in res:
        if line.startswith('+ '):
            is_diff = True
            break
        elif line.startswith('- '):
            is_diff = True
            break
    
    if is_diff:
        for line in res:
            if line.startswith('+ '):
                print(green(line))
            elif line.startswith('- '):
                print(red(line))
            else:
                print(line)

    return not is_diff

def main(generate=False):
    test_files = glob.glob('tests/*.fm')
    for f in test_files:
        print(f)
        golden_llvm = f.replace('.fm', '.ll')
        golden_llvm_o1 = f.replace('.fm', '_O1.ll')
        golden_ir = f.replace('.fm', '.ir')
        if generate:
            subprocess.run(['./target/debug/familia', '-o', golden_llvm, f, 'dump-llvm'])
            subprocess.run(['./target/debug/familia', '-o', golden_llvm_o1, f, 'dump-llvm', 'o1'])
            subprocess.run(['./target/debug/familia', '-o', golden_ir, f, 'dump-ir'])
        else:
            s = subprocess.run(['./target/debug/familia', f, 'dump-llvm'], stdout=subprocess.PIPE).stdout
            with open(golden_llvm, 'r') as fl:
                golden = fl.read()
            if not diff_str(s.decode('utf-8')[:-1], golden):
                print('LLVM mismatch')
                sys.exit(1)
            s = subprocess.run(['./target/debug/familia', f, 'dump-llvm', 'o1'], stdout=subprocess.PIPE).stdout
            with open(golden_llvm_o1, 'r') as fl:
                golden = fl.read()
            if not diff_str(s.decode('utf-8')[:-1], golden):
                print('LLVM mismatch')
                sys.exit(1)
            s = subprocess.run(['./target/debug/familia', f, 'dump-ir'], stdout=subprocess.PIPE).stdout
            with open(golden_ir, 'r') as fl:
                golden = fl.read()
            if not diff_str(s.decode('utf-8')[:-1], golden):
                print('IR mismatch')
                sys.exit(1)
            
if __name__ == '__main__':
    generate = False
    if len(sys.argv) > 1 and sys.argv[1] == '--generate':
        generate = True
        print('Generating golden files:', generate)
    main(generate)
