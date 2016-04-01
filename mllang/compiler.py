#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys
import os
import re

PATH_COMPILE = 'compile.scm'
PATH_BUILD   = 'build.scm'
PATH_TESTS   = 'tests.scm'
RE_INCLUE    = '\(import \"([a-z]+.scm)\"\)'

REPLACE_PATH     = '###PATH-TO-INPUT-FILE###'
REPLACE_TESTS_IN = '###TESTS-INPUT###'
REPLACE_TESTS_OUT= '###TESTS-OUTPUT###'

PATH_TESTS_IN  = '../tests/in'
PATH_TESTS_OUT = '../tests/out'

def get_file_inner(path):
    return open(path).read()

def get_files_inner(files):
    return map(get_file_inner, files)

def module_concat(compile_):
    return False

def global_concat(compile_):
    files = get_files_inner(re.findall(RE_INCLUE, compile_))
    files.append(compile_)
    return '\n\n'.join(files)

def get_normal_path(path):
    return os.path.normpath(os.path.join(os.getcwd(), path))

def get_compile(path):
    return get_file_inner(PATH_COMPILE).replace(REPLACE_PATH, '\"%s\"' % path)

def get_tests_list(list_dir, path_in=PATH_TESTS_IN):
    return ' '.join(map(
                lambda x: get_normal_path(
                            os.path.join(os.getcwd(), list_dir, x)), 
                os.listdir(path_in)
            ))

def get_tests():
    tests_file = get_file_inner(PATH_TESTS)
    return tests_file.replace(REPLACE_TESTS_IN, get_tests_list(PATH_TESTS_IN))\
                     .replace(REPLACE_TESTS_OUT, get_tests_list(PATH_TESTS_OUT))

def run_tests(compile_):
    tests = get_tests()
    files = get_files_inner(re.findall(RE_INCLUE, compile_))
    files.append(tests)
    return '\n\n'.join(files)

KEYS = {
    '-g': global_concat,
    '-m': module_concat,
    '-t': run_tests
}

def main():
    type = sys.argv[1]
    path_to_file = get_normal_path(sys.argv[2])

    path_build = PATH_BUILD if len(sys.argv) == 3 else sys.argv[3]

    compile_ = get_compile(path_to_file)

    build = open(path_build, 'w')
    build.write(KEYS[ type ](compile_))

if __name__ == '__main__':
    main()