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

DEFAULT_GENERAL = '../examples/sample1.sm'
DEFAULT_MODULES = 'build/modules'
DEFAULT_BUILD   = 'build/compile.scm'
DEFAULT_TESTS   = 'build/tests.scm'

class Arg(object):
    """docstring for Arg"""
    def __init__(self, name, default):
        super(Arg, self).__init__()
        self.name = name
        self.default = default

DESCS = {
    '-g': 'compile path_in file to path_out',
    '-h': 'help',
    '-m': 'make modules',
    '-t': 'run tests'
}
ARGS = {
    '-g': [ Arg('path_in', DEFAULT_IN),
            Arg('path_out', DEFAULT_OUT) ],
    '-h': [],
    '-m': [ Arg('module_name', DEFAULT_MODULE, '*') ],
    '-t': []
}

FUNCS = {
    '-g': 'global_concat',
    '-h': 'print_usage',
    '-m': 'module_concat',
    '-t': 'run_tests'
}

class CompilerType(object):
    """description, args, func for CompilerType"""
    def __init__(self, name):
        super(CompilerType, self).__init__()
        self.name = name
        self.desc = DESCS[name]
        self.args = ARGS[name]
        self.func = FUNCS[name]

    def run(self, args):
        call_str = '(%s)' % ', '.join(args)
        eval(self.func + call_str)

    def __str__(self):
        args = ' '.join([a.name for a in self.args])
        return '%s %s: %s' % (self.name, args, self.desc)

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

def print_usage():
    print './compiler.py key args\n\n'\
          'keys:\n' +\
          '\n'.join['%s %s: %s' % (
                key,
                ' '.join([a.name for a in ARGS[key]]),
                DESCS[key]
            ) for key in KEYS]

def run_tests(compile_):
    tests = get_tests()
    files = get_files_inner(re.findall(RE_INCLUE, compile_))
    files.append(tests)
    return '\n\n'.join(files)

KEYS = ['-g', '-m', '-t', '-h']

def main():
    type = sys.argv[1]

    if type in KEYS:
        compiler = CompilerType(type)
    else:
        print_usage()
"""
    path_to_file = get_normal_path(sys.argv[2])

    path_build = path_to_file if type == '-t' else \
                 PATH_BUILD if len(sys.argv) == 3 else sys.argv[3]

    compile_ = get_compile(path_to_file)

    build = open(path_build, 'w')
    if type in KEYS:
        build.write(KEYS[ type ](compile_))
    else:
        print_usage()
"""
if __name__ == '__main__':
    main()