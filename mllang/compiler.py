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

DEFAULT_IN     = '../examples/sample1.sm'
DEFAULT_OUT    = 'build/compile.scm'
DEFAULT_MODULE = 'build/modules'
DEFAULT_TESTS  = 'build/tests.scm'

ARG_INDEX = 2

class Arg(object):
    """docstring for Arg"""
    def __init__(self, name, default, many=''):
        super(Arg, self).__init__()
        self.name = name
        self.value = default
        self.many = many

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
    '-g': 'compile_file',
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

    def run(self):
        call_str = '(%s)' % ', '.join(['"' + a.value + '"' for a in self.args])
        eval(self.func + call_str)

    def __str__(self):
        args = ' '.join([a.name for a in self.args])
        return '%s %s: %s' % (self.name, args, self.desc)

def get_normal_path(path):
    return os.path.normpath(os.path.join(os.getcwd(), path))

def get_file_inner(path):
    return open(path).read()

def get_files_inner(files):
    return map(get_file_inner, files)

def get_compile(path):
    return get_file_inner(PATH_COMPILE).replace(REPLACE_PATH, '\"%s\"' % path)

def global_concat(compile_):
    files = get_files_inner(re.findall(RE_INCLUE, compile_))
    files.append(compile_)
    return '\n\n'.join(files)

def compile_file(path_in, path_out):
    compile_ = get_compile( get_normal_path(path_in) )
    output = open(path_out, 'w')

    output.write( global_concat(compile_) )

def module_concat(compile_):
    return False

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
    print 'usage: ./compiler.py <key> <args>\n' +\
          '\n'.join(['                     %s %s: %s' % (
                key,
                ' '.join(['<' + a.name + '>' for a in ARGS[key]]),
                DESCS[key]
            ) for key in KEYS])

def run_tests(compile_):
    tests = get_tests()
    files = get_files_inner(re.findall(RE_INCLUE, compile_))
    files.append(tests)
    return '\n\n'.join(files)

KEYS = ['-h', '-g', '-m', '-t']

def main():
    argv = sys.argv
    num_of_args = len(argv)

    type = '' if num_of_args == 1 else argv[1]

    if type in KEYS:
        compiler = CompilerType(type)
        for i in range(ARG_INDEX, num_of_args):
            compiler.args[i - ARG_INDEX].value = argv[i]

        compiler.run()
    else:
        print_usage()

if __name__ == '__main__':
    main();