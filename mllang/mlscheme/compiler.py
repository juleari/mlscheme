#!/usr/bin/env python
# -*- coding: utf-8 -*-
from subprocess import call
import sys
import os
import re

PATH_COMPILE = 'compile.scm'
PATH_BUILD   = 'build.scm'
PATH_GENBASE = os.path.join(os.path.sep, os.path.dirname(os.path.abspath(__file__)), 'genbase.sm')
PATH_TESTS   = 'tests.scm'
RE_INCLUE    = '\(import \"([a-z]+.scm)\"\)'

REPLACE_PATH     = '###PATH-TO-INPUT-FILE###'
REPLACE_GEN      = '###PATH-TO-GEN-FILE###'
REPLACE_GENBASE  = '###PATH-TO-GENBASE-FILE###'
REPLACE_TESTS_IN = '###TESTS-INPUT###'
REPLACE_TESTS_OUT= '###TESTS-OUTPUT###'

PATH_TESTS_IN  = '../tests/in'
PATH_TESTS_OUT = '../tests/out'

DEFAULT_IN     = '../examples/1.sm'
DEFAULT_OUT_DIR= ''
DEFAULT_OUT    = 'build.scm'
DEFAULT_GEN    = 'gen.scm'
DEFAULT_MODULE = 'modules'
DEFAULT_TESTS  = 'tests.scm'
DEFAULT_ECHO   = 'guile'

ARG_INDEX = 2

class Arg(object):
    def __init__(self, name, default, many=''):
        super(Arg, self).__init__()
        self.name = name
        self.value = default
        self.many = many

DESCS = {
    '-g': 'generate compiler for path_in file to compiler_file to out_file',
    '-c': 'compile echo_program path_in to out_file',
    '-l': 'load compiled echo_program path_in',
    '-h': 'help',
    '-m': 'make modules',
    '-t': 'run tests'
}
ARGS = {
    '-g': [ Arg('path_in', DEFAULT_IN),
            Arg('compiler_file', DEFAULT_OUT),
            Arg('path_out', DEFAULT_GEN) ],
    '-c': [ Arg('echo_program', DEFAULT_ECHO),
            Arg('path_in', DEFAULT_IN),
            Arg('path_out', DEFAULT_GEN) ],
    '-l': [ Arg('echo_program', DEFAULT_ECHO),
            Arg('path_in', DEFAULT_IN),
            Arg('path_out', DEFAULT_GEN) ],
    '-h': [],
    '-m': [ Arg('module_name', DEFAULT_MODULE, '*') ],
    '-t': []
}
FUNCS = {
    '-g': 'generate_file',
    '-c': 'compile_file',
    '-l': 'load_file',
    '-h': 'print_usage',
    '-m': 'module_concat',
    '-t': 'run_tests'
}

class CompilerType(object):
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
    return open(os.path.join(os.path.sep, os.path.dirname(os.path.abspath(__file__)), path)).read()

def get_files_inner(files):
    return map(get_file_inner, files)

def get_compile(path_in, path_out):
    return get_file_inner(PATH_COMPILE).replace(REPLACE_PATH, '\"%s\"' % path_in)\
                                       .replace(REPLACE_GENBASE, '\"%s\"' % PATH_GENBASE)\
                                       .replace(REPLACE_GEN, '\"%s\"' % path_out)

def global_concat(compile_):
    files = get_files_inner(re.findall(RE_INCLUE, compile_))
    files.append(compile_)
    return '\n\n'.join(files)

def generate_file(path_in, path_comp, path_out):
    compile_ = get_compile( get_normal_path(path_in), get_normal_path(path_out) )
    output = open(path_comp, 'w')

    output.write( global_concat(compile_) )

def compile_file(echo_program, path_in, path_out):
    generate_file(path_in, DEFAULT_OUT, path_out)
    call(echo_program + " " + DEFAULT_OUT, shell=True)

def load_file(echo_program, path_in, path_out):
    generate_file(path_in, DEFAULT_OUT, path_out)
    call(echo_program + " " + DEFAULT_OUT, shell=True)
    call(echo_program + " -l " + path_out, shell=True)

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
    print 'usage: mlscheme <key> <args>\n' +\
          '\n'.join(['                %s %s: %s' % (
                key,
                ' '.join(['<' + a.name + '>' for a in ARGS[key]]),
                DESCS[key]
            ) for key in KEYS])

def run_tests(compile_):
    tests = get_tests()
    files = get_files_inner(re.findall(RE_INCLUE, compile_))
    files.append(tests)
    return '\n\n'.join(files)

KEYS = ['-h', '-g', '-c', '-l', '-m', '-t']

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
