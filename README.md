# mlscheme
Функциональный язык программирования с динамической типизацией и ML-подобным синтаксисом

- dev -- разработка
- mllang -- текущая рабочая версия модулей
- examples -- примеры кода на языке

## Установка:
```bash
git clone https://github.com/juleari/mlscheme.git
cd mlscheme/mllang
sudo make install
```

## Компиляция программы
```bash
mlscheme <key> <args>
```

### Ключи
- -h -- help
```bash
mlscheme -h
```
- -g -- generate -- генерирует компилятор программы на языке, находящейся по адресу path_in, в программу на языке Scheme (по адресу path_out) в файл, находящийся по адресу path_compiler
```bash
mlscheme -g <path_in> <path_compiler> <path_out>
mlscheme -g example.sm compiler.scm example.scm
```

- -c -- compile -- компилирует программу на языке (по адресу path_in) в программу на языке Scheme (по адресу path_out) с помощью echo_program
```bash
mlscheme -c <echo_program> <path_in> <path_out>
mlscheme -c guile example.sm example.scm
mlscheme -c guile example.sm
mlscheme -c csi example.sm
```

- -l -- load -- выполняет программу на языке (по адресу path_in) с помощью echo_program (скомпилированная программа на языке Scheme будет по адресу path_out)
```bash
mlscheme -l <echo_program> <path_in>
mlscheme -l guile example.sm example.scm
mlscheme -l guile example.sm
mlscheme -l csi example.sm
```
