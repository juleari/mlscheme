# Install
```bash
git clone git@github.com:juleari/mlscheme.git
cd mlscheme/mllang
python setup.py build
sudo python setup.py install
```

# Usage
## build compiler to sm file
```bash
mlscheme -g <path_in> <path_compiler> <path_out>
mlscheme -g example.sm compiler.scm example.scm
```

## compile sm file to scm
```bash
mlscheme -c <echo_program> <path_in> <path_out>
mlscheme -c guile example.sm example.scm
mlscheme -c guile example.sm
mlscheme -c csi example.sm
```

## load sm file
```bash
mlscheme -l <echo_program> <path_in> <path_out>
mlscheme -l guile example.sm example.scm
mlscheme -l guile example.sm
mlscheme -l csi example.sm
```
