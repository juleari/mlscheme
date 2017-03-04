from setuptools import setup, find_packages
import mlscheme

setup(
    name='mlscheme',
    version=mlscheme.__version__,
    packages=find_packages(),
    include_package_data=True,
    entry_points = {
        'console_scripts': [
            'mlscheme = mlscheme.compiler:main',
        ]
    },
)
