from setuptools import setup, find_packages

setup(name='testmlscheme',
      packages=find_packages(),
      entry_points = {
        'console_scripts': [
            'testmlscheme = mlscheme.compiler:main',
        ]
      },
)
