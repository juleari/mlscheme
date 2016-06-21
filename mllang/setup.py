from setuptools import setup, find_packages

setup(name='mlscheme',
      packages=find_packages(),
      entry_points = {
        'console_scripts': [
            'mlscheme = mlscheme.compiler:main',
        ]
      },
)