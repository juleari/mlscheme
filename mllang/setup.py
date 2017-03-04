from setuptools import setup, find_packages

setup(name='mlscheme',
      packages=find_packages(),
      include_package_data=True,
      entry_points = {
        'console_scripts': [
            'mlscheme = mlscheme.compiler:main',
        ]
      },
)
