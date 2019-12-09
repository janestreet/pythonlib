from setuptools import setup

setup(
    name='ocaml',
    version='0.0.5',
    author='',
    author_email='',
    packages=['ocaml'],
    package_dir={'ocaml': '.'},
    package_data={'ocaml': ['sharedlib/ocaml.so', 'sharedlib/stdlib.cmi']},
    install_requires=['wurlitzer'],
)
