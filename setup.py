from setuptools import setup
import re

with open('validtnt/__init__.py') as f:
    content = f.read()
    longdesc = re.match(r'^"""([\s\S]+?)"""', content).group(1).strip()
    with open('README.rst', 'w') as rdme:
        rdme.write(longdesc)
    version = re.search(r'__version__\s*=\s*"([^"]+)"', content).group(1)
del f, rdme

setup(
    name="validtnt",
    version=version,
    description="Parse and validate Typographic Number Theory proofs.",
    long_description=longdesc,
    url="https://github.com/Kenny2github/validtnt",
    author="Ken Hilton",
    license="MIT",
    classifiers=[
        'Development Status :: 3 - Alpha',
        'Intended Audience :: Developers',
        'Intended Audience :: Science/Research',
        'Topic :: Scientific/Engineering :: Mathematics',
        'Topic :: Software Development :: Interpreters',
        'License :: OSI Approved :: MIT License',
        'Programming Language :: Python :: 3.7'
    ],
    keywords='tnt validator logic',
    packages=['validtnt'],
    python_requires='>=3.7',
)
