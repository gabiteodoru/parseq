from setuptools import setup, find_packages

with open("README.md", "r", encoding="utf-8") as fh:
    long_description = fh.read()

setup(
    name="gabiteodoru-parseq",
    version="0.1.3",
    author="Gabi Teodoru",
    author_email="gabiteodoru@gmail.com",
    description="ParseQ: Q Language to Python Translator with AI-Powered Disambiguation",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/gabiteodoru/parseq",
    packages=find_packages(),
    classifiers=[
        "Development Status :: 4 - Beta",
        "Intended Audience :: Developers",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
        "Programming Language :: Python :: 3.12",
        "Topic :: Software Development :: Compilers",
        "Topic :: Software Development :: Code Generators",
        "Topic :: Scientific/Engineering",
    ],
    python_requires=">=3.8",
    install_requires=[
        # No external dependencies - uses subprocess for Claude CLI
    ],
    package_data={
        "parseq": ["parseq.q", "parseq_ns.q", "q_operators.md"],
    },
    include_package_data=True,
    keywords="q kdb parseq compiler translator ai claude functional-programming",
)