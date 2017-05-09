from ebb_lint.flake8 import EbbLint
from ebb_lint._version import get_versions


__version__ = get_versions()['version']
del get_versions


__all__ = (
    'EbbLint',
    '__version__',
)
