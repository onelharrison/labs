"""A collection of functions for safely working with dirty data"""

from functools import reduce
from typing import (Any, Callable, Collection, Hashable, Iterable,
                    Mapping, Optional, Sequence, Union, overload)


def coalesce(
    *values: Iterable[Optional[Any]], default: Optional[Any] = None
) -> Optional[Any]:
    """Return the first non-None value"""
    return next((v for v in values if v is not None), default)


def safe_cast(
    value: Any, astype: Callable, default: Optional[Any] = None
) -> Optional[Any]:
    """Convert one type to another without raising errors"""
    try:
        return astype(value)
    except (TypeError, ValueError):
        pass

    return default


@overload
def safe_get(collection: Mapping, key: Hashable, default: Optional[Any] = None):
    pass


@overload
def safe_get(collection: Sequence, key: int, default: Optional[Any] = None):
    pass


def safe_get(collection, key, default = None):
    """Get values from a collection without raising errors"""
    # pylint:disable=isinstance-second-argument-not-valid-type

    if isinstance(collection, Mapping):
        return collection.get(key, default)

    if isinstance(collection, Sequence):
        try:
            return collection[key]
        except (IndexError, TypeError):
            pass

    return default


def dig(
    collection: Collection,
    *keys: Union[int, Hashable],
    default: Optional[Any] = None
) -> Optional[Any]:
    """Get values from a potentially nested collection without raising errors"""
    return reduce(lambda x, y: safe_get(x, y, default), keys, collection)
