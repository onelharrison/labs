from functools import reduce


def coalesce(*values, default=None):
    return next((v for v in values if v is not None), default)


def safe_cast(value, astype, default=None):
    try:
        return astype(value)
    except (TypeError, ValueError):
        pass

    return default


def safe_get(collection, key, default=None):
    if isinstance(collection, dict):
        return collection.get(key, default)
    elif isinstance(collection, list):
        try:
            return collection[key]
        except (IndexError, TypeError):
            pass

    return default


def dig(collection, *keys, default=None):
    return reduce(lambda x, y: safe_get(x, y, default), keys, collection)
