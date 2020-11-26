# Source from https://blog.witchoflight.com/2020/syntactic-unification/

from dataclasses import dataclass


@dataclass(frozen=True)
class Var:
    name: str

    def __repr__(self) -> str:
        return self.name


def unify(a, b, model={}):
    a = walk(model, a)
    b = walk(model, b)

    if a == b:
        return model

    # ... variable cases ...

    if isinstance(a, Var):
        if occurs(a, b, model):
            return None
        return {**model, a: b}

    if isinstance(b, Var):
        if occurs(a, b, model):
            return None
        return {**model, b: a}

    # ... tuple cases ...

    if isinstance(a, tuple) and isinstance(b, tuple):
        if len(a) != len(b):
            return None

        for ax, bx in zip(a, b):
            model = unify(ax, bx, model)
            if model is None:
                return None
        return model

    return None


def walk(model, term):
    while term in model:
        term = model[term]
    return term


def occurs(a, b, model):
    b = walk(model, b)
    if a == b:
        return True
    if isinstance(b, tuple):
        return any(occurs(a, x, model) for x in b)
    return False
