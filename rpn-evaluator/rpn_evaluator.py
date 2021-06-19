#!/usr/bin/env python

import logging
import operator as op
import sys
from typing import Any, List, Union

logging.getLogger(__name__).setLevel("INFO")

supported_operators = {"+": op.add, "-": op.sub, "*": op.mul, "/": op.truediv}

Number = Union[int, float]


def tokenize(expr: str) -> List[str]:
    """Parses expression `expr` into a list of tokens"""
    return expr.split(" ")


def popn(stack: List[Any], n: int = 1) -> List[Any]:
    """Pops and returns `n` items from a stack"""
    return [stack.pop() for _ in range(n)]


def to_num(x: Any) -> Number:
    """Converts a value to a its appropriate numeric type"""
    n = float(x)
    return int(n) if n.is_integer() else n


def consume_token(token: str, stack: List[Number]):
    """Consumes a token given the current stack and returns the updated stack"""
    if token in supported_operators:
        try:
            num1, num2 = popn(stack, 2)
        except IndexError:
            logging.error("SyntaxError: Malformed expression")
            sys.exit(1)

        result = supported_operators[token](num2, num1)
        stack.append(result)
    else:
        try:
            stack.append(to_num(token))
        except ValueError:
            logging.error("SyntaxError: Unsupported token '%s'", token)
            sys.exit(1)

    return stack


def get_result_from_stack(stack: List[Number]) -> Number:
    """Gets the result from `stack`"""
    result, *rest = popn(stack, 1)
    if rest:
        logging.error("SyntaxError: Found extra tokens")
        sys.exit(1)
    return result


def evaluate_v1(tokens: List[str]) -> Number:
    """Evaluates a tokenized expression and returns the result"""
    stack: List = []

    for token in tokens:
        stack = consume_token(token, stack)

    return get_result_from_stack(stack)


def evaluate_v2(tokens: List[str]) -> Number:
    """Evaluates a tokenized expression and returns the result"""

    def _evaluate(tokens: List[str], stack: List) -> Number:
        if not tokens:
            return get_result_from_stack(stack)

        stack = consume_token(tokens[0], stack)

        return _evaluate(tokens[1:], stack)

    return _evaluate(tokens, [])


if __name__ == "__main__":
    # Usage example
    #
    # echo "1 10 30 + *" | ./rpn_evaluator.py
    # echo "1 10 30 + *" | python rpn_evaluator.py
    print(evaluate_v2(tokenize(input())))
