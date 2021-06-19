#!/usr/bin/env python

import logging
import operator as op
import sys
from typing import List

logging.getLogger().setLevel("INFO")

supported_operators = {"+": op.add, "-": op.sub, "*": op.mul, "/": op.truediv}


def tokenize(expr: str) -> List[str]:
    """Parses expression `expr` into a list of tokens"""
    return expr.split(" ")


def to_num(x):
    x = float(x)
    return int(x) if x.is_integer() else x


def evaluate_v1(tokens: List[str]):
    """Evaluates a tokenized expression and returns the result"""
    stack: List = []

    for token in tokens:
        if token in supported_operators:
            try:
                num1 = stack.pop()
                num2 = stack.pop()
            except IndexError:
                logging.error(f"SyntaxError: Malformed expression")
                sys.exit(1)

            result = supported_operators[token](num2, num1)
            stack.append(result)
        else:
            try:
                stack.append(to_num(token))
            except ValueError:
                logging.error(f"SyntaxError: Unsupported token '{token}'")
                sys.exit(1)

    result = stack.pop()
    if stack:
        logging.error(f"SyntaxError: Found extra tokens")
        sys.exit(1)
    return result


def evaluate_v2(tokens: List[str]):
    """Evaluates a tokenized expression and returns the result"""

    def _evaluate(tokens: List[str], stack: List):
        if not tokens:
            result = stack.pop()
            if stack:
                logging.error(f"SyntaxError: Found extra tokens")
                sys.exit(1)
            return result

        token = tokens[0]

        if token in supported_operators:
            try:
                num1 = stack.pop()
                num2 = stack.pop()
            except IndexError:
                logging.error(f"SyntaxError: Malformed expression")
                sys.exit(1)

            result = supported_operators[token](num2, num1)
            stack.append(result)
        else:
            try:
                stack.append(to_num(token))
            except ValueError:
                logging.error(f"SyntaxError: Unsupported token '{token}'")
                sys.exit(1)

        return _evaluate(tokens[1:], stack)

    return _evaluate(tokens, [])


if __name__ == "__main__":
    # Usage example
    #
    # echo "1 10 30 + *" | ./rpn_evaluator.py
    # echo "1 10 30 + *" | python rpn_evaluator.py
    print(evaluate_v2(tokenize(input())))
