import itertools as it
from collections import defaultdict, namedtuple
from typing import Dict, List, Optional, Set, Tuple

ProductionRule = namedtuple("Production", ["nonterminal", "rule"])


class CFG:
    def __init__(self, bnf: str, epsilon: str = "EPSILON"):
        self.__bnf = bnf.strip()
        self.epsilon = epsilon

        self.__dict = self.to_dict()

    @property
    def production_rules(self) -> Set[ProductionRule]:
        return {
            ProductionRule(nonterminal, rule)
            for nonterminal, rules in self.__dict.items()
            for rule in rules
        }

    def production_rules_for(self, sym: str) -> Set[ProductionRule]:
        if sym not in self.nonterminals:
            raise ValueError(f"{sym} is not a nonterminal")

        return set(filter(lambda p: p.nonterminal == sym, self.production_rules))

    @property
    def nonterminals(self) -> Set[str]:
        return set(self.__dict.keys())

    @property
    def terminals(self) -> Set[str]:
        _terminals = set()
        for _, rule in self.production_rules:
            for sym in rule.split(" "):
                if sym not in self.nonterminals and sym != self.epsilon:
                    _terminals.add(sym)
        return _terminals

    def __str__(self):
        return self.__bnf

    def to_dict(self):
        if hasattr(self, "__dict"):
            return self.__dict

        __dict = defaultdict(list)

        nonterminal = None
        lines = map(str.strip, self.__bnf.split("\n"))

        for line in lines:
            if line == "":
                continue

            if line.startswith("|"):
                if nonterminal is None:
                    raise Exception(
                        "Invalid grammar: 'also derives' symbol '|' is not part of a production"
                    )
                else:
                    __dict[nonterminal].extend(
                        list(map(str.strip, line[1:].split("|")))
                    )
            else:
                nonterminal, rules = list(map(str.strip, line.split("::=")))
                __dict[nonterminal].extend(list(map(str.strip, rules.split("|"))))

        return dict(__dict)


class LLInspector:
    def __init__(self, cfg: CFG):
        self.__cfg = cfg

    def first_set(self, sym) -> Set[str]:
        _first_set = set()

        # TODO: Possibly do a treatment for eof
        if sym in self.__cfg.terminals or sym == self.__cfg.epsilon:
            _first_set.add(sym)
            return _first_set

        for _, rule in self.__cfg.production_rules_for(sym):
            rule_symbols = rule.split(" ")

            i = 0
            k = len(rule_symbols) - 1

            rhs = self.first_set(rule_symbols[i]).difference({self.__cfg.epsilon})
            while self.__cfg.epsilon in self.first_set(rule_symbols[i]) and i <= k - 1:
                rhs.update(
                    self.first_set(rule_symbols[i + 1]).difference({self.__cfg.epsilon})
                )
                i += 1

            if i == k and self.__cfg.epsilon in self.first_set(rule_symbols[k]):
                rhs.update({self.__cfg.epsilon})

            _first_set.update(rhs)

        return _first_set

    def follow_set(self, sym) -> Set[str]:
        _follow_sets = defaultdict(set)

        for nonterminal in self.__cfg.nonterminals:
            _follow_sets[nonterminal].update(self.follow_set(nonterminal))

        return _follow_sets

    def first_plus_set(self, sym) -> Set[str]:
        return self.first_set(sym).union(self.follow_set(sym))

    def first_sets(self, symbols: Optional[List] = None) -> Dict[str, Set]:
        _first_sets = defaultdict(set)

        if symbols is None:
            symbols = list(self.__cfg.nonterminals)

        for sym in symbols:
            _first_sets[sym].update(self.first_set(sym))

        return dict(_first_sets)

    @property
    def follow_sets(self) -> Dict[str, Set]:
        return dict()


if __name__ == "__main__":

    lexp_bnf = """
    list ::= ( lexpseq )

    lexpseq ::= lexp lexpseq-tail

    lexpseq-tail ::= , lexpseq lexpseq-tail
                   | EPSILON

    lexp ::= atom
          | list

    atom ::= NUMBER
           | IDENTIFIER
    """

    lexp_cfg = CFG(lexp_bnf)

    lli = LLInspector(lexp_cfg)

    print(lli.first_sets())
    # TODO: Implement follow sets
