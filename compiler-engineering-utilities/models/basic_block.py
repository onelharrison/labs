from dataclasses import dataclass
from enum import Enum, unique
from typing import List, Tuple


@unique
class LeaderStatementIdRule(str, Enum):
    RULE_1 = "RULE 1: Statement is first statement in program"
    RULE_2 = "RULE 2: Statement is target of a branch statement"
    RULE_3 = "RULE 3: Statement immediately follows a branch or return statement"

    def __str__(self):
        return str.__str__(self)


@dataclass(frozen=True)
class Statement:
    line_num: int
    text: str
    comment: str = ""

    def __hash__(self):
        return hash(self.line_num)


@dataclass(frozen=True)
class BasicBlock:
    block_id: int
    statements: List[Statement]

    @property
    def leader_statement(self) -> Statement:
        return self.statements[0]

    @property
    def last_statement(self) -> Statement:
        return self.statements[-1]

    @property
    def line_num_range(self) -> Tuple[int, int]:
        return (self.leader_statement.line_num, self.statements[-1].line_num)

    def __hash__(self):
        return hash(self.block_id)
