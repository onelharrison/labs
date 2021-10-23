import pprint
from collections import defaultdict
from itertools import zip_longest
from typing import Dict, List, Set

from models.basic_block import BasicBlock, LeaderStatementIdRule, Statement
from models.graph import Graph

pp = pprint.PrettyPrinter(indent=2)


def is_branch_statement(statement: str) -> bool:
    branch_instruction, *_ = statement.split(",")
    return branch_instruction.startswith("br")


def extract_branch_target(statement: str) -> str:
    return statement.split(",")[3].strip()


def is_goto_statement(statement: str) -> bool:
    return statement.startswith("goto")


def extract_goto_target(statement: str) -> str:
    return statement.split(",")[1].strip()


def is_label_statement(statement: str) -> bool:
    return statement.endswith(":")


def extract_label(statement: str) -> str:
    return statement.strip(":")


def get_basic_block_leader_statements(program_lines: List[str],) -> List[Statement]:

    leader_statements: Set[Statement] = set()

    jump_targets = defaultdict(list)

    # First pass to capture jump targets, leader
    for line_num, (current_line, next_line) in enumerate(
        zip_longest(program_lines, program_lines[1:], fillvalue="EOF"), 1
    ):
        if line_num == 1:
            leader_statements.add(
                Statement(line_num, current_line, str(LeaderStatementIdRule.RULE_1))
            )

        if is_branch_statement(current_line):
            if next_line != "EOF":
                leader_statements.add(
                    Statement(
                        line_num + 1, next_line, str(LeaderStatementIdRule.RULE_3)
                    )
                )

            branch_target = extract_branch_target(current_line)
            jump_targets[branch_target].append(line_num)

        if is_goto_statement(current_line):
            goto_target = extract_goto_target(current_line)
            jump_targets[goto_target].append(line_num)

    # Second pass to capture all leader statements (including those that are jump targets that
    # may have been discovered in jump statements that were after them).
    for line_num, current_line in enumerate(program_lines, 1):
        # NOTE: There is an assumption here that jump targets are only to labels.
        # The jump statements containing jump targets only include the label names,
        # but the lines for labels are suffixed by a colon. Since jump targets
        # are discovered through jump statements (that don't include the colon with
        # the jump target), when a label line that is a known jump target is encountered,
        # the colon suffix has to be stripped before looking up the label in the collection
        # of jump targets.
        if (
            is_label_statement(current_line)
            and extract_label(current_line) in jump_targets
        ):
            label = extract_label(current_line)
            comment = f"{str(LeaderStatementIdRule.RULE_2)} (on line(s) {', '.join(list(map(str, jump_targets[label])))})"
            leader_statements.add(Statement(line_num, current_line, comment))

    return sorted(list(leader_statements), key=lambda s: s.line_num)


def create_control_flow_graph(
    program_lines: List[str], basic_block_leader_statements: List[Statement]
) -> Graph[BasicBlock]:
    basic_blocks: List[BasicBlock] = []

    for block_id, (current_leader_statement, next_leader_statement) in enumerate(
        zip_longest(basic_block_leader_statements, basic_block_leader_statements[1:]),
        1,
    ):
        if next_leader_statement:
            basic_block_statements = filter(
                lambda line_pair: (
                    line_pair[0] >= current_leader_statement.line_num
                    and line_pair[0] < next_leader_statement.line_num
                ),
                enumerate(program_lines, 1),
            )
        else:
            basic_block_statements = filter(
                lambda line_pair: (line_pair[0] >= current_leader_statement.line_num),
                enumerate(program_lines, 1),
            )

        basic_blocks.append(
            BasicBlock(
                block_id=block_id,
                statements=[
                    Statement(line_num, text)
                    for line_num, text in basic_block_statements
                ],
            )
        )

    graph: Graph[BasicBlock] = Graph()
    basic_block_by_label: Dict[str, BasicBlock] = dict()

    for basic_block in basic_blocks:
        graph.add_node(basic_block)

        if is_label_statement(basic_block.leader_statement.text):
            label = extract_label(basic_block.leader_statement.text)
            basic_block_by_label[label] = basic_block

    for current_basic_block, next_basic_block in zip_longest(
        basic_blocks, basic_blocks[1:]
    ):
        if is_branch_statement(current_basic_block.last_statement.text):
            branch_target = extract_branch_target(
                current_basic_block.last_statement.text
            )
            graph.add_edge(current_basic_block, basic_block_by_label[branch_target])

        elif is_goto_statement(current_basic_block.last_statement.text):
            goto_target = extract_goto_target(current_basic_block.last_statement.text)
            graph.add_edge(current_basic_block, basic_block_by_label[goto_target])

        if next_basic_block:
            if not is_goto_statement(current_basic_block.last_statement.text):
                graph.add_edge(current_basic_block, next_basic_block)

    return graph


def stringify_control_flow_graph_as_dot(graph: Graph[BasicBlock]) -> str:
    digraph = ["digraph {"]

    for basic_block in graph.nodes:

        block_text = "\n".join(
            map(lambda s: f"{s.line_num} {s.text}", basic_block.statements)
        )
        digraph.append(
            f'{basic_block.block_id} [label="B-{basic_block.block_id}\n\n{block_text}"]'
        )

    for basic_block_1, basic_block_2 in graph.edges:
        digraph.append(f"{basic_block_1.block_id} -> {basic_block_2.block_id}")

    digraph.append("}")

    return "\n".join(digraph)


if __name__ == "__main__":
    with open("program.ir") as f:
        program_lines = list(map(str.strip, f.readlines()))

    basic_block_leader_statements = get_basic_block_leader_statements(program_lines)
    pp.pprint(basic_block_leader_statements)

    control_flow_graph = create_control_flow_graph(
        program_lines, basic_block_leader_statements
    )

    with open("cfg.gv", "w") as f:
        f.write(stringify_control_flow_graph_as_dot(control_flow_graph))
