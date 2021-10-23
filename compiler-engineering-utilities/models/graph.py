from dataclasses import dataclass, field
from typing import Generic, Set, Tuple, TypeVar

## Exceptions


class GraphException(Exception):
    pass


class MissingNodeException(GraphException):
    def __init__(self, node):
        super().__init__(f"{node} not found")


## Graph

T = TypeVar("T")


@dataclass
class Graph(Generic[T]):
    nodes: Set[T] = field(default_factory=set)
    edges: Set[Tuple[T, T]] = field(default_factory=set)

    def add_node(self, node: T) -> None:
        self.nodes.add(node)

    def add_edge(self, node1: T, node2: T, add_missing_node=False) -> None:
        if node1 not in self.nodes:
            if add_missing_node:
                self.add_node(node1)
            else:
                raise MissingNodeException(node1)

        if node2 not in self.nodes:
            if add_missing_node:
                self.add_node(node2)
            else:
                raise MissingNodeException(node2)

        self.edges.add((node1, node2))
