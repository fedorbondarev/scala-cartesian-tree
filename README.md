# Scala-Cartesian-Tree

`Scala-Cartesian-Tree` is a pure functional implementation of the Cartesian Tree data structure in Scala, leveraging Cats Effect for random number generation.

## Overview

A Cartesian tree is a binary tree derived from a sequence of numbers. This implementation provides various functions to manipulate and traverse the Cartesian tree, including addition and deletion of elements, depth-first and breadth-first searches, finding maximum and minimum values, counting the number of nodes, and printing the tree structure.

## Functions

- **Add Element (add)**: Adds an element to the Cartesian tree.
- **Delete Element (delete)**: Deletes an element from the Cartesian tree.
- **Fold Left (foldLeft)**: Traverses the tree and accumulates the result of applying function f to each node.
- **Breadth-First Search (breadthFirstSearch)**: Traverses the tree in breadth-first order.
- **Depth-First Search (depthFirstSearch)**: Traverses the tree in depth-first order.
- **Maximum (max)**: Finds the maximum value using either breadth-first or depth-first search.
- **Minimum (min)**: Finds the minimum value using either breadth-first or depth-first search.
- **Size (size)**: Counts the number of nodes in the tree.
- **Print (print)**: Displays the tree structure for easy visualization.

## Testing

This project includes unit tests to ensure the correctness and reliability of the Cartesian tree implementation. Tests cover various scenarios and edge cases to validate the functionality of each operation.

## Usage

To use the Scala Cartesian Tree implementation, follow these steps:

## Contribution

Contributions to this project are welcome! If you find any issues, have suggestions for improvement, or would like to add new features, feel free to open an issue or submit a pull request.