
import test from 'ava';
// import input from './fixtures/day7';

type Node = string;
type Edge = [Node, Node];

const stringToEdge = (s: string):Edge  => {
  const parts = s.split(' ');
  return [
    parts[1],
    parts[7]
  ];
};

test('day7#stringToEdge', t => {
  t.deepEqual(stringToEdge('Step C must be finished before step A can begin.'), ['C', 'A']);
  t.deepEqual(stringToEdge('Step C must be finished before step F can begin.'), ['C', 'F']);
  t.deepEqual(stringToEdge('Step A must be finished before step B can begin.'), ['A', 'B']);
  t.deepEqual(stringToEdge('Step A must be finished before step D can begin.'), ['A', 'D']);
  t.deepEqual(stringToEdge('Step B must be finished before step E can begin.'), ['B', 'E']);
  t.deepEqual(stringToEdge('Step D must be finished before step E can begin.'), ['D', 'E']);
  t.deepEqual(stringToEdge('Step F must be finished before step E can begin.'), ['F', 'E']);
});

const getParents = (edges: Array<Edge>) => (node: Node): Array<Node> =>
  edges.filter(([, a]) => a === node).map(([b]) => b);

const testEdges: Array<Edge> = [
  ['C', 'A'],
  ['C', 'F'],
  ['A', 'B'],
  ['A', 'D'],
  ['B', 'E'],
  ['D', 'E'],
  ['F', 'E']
];
test('day7#getParents', t => {
  t.deepEqual(getParents(testEdges)("A"), ['C']);
  t.deepEqual(getParents(testEdges)("B"), ['A']);
  t.deepEqual(getParents(testEdges)("C"), []);
  t.deepEqual(getParents(testEdges)("D"), ['A']);
  t.deepEqual(getParents(testEdges)("E"), ['B', 'D', 'F']);
  t.deepEqual(getParents(testEdges)("F"), ['C']);
});

// `Step C must be finished before step A can begin.
// Step C must be finished before step F can begin.
// Step A must be finished before step B can begin.
// Step A must be finished before step D can begin.
// Step B must be finished before step E can begin.
// Step D must be finished before step E can begin.
// Step F must be finished before step E can begin.`

// const macro = async(t, _, fun, input, expected) => {
//   const actual = await fun(input);

//   t.deepEqual(actual, expected);
// };

// macro.title = (_, part, __, input, expected) => `day2#part${part}(${input}) === ${expected}`;

// test(macro, 1, part1, ['abcdef', 'bababc', 'abbcde', 'abcccd', 'aabcdd', 'abcdee', 'ababab'], 12);
// // test(macro, 1, part1, input, 6150);

// // test(macro, 2, part2, ['fghij', 'fguij', 'abcde'], 'fgij');
// // test(macro, 2, part2, input, 'rteotyxzbodglnpkudawhijsc');
