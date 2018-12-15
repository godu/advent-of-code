
import test from 'ava';
import input from './fixtures/day8';

type Input = Array<number>;
type Node = {
    children: Array<Node>,
    metadatas: Array<number>
};

const take = (count: number) => <T>(arr: Array<T>): [Array<T>, Array<T>] => [arr.slice(0, count), arr.slice(count)]

const parseNode = (numbers: Array<number>): [Node, Array<number>] => {
    let [childCount, metadataCount, ...followingNumbers] = numbers;

    let children: Array<Node> = [];
    while (childCount > 0) {
        const [child, restTmp] = parseNode(followingNumbers);
        followingNumbers = restTmp;

        children = [...children, child];
        childCount = childCount - 1;
    }

    const [metadatas, rest]: [Array<number>, Array<number>] = take(metadataCount)(followingNumbers);

    return [
        {
            children,
            metadatas
        },
        rest
    ];
};

const sum = (arr: Array<number>) => arr.reduce((a, b) => a + b, 0)

const getValues = (node: Node): number => {
    const {
        metadatas,
        children
    } = node;

    return sum([
        ...metadatas,
        ...children.map(getValues)
    ]);
}

export const part1 = (input: Input): number => {
    const [node]: [Node, Array<number>] = parseNode(input);
    return getValues(node);
};

test("day8#part1", async t => {
    t.deepEqual(await part1([2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2]), 138)
    t.deepEqual(await part1(input.split(' ').map(v => parseInt(v, 10))), 37905)
});