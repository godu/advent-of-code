
import test from 'ava';
import { from, of, combineLatest, zip } from 'rxjs';
import { count, filter, flatMap, groupBy, last, map, pluck, reduce, skip, take } from 'rxjs/Operators'
import input from './fixtures/day2';

const toChar$ = (s: string) => from([...s]);
const identity = <T>(v: T) => v;
const isEqual = <T>(a: T) => (b: T) => a === b;

type Input = Array<string>;

export const part1 = (input: Input) => {
    return from(input).pipe(
        flatMap((word: string) => {
            const letterCount = of(word).pipe(
                flatMap(toChar$),
                groupBy(identity),
                flatMap(count())
            );

            const haveExactlyTwo = letterCount.pipe(filter(isEqual(2)), take(1), count());
            const haveExactlyThree = letterCount.pipe(filter(isEqual(3)), take(1), count());

            return combineLatest(haveExactlyTwo, haveExactlyThree).pipe(last());
        }),
        reduce((acc, curr) => ([acc[0] + curr[0], acc[1] + curr[1]]), [0, 0]),
        map(([a, b]) => a * b)
    ).toPromise();
};

test("day2#part1", async t => {
    t.deepEqual(await part1(['abcdef', 'bababc', 'abbcde', 'abcccd', 'aabcdd', 'abcdee', 'ababab']), 12)
    t.deepEqual(await part1(input), 6150)
});

export const part2 = (input: Input) => {
    return from(input).pipe(
        map((word, index) =>
            [
                from(word),
                from(input).pipe(skip(index + 1))
            ]
        ),
        flatMap(([word$, words$]) => {
            return words$.pipe(
                flatMap(word => {
                    return combineLatest(
                        zip(
                            from(word),
                            word$,
                            (a, b) => {
                                return a === b ? 0 : 1;
                            }
                        ).pipe(reduce((a, b) => a + b, 0)),
                        zip(
                            from(word),
                            word$,
                            (a, b) => {
                                return a === b ? a : '';
                            }
                        ).pipe(reduce((a, b) => a + b, ''))
                    ).pipe(last());
                })
            )
        }),
        reduce((acc, curr) => {
            return acc[0] > curr[0] ? curr : acc;
        }, [Infinity, '']),
        pluck('1')
    ).toPromise();
};

test("day2#part2", async t => {
    t.deepEqual(await part2(['fghij', 'fguij', 'abcde']), 'fgij')
    t.deepEqual(await part2(input), 'rteotyxzbodglnpkudawhijsc')
});
