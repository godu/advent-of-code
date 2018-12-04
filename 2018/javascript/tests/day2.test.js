
import test from 'ava';
import { Observable } from 'rxjs/Rx';
import input from './fixtures/day2';

const { from, of, zip, combineLatest } = Observable;
const count = s$ => s$.count();
const toChar$ = s => [...s] |> from;
const identity = v => v;
const isEqual = a => b => a === b;

export const part1 = input => {
  return from(input)
    .flatMap(word => {
      const letterCount = of(word)
        .flatMap(toChar$)
        .groupBy(identity)
        .flatMap(count);

      const haveExactlyTwo = letterCount.filter(isEqual(2)).take(1).count();
      const haveExactlyThree = letterCount.filter(isEqual(3)).take(1).count();

      return combineLatest(haveExactlyTwo, haveExactlyThree).last();
    })
    .reduce((acc, curr) => ([acc[0] + curr[0], acc[1] + curr[1]]), [0, 0])
    .map(([a, b]) => a * b)
    .toPromise();
};

export const part2 = input => {
  return from(input)
    .map((word, index) => [word |> toChar$, from(input).skip(index + 1)])
    .flatMap(([word$, words$]) => {
      return words$.flatMap(word => {
        return combineLatest(
          zip(
            word |> toChar$,
            word$,
            (a, b) => {
              return a === b ? 0 : 1;
            }
          ).reduce((a, b) => a + b, 0),
          zip(
            word |> toChar$,
            word$,
            (a, b) => {
              return a === b ? a : '';
            }
          ).reduce((a, b) => a + b, '')
        ).last();
      });
    })
    .reduce((acc, curr) => {
      return acc[0] > curr[0] ? curr : acc;
    }, [Infinity, ''])
    .pluck(1)
    .toPromise();
};

const macro = async (t, _, fun, input, expected) => {
  const actual = await fun(input);

  t.deepEqual(actual, expected);
};

macro.title = (_, part, __, input, expected) => `day2#part${part}(${input}) === ${expected}`;

test(macro, 1, part1, ['abcdef', 'bababc', 'abbcde', 'abcccd', 'aabcdd', 'abcdee', 'ababab'], 12);
// test(macro, 1, part1, input, 6150);

// test(macro, 2, part2, ['fghij', 'fguij', 'abcde'], 'fgij');
// test(macro, 2, part2, input, 'rteotyxzbodglnpkudawhijsc');
