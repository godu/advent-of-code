
import test from 'ava';
import { from } from 'rxjs';
import { reduce, repeat, startWith, scan, skipWhile, first } from 'rxjs/Operators'
import intput from './fixtures/day1';

type Input = Array<number>;

export const part1 = (input: Input) => {
  return from(input).pipe(
    reduce((a: number, b: number): number => a + b, 0),
  ).toPromise();
};

test("day1#part1", async t => {
  t.deepEqual(await part1([1, 1, -2]), 0);
  t.deepEqual(await part1([-1, -2, -3]), -6);
  t.deepEqual(await part1(intput), 497);
});

const alreadySeen = <T>() => {
  const values: Set<T> = new Set();
  return (x: T) => {
    if (values.has(x)) {
      return false;
    } else {
      values.add(x);
      return true;
    }
  };
};

export const part2 = (input: Input): Promise<number> => {
  return from(input).pipe(
    repeat(),
    startWith(0),
    scan((a, b) => a + b, 0),
    skipWhile(alreadySeen()),
    first()
  ).toPromise();
};

test("day1#part2", async t => {
  t.deepEqual(await part2([1, -1]), 0);
  t.deepEqual(await part2([3, 3, 4, -2, -4]), 10);
  t.deepEqual(await part2([-6, 3, 8, 5, -6]), 5);
  t.deepEqual(await part2([7, 7, -2, -7, -4]), 14);
  t.deepEqual(await part2(intput), 558);
});
