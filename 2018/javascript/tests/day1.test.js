
import test from 'ava';
import { Observable } from 'rxjs/Rx';
import intput from './fixtures/day1';

export const part1 = input => {
  return Observable.from(input)
    .reduce((a, b) => a + b, 0)
    .toPromise();
};

const alreadySeen = () => {
  const values = new Set();
  return x => {
    if (values.has(x)) {
      return false;
    } else {
      values.add(x);
      return true;
    }
  };
};

export const part2 = input => {
  return Observable.from(input)
    .repeat()
    .startWith(0)
    .scan((a, b) => a + b, 0)
    .skipWhile(alreadySeen())
    .first()
    .toPromise();
};

const macro = async(t, _, fun, input, expected) => {
  const actual = await fun(input);

  t.deepEqual(actual, expected);
};

macro.title = (_, part, __, input, expected) => `day1#part${part}(${input}) === ${expected}`;

test(macro, 1, part1, [1, 1, -2], 0);
test(macro, 1, part1, [-1, -2, -3], -6);
test(macro, 1, part1, intput, 497);

test(macro, 2, part2, [1, -1], 0);
test(macro, 2, part2, [3, 3, 4, -2, -4], 10);
test(macro, 2, part2, [-6, 3, 8, 5, -6], 5);
test(macro, 2, part2, [7, 7, -2, -7, -4], 14);
test(macro, 2, part2, intput, 558);
