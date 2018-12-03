import { Observable } from "rxjs/Rx";

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
