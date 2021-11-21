import * as util from 'util';

export interface A<T> {
  a: T;
}

export interface B<T> extends A<T> {
  b: number;
}

export interface C<T> {
  c: T;
}

export interface D<T> extends B<number>, C<string> {
  d: T;
}

export class BaseClass<T> implements B<number>, C<string> {
  a: number;
  b: number;
  c: string;
  base: T;
}

export class DerivedClass extends BaseClass<number> {
  derived: number;
}

export interface SimpleGeneric<T> {
  s: string;
  t: T;
}

export type GenericCloneFn<T> = (input: SimpleGeneric<T>) => SimpleGeneric<T>;

export function testString(testFn: GenericCloneFn<string>): boolean {
  const o = {
    s: "string",
    t: "str"
  };

  return util.isDeepStrictEqual(testFn(o), o)
    && testFn(o) !== o;
}

export interface AInt {
  a: number;
}

export function testObj(testFn: GenericCloneFn<AInt>): boolean {
  const o = {
    s: "object",
    t: {
      a: 3
    }
  };

  return util.isDeepStrictEqual(testFn(o), o)
    && testFn(o) !== o;
}

export function testGenericPropagation(testFn: GenericCloneFn<D<string>>): boolean {
  let s = {
    s: "outer",
    t: {
      a: 7,
      b: 10,
      c: "hello",
      d: "world",
    }
  };

  return util.isDeepStrictEqual(testFn(s), s)
    && testFn(s) !== s;
}
