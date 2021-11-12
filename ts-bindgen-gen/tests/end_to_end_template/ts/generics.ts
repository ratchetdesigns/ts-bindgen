import * as util from 'util';

export interface A {
  a: number;
}

export interface B extends A {
  b: number;
}

export interface C {
  c: number;
}

export interface D extends B, C {
  d: number;
}

export class BaseClass implements B, C {
  a: number;
  b: number;
  c: number;
  base: number;
}

export class DerivedClass extends BaseClass {
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
