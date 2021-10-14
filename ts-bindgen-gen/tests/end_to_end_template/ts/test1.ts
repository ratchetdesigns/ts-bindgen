import fastcheck from 'fast-check';
import * as util from 'util';

export type A = string;

export enum MyEnum {
  A = "A",
  B = "B",
}

export interface Base {
  baseField: number;
}

export interface Derived extends Base {
  derivedField: string;
}

export interface Abc {
  hello: string;
  union: string | number | null | undefined;
  tup: [string, number];
  readonly environment: {
      [key: string]: string;
  };
  fn: (a: number) => string;
}

export interface Def extends Abc {
  anotherField: boolean;
}

export type MaybeNumber = null | undefined | number;

export type AnyType = A | MyEnum | Base | Derived | Abc | Def | MaybeNumber;

export type BridgeTestFn = (input: AnyType) => AnyType;

// TODO: because we serialize to json, we turn undefined into a missing key
function without<T, P extends keyof T>(obj: T, prop: P): Omit<T, P> {
  let o = { ...obj };
  delete o[prop];
  return o;
}

export function runTest(testFn: BridgeTestFn): boolean {
  const base = { baseField: 7 };
  const derived = { baseField: 3, derivedField: "potato" };
  const abc: Abc = {
    hello: "hi",
    union: undefined,
    tup: ["yo", 7],
    environment: {
      env_1: "something",
      env_2: "or another",
    },
    fn: (a: number) => `hello $a`,
  };
  return testFn("hello") === "hello"
    && testFn(MyEnum.A) === "A"
    && util.isDeepStrictEqual(testFn(base), base)
    && testFn(base) !== base
    && util.isDeepStrictEqual(testFn(derived), derived)
    && testFn(derived) !== derived
    && util.isDeepStrictEqual(testFn({...abc, union: null}), {...abc, union: null})
    && util.isDeepStrictEqual(testFn({...abc, union: 7}), {...abc, union: 7})
    && util.isDeepStrictEqual(testFn({...abc, union: undefined }), without(abc, "union"))
    && testFn(abc) !== abc
    && typeof testFn(undefined) === 'undefined';
}
