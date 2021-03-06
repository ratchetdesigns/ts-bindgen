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

export class MyClass{
  n: number;

  constructor(n: number) {
    this.n = n;
  }

  getN(): number {
    return this.n;
  }
}

interface T1 {
  b: number;
}

interface T2 {
  a: string;
}

export type Isect = T1 & T2;

export type AnyType = A | MyEnum | Base | Derived | Abc | Def | MaybeNumber | MyClass | Isect;

export type RoundTripCloneFn = (input: AnyType) => AnyType;

function without<T, P extends keyof T>(obj: T, prop: P): Omit<T, P> {
  let o = { ...obj };
  delete o[prop];
  return o;
}

export function runTest(testFn: RoundTripCloneFn): boolean {
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
    fn: (a: number) => `hello ${a}`,
  };
  const myClass = new MyClass(5);
  const isect = {
    a: "isect!",
    b: 37
  };
  return testFn("hello") === "hello"
    && testFn(MyEnum.A) === "A"
    && util.isDeepStrictEqual(testFn(base), base)
    && testFn(base) !== base
    && util.isDeepStrictEqual(testFn(derived), derived)
    && testFn(derived) !== derived
    && util.isDeepStrictEqual(
      without(testFn({...abc, union: null}) as Abc, "fn"),
      without({...abc, union: null}, "fn")
    )
    && util.isDeepStrictEqual(
      without(testFn({...abc, union: 7}) as Abc, "fn"),
      without({...abc, union: 7}, "fn")
    )
    && util.isDeepStrictEqual(
      without(testFn({...abc, union: undefined }) as Abc, "fn"),
      without(abc, "fn")
    )
    && (testFn(abc) as Abc).fn(4) === "hello 4"
    && testFn(abc) !== abc
    && testFn(myClass) === myClass
    && typeof testFn(undefined) === 'undefined'
    && util.isDeepStrictEqual(testFn(isect), isect)
    && testFn(isect) !== isect;
}
