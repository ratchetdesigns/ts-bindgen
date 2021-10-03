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
}

export interface Def extends Abc {
  anotherField: boolean;
}

export type AnyType = A | MyEnum | Base | Derived | Abc | Def;

export type BridgeTestFn = (input: AnyType) => AnyType;

export function runTest(testFn: BridgeTestFn): boolean {
  const base = { baseField: 7 };
  const derived = { baseField: 3, derivedField: "potato" };
  return testFn("hello") === "hello"
    && testFn(MyEnum.A) === "A"
    && util.isDeepStrictEqual(testFn(base), base)
    && testFn(base) !== base
    && util.isDeepStrictEqual(testFn(derived), derived)
    && testFn(derived) !== derived;
}
