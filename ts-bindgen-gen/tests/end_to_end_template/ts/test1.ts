import fastcheck from 'fast-check';

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

export type BridgeTestFn = (AnyType) => AnyType;

export function runTest(testFn: BridgeTestFn): boolean {
  return testFn("hello") === "hello";
}
