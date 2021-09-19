import { Common } from "./common";

export declare type A = string;

export declare type B = A[];

export declare enum MyEnum {
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
  [a: string]: number;
  hello: string;
  opt?: B;
  isect: string & 'abc';
  union: string | number | null;
  readonly environment: {
      [key: string]: string;
  };
  other: Common;
  any: any;
}

export interface Def extends Abc {
  anotherField: boolean;
}

class MyClass {
  constructor(myCtorArg: Object);

  field1: string;
  someMethod(x: string, y: number): Abc;
}
