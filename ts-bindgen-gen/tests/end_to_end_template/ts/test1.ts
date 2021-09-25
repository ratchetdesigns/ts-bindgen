export declare type A = string;

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
  hello: string;
  union: string | number | null | undefined;
  readonly environment: {
      [key: string]: string;
  };
}

export interface Def extends Abc {
  anotherField: boolean;
}

export function runTest(): number {
  return 5;
}
