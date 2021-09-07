declare type Mapping = {
    [k1: string]: {
        [k2: string]: any;
    };
};

export declare type A = string;

export declare type B = A[];

export interface Abc {
  hello: string;
  opt?: B;
  isect: string & 'abc';
  union: string | number | null;
  readonly environment: {
      [key: string]: string;
  };
}

export interface Def extends Abc {
  another_field: boolean;
}

class MyClass {
  constructor(myCtorArg: Object);

  field1: string;
  someMethod(x: string, y: number): Abc;
}
