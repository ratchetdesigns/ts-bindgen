export interface GettersAndSetters {
  get num(): number;
  set num(n: number);

  get getter(): string;

  set setter(s: string);
}

export interface Indexer {
  [a: string]: number;
}

export interface Ctor {
  new(n: number): Ctor;
}
