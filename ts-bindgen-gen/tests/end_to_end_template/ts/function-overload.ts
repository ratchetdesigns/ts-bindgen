export function overloaded(n: number): number;
export function overloaded(s: string): string;
export function overloaded(n: number | string): number | string {
  return n;
}

export class Over {
  s: string;

  overload(n: number): number;
  overload(s: string): string;
  overload(n: number | string): number | string {
    return n;
  }

  constructor();
  constructor(n: number);
  constructor(s: string);
  constructor(n?: number | string) {
    this.s = '' + (n ?? 42);
  }
}
