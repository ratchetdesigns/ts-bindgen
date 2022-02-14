export function overloaded(n: number): number;
export function overloaded(s: string): string;
export function overloaded(n: number | string): number | string {
  return n;
}
