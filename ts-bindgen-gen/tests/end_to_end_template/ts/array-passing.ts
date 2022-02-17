export class C {
  constructor() {}
}
export function acceptStringArray(a: Array<string>): number {
  return a.length;
}
export function acceptClassArray(a: Array<C>): number {
  return a.length;
}
export function returnStringArray(): Array<string> {
  return ["hello", "world"];
}
export function returnClassArray(): Array<C> {
  return [new C(), new C()];
}
