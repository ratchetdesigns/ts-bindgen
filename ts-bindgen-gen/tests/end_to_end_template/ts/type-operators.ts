export interface SomeThing {
  a: number;
}
export type KeyedThing = keyof SomeThing;
export type ReadonlyThing = readonly string[];
export const sym: unique symbol = Symbol();
// TODO: figure out how to handle https://docs.rs/swc_ecma_ast/0.49.1/swc_ecma_ast/enum.TsTypeQueryExpr.html.
//export function thing(t: typeof sym) { }
