export * as Test1 from './test1';
import {runTest as runTest1, BridgeTestFn as BridgeTestFn1} from './test1';

export function runTest(bridgeTestFn1: BridgeTestFn1, ...a: number[]): boolean {
  return runTest1(bridgeTestFn1);
}
