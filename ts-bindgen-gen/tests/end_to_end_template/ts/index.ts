export * as RoundTripClone from './round-trip-clone';
export { ClassMethodInvokerTest } from './class-method-invoker';
import {runTest as roundTripClone, RoundTripCloneFn} from './round-trip-clone';

export function runTest(cloneFn: RoundTripCloneFn, ...a: number[]): boolean {
  return roundTripClone(cloneFn);
}
