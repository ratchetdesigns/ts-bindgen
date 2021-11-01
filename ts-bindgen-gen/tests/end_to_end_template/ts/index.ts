export * as RoundTripClone from './round-trip-clone';
import {runTest as roundTripClone, RoundTripCloneFn} from './round-trip-clone';

export function runTest(cloneFn: RoundTripCloneFn, ...a: number[]): boolean {
  return roundTripClone(cloneFn);
}
