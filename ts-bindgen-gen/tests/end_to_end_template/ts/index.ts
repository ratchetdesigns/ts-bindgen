export * as RoundTripClone from './round-trip-clone';
export { ClassMethodInvokerTest } from './class-method-invoker';
export * as Generics from './generics';
import {runTest as roundTripClone, RoundTripCloneFn} from './round-trip-clone';
import {AInt, testObj as genericTestObj, testString as genericTestString, GenericCloneFn} from './generics';

export function runTest<G>(
  cloneFn: RoundTripCloneFn,
  genericStringClone: GenericCloneFn<string>,
  genericObjClone: GenericCloneFn<AInt>,
  ...a: number[]
): boolean {
  return roundTripClone(cloneFn)
    && genericTestString(genericStringClone)
    && genericTestObj(genericObjClone);
}
