export * as RoundTripClone from './round-trip-clone';
export { ClassMethodInvokerTest } from './class-method-invoker';
export * as Generics from './generics';
import {runTest as roundTripClone, RoundTripCloneFn} from './round-trip-clone';
import {AInt, testObj as genericTestObj, testString as genericTestString, testGenericPropagation as genericTestPropagation, GenericCloneFn, D} from './generics';
import './import-export-tester';
export * as TypeOps from './type-operators';
export * as destructureParamTest from './destructure-param-test';
export * as interfaceTest from './interface-test';
export * as overloadTest from './function-overload';
export * as arrayTest from './array-passing';

export function runTest<G>(
  cloneFn: RoundTripCloneFn,
  genericStringClone: GenericCloneFn<string>,
  genericObjClone: GenericCloneFn<AInt>,
  genericDClone: GenericCloneFn<D<string>>,
  ...a: number[]
): boolean {
  return roundTripClone(cloneFn)
    && genericTestString(genericStringClone)
    && genericTestObj(genericObjClone)
    && genericTestPropagation(genericDClone);
}
