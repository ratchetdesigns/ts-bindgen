import { IFace, Cls } from './trait-import';
import Interface from './default-interface-export';
import Class from './default-class-export';

export interface Iface2 extends IFace {
  b: number;
}

export class Cls2 extends Cls {
  c: number;
}

export interface Interface2 extends Interface {
  b: number;
}

export class Class2 extends Class {
  b: number;
}
