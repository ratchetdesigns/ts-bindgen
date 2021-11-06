export class ClassMethodInvokerTest {
  str: string;

  constructor(str: string) {
    this.str = str;
  }

  getInfo(): string {
    return `hello world ${this.str}`;
  }
}
