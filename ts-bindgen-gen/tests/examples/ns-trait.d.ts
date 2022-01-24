export class Shape {
  baseProp: TopLevel;
  baseMethod(): TopLevel;
}

export class TopLevel {}

namespace other {
  export class Square extends Shape {}
}
