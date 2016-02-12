class Point {
  x : number;
  y : number;
}

function norm(p : Point) : number {
  return Math.abs(p.x) + Math.abs(p.y);
}

norm(3);
// norm.ts(10,6): error TS2345: Argument of type 'number' is not assignable to parameter of type 'Point'.
// Property 'x' is missing in type 'Number'.
