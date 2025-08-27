fn add(a: int, b: int) -> int {
    return a + b;
}

fn mul(a: int, b: int) -> int {
    return a * b;
}

fn square(x: int) -> int {
    return mul(x, x);
}

let g: int = 10;

fn main() -> int {
    let x: int = 5;
    let y: int = 3;
    let result: int = add(x, y);

    print(result);         
    print(square(4));      

    if (result > g) {   
        print(1);            
    } else {
        print(0);
    }

    let i: int = 0;
    while (i < 3) { 
        print(i);
        i = i + 1;
    }

    return result;
}
