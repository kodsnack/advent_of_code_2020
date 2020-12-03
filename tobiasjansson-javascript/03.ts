// Ville testa Deno.land

export {} // Annars får jag ingen module fel

// deno run --allow-read 03.ts data/03-in.txt
const data = await Deno.readTextFile(Deno.args[0]);

let lines = data.split('\n');

const width = lines[0].length;
const height = lines.length;

function trees(dx:number, dy:number) {
    let x:number = 0;
    let y:number = 0;
    let tree = 0;
    while (y < height) {
        let line = lines[y].split('');
        if (line[x % width] == '#') tree++;
        x += dx;
        y += dy;
    }
    return tree;
}

// Del 1
console.log(trees(3,1));

// Del 2
console.log(trees(1,1) * trees(3,1) * trees(5,1) * trees(7,1) * trees(1,2));