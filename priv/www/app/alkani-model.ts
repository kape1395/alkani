
export class Player {
    name:  string;
    pos_x: number;
    pos_y: number;
    size:  number;
}

export class Food {
    pos_x: number;
    pos_y: number;
    size:  number;
}

export class Model{
    players: Player[];
    food:    Food[];
}

