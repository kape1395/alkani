
export class Player {
    name: string;
    x: number;
    y: number;
    r: number;
}

export class Food {
    x: number;
    y: number;
    r: number;
}

export class Model{
    name:    string;    //
    size:    number;    //
    r:       number;    // Radius of the player.
    players: Player[];  // All visible other players.
    food:    Food[];    // All visible food.
}

