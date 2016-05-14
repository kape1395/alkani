import { Component         } from '@angular/core';
import { OnInit            } from '@angular/core';
import { AlkaniGameService } from './alkani-game.service';

@Component({
    selector: 'alkani-game-area',
    template: `
        <canvas class="alkani-game-area"></canvas>
        `,
    directives: []
})
export class AlkaniGameAreaComponent implements OnInit {
    constructor(private alkaniGameService: AlkaniGameService) {
    }
    ngOnInit() {
        this.alkaniGameService.connect();
    }
}
