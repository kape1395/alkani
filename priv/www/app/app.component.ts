import { Component } from '@angular/core';
import { AlkaniGameService       } from './alkani-game.service';
import { AlkaniGameAreaComponent } from './alkani-game-area.component';

@Component({
    selector: 'my-app',
    template: `
        <h1>They are {{title}}!</h1>
        <alkani-game-area></alkani-game-area>
        `,
    directives: [AlkaniGameAreaComponent],
    providers: [AlkaniGameService]
})
export class AppComponent {
    title = "Alkani";
}
