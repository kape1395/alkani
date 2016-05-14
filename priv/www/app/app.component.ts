import { Component } from '@angular/core';
import { Http      } from '@angular/http';
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
    ws = null;
    constructor(public http: Http) {
        var playerSocketUrl = "ws://" + location.host + "/alkani/player";
        console.debug('Hello Debug!');
        this.ws = new WebSocket(playerSocketUrl);
        this.ws.onmessage = (evt) => console.debug(evt.data);
        this.ws.onerror   = (evt) => console.error(`Error: ${evt}`, evt);
        this.ws.onclose   = (evt) => console.debug("** Closed **");
        this.ws.onopen    = (evt) => console.debug("** Openned ***");
    }
}
