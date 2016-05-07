import { Component } from '@angular/core';
import { Http      } from '@angular/http';

@Component({
  selector: 'my-app',
  template: '<h1>Welcome to {{title}}</h1>'
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
        this.ws.onclose   = (evt) => console.error("** Closed **");
        this.ws.onopen    = (evt) => console.error("** Openned ***");
    }
}
