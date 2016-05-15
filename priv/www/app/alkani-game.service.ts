import { Injectable } from '@angular/core';
import { Http       } from '@angular/http';

@Injectable()
export class AlkaniGameService {
    private ws = null;

    constructor(private http: Http) {
    }

    connect() {
        var playerSocketUrl = "ws://" + location.host + "/alkani/player";
        this.ws = new WebSocket(playerSocketUrl);
        this.ws.onmessage = (evt) => console.debug("** Event:", evt.data);
        this.ws.onerror   = (evt) => console.error(`Error: ${evt}`, evt);
        this.ws.onclose   = (evt) => console.debug("** Closed **");
        this.ws.onopen    = (evt) => console.debug("** Openned ***");
    }

    getModel() {
        return {
            players: [
                {name: "alkanas", pos_x: 20, pos_y: 20, size: 20}
            ],
            food: [
                {pos_x: 20, pos_y: 10, size: 1},
                {pos_x: 10, pos_y: 20, size: 1}
            ]
        };
    }
}
