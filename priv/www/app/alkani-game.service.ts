import { Injectable } from '@angular/core';
import { Http       } from '@angular/http';
import { Subject    } from 'rxjs/Subject';
import { Model      } from './alkani-model';

//
// See http://reactivex.io/rxjs/manual/overview.html#subject
//
@Injectable()
export class AlkaniGameService {
    private ws: WebSocket;
    private ms: Subject<Model>;

    constructor(private http: Http) {
    }

    connect() {
        var playerSocketUrl = "ws://" + location.host + "/alkani/player";
        this.ms = new Subject<Model>();
        this.ws = new WebSocket(playerSocketUrl);
        this.ws.onmessage = (evt) => this.ms.next(<Model>JSON.parse(evt.data));
        this.ws.onerror   = (evt) => this.ms.error(evt);
        this.ws.onclose   = (evt) => this.ms.complete();
    }

    getModelSource(): Subject<any> {
        return this.ms;
    }

}
