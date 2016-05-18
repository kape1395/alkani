import { Component         } from '@angular/core';
import { OnInit            } from '@angular/core';
import { ViewChild         } from "@angular/core";
import { AfterViewInit     } from "@angular/core";
import { Subject           } from 'rxjs/Subject';
import { Model             } from './alkani-model';
import { AlkaniGameService } from './alkani-game.service';

//
// See http://embed.plnkr.co/LFhOuepJrnPVlwUXmkUt/
//
@Component({
    selector: 'alkani-game-area',
    template: `
        <canvas #canvas class="alkani-game-area" width="400" height="400"></canvas>
        `,
    directives: []
})
export class AlkaniGameAreaComponent implements OnInit, AfterViewInit {
    private modelSource: Subject<Model>;
    private model: Model;
    private context: CanvasRenderingContext2D;

    @ViewChild("canvas") canvas;

    constructor(private alkaniGameService: AlkaniGameService) {
    }

    ngOnInit() {
        this.alkaniGameService.connect();
        this.modelSource = this.alkaniGameService.getModelSource();
    }

    ngAfterViewInit() {
        this.context = this.canvas.nativeElement.getContext("2d");
        this.modelSource.subscribe({
            next: (v) => { this.model = v; this.redraw(); }
        });
    }

    redraw() {
        var ctx = this.context;
        var sizeX = ctx.canvas.width;
        var sizeY = ctx.canvas.height;
        ctx.clearRect(0, 0, sizeX, sizeY);
        if (!this.model) {
            console.log("No model to draw.");
            return;
        }
        for (var f of this.model.food) {
            ctx.beginPath();
            ctx.arc(f.pos_x * sizeX, f.pos_y * sizeY, f.size * 5, 0, 2 * Math.PI);
            ctx.fillStyle = "blue";
            ctx.fill();

        }
    }
}
