import { Component         } from '@angular/core';
import { OnInit            } from '@angular/core';
import { ViewChild         } from "@angular/core";
import { AfterViewInit     } from "@angular/core";
import { HostListener      } from "@angular/core";
import { Subject           } from 'rxjs/Subject';
import { Model             } from './alkani-model';
import { AlkaniGameService } from './alkani-game.service';

//
// See http://embed.plnkr.co/LFhOuepJrnPVlwUXmkUt/
//
@Component({
    selector: 'alkani-game-area',
    template: `
        <div>
            <canvas #canvas class="alkani-game-area" width="400" height="400"></canvas>
        </div>
        <div>Size={{size}}, x={{cursor_x}}, y={{cursor_y}}</div>
        `,
    directives: []
})
export class AlkaniGameAreaComponent implements OnInit, AfterViewInit {
    private modelSource: Subject<Model>;
    private model: Model;
    private name: string;
    private size: number;
    private cursor_x: number;
    private cursor_y: number;
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
        this.cursor_x = this.context.canvas.width / 2;
        this.cursor_y = this.context.canvas.height / 2;
        this.modelSource.subscribe({
            next: (v) => {
                this.model = v;
                this.name = v.name;
                this.size = v.size;
                this.redraw();
            }
        });
    }

    @HostListener("mousemove", ['$event'])
    onMouseMove(event: MouseEvent) {
        this.cursor_x = event.offsetX;
        this.cursor_y = event.offsetY;
        return false;
    }

    redraw() {
        var ctx = this.context;
        var area_size = ctx.canvas.width;    // Width and Height must be equal.
        ctx.clearRect(0, 0, area_size, area_size);
        if (!this.model) {
            console.log("No model to draw.");
            return;
        }
        //
        ctx.beginPath();
        ctx.arc(area_size / 2, area_size / 2, this.model.r * area_size, 0, 2 * Math.PI);
        ctx.fillStyle = "red";
        ctx.fill();
        //
        for (var f of this.model.food) {
            ctx.beginPath();
            ctx.arc(area_size / 2 + f.x * area_size, area_size / 2 + f.y * area_size, f.r * area_size, 0, 2 * Math.PI);
            ctx.fillStyle = "blue";
            ctx.fill();

        }
    }
}
