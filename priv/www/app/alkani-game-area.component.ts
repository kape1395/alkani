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
        <div>Name={{name}}, size={{size}}, x={{dirX}}, y={{dirY}}</div>
        `,
    directives: []
})
export class AlkaniGameAreaComponent implements OnInit, AfterViewInit {
    private modelSource: Subject<Model>;
    private model: Model;
    private name: string;
    private size: number;
    private dirX: number = 0;
    private dirY: number = 0;
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
        if (event.target != this.canvas.nativeElement) {
            // Mouse move events over <alkani-game-area> but not over
            // the <canvas> can be captured here.
            this.setNewDirection(0, 0);
            return false;
        }
        var area_size = this.context.canvas.width;
        var center = area_size / 2; // NOTE: Width and Height must be equal.
        var newDirX = (            event.offsetX) - center;
        var newDirY = (area_size - event.offsetY) - center;
        var newDirLen = Math.sqrt(newDirX * newDirX + newDirY * newDirY);
        if (this.model == null || newDirLen < this.model.r * area_size) {
            newDirX = 0;
            newDirY = 0;
        }
        this.setNewDirection(newDirX, newDirY);
        return false;
    }

    @HostListener("mouseout")
    onMouseOut() {
        this.setNewDirection(0, 0);
    }

    setNewDirection(newDirX: number, newDirY: number) {
        if (newDirX != this.dirX || newDirY != this.dirY) {
            this.dirX = newDirX;
            this.dirY = newDirY;
            this.alkaniGameService.sendDirection(newDirX, newDirY);
        }
    }

    redraw() {
        var ctx = this.context;
        var area_size = ctx.canvas.width;    // NOTE: Width and Height must be equal.
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
            ctx.arc(area_size / 2 + f.x * area_size, area_size / 2 - f.y * area_size, f.r * area_size, 0, 2 * Math.PI);
            ctx.fillStyle = "blue";
            ctx.fill();
        }
        for (var p of this.model.players) {
            ctx.beginPath();
            ctx.arc(area_size / 2 + p.x * area_size, area_size / 2 - p.y * area_size, 10, 0, 2 * Math.PI); // TODO
            ctx.fillStyle = "yellow";
            ctx.fill();
        }
    }
}
