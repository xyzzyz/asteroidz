function Game() {
    var canvas = $("#asteroids_canvas");
    this.ctx = canvas[0].getContext("2d");
    this.width = canvas.attr('width');
    this.height = canvas.attr('height');

    this.gameObjects = [];
    this.playerShip = new SpaceShip(0.5, 0.5, 0);
    this.playerProjectile = new Projectile(0.7, 0.7);
    this.playerAsteroid = new Asteroid(0.23, 0.23, 1);
    this.gameObjects[0] = this.playerShip;
    this.gameObjects[1] = this.playerProjectile;
    this.gameObjects[2] = this.playerAsteroid;
}

Game.prototype.translateTo = function(sprite) {
    this.ctx.translate(sprite.x*this.width, sprite.y*this.height);
};

Game.prototype.getState = function(gameStateJSON) {
    var inf = JSON.parse(gameStateJSON);
    this.gameObjects = [];
    for (var i = 0; i < inf.length; ++i) {
        if (inf[i].type === "spaceship") {
            this.gameObjects[i] = new SpaceShip(inf[i].x, inf[i].y, inf[i].rot);
        } else if (inf[i].type === "projectile") {
            this.gameObjects[i] = new Projectile(inf[i].x, inf[i].y);
        } else if (inf[i].type === "asteroid") {
            this.gameObjects[i] = new Asteroid(inf[i].x, inf[i].y, inf[i].size);
        }
    }
}

Game.prototype.update = function() {
    var tmp0 = new SpaceShip(0.4, 0.4, 0.2);
    tmp0.type = "spaceship";
    var tmp1 = new SpaceShip(0.2, 0.2, 0.3);
    tmp1.type = "spaceship";
    var tmp2 = new Projectile(0.3, 0.3);
    tmp2.type = "projectile";
    var tmp3 = new Asteroid(0.1, 0.1, 5);
    tmp3.type = "asteroid";
    var tmpObjects = [tmp0, tmp1, tmp2, tmp3];
    gameStateJSON = JSON.stringify(tmpObjects);
    
    this.getState(gameStateJSON);
    this.redraw()

};

Game.prototype.redraw = function() {
    this.ctx.fillStyle = "black";
    this.ctx.fillRect(0, 0, this.width, this.height);

    for (var i = 0; i < this.gameObjects.length; ++i) {
        this.ctx.save();
        this.translateTo(this.gameObjects[i]);
        this.gameObjects[i].draw(this.ctx);
        this.ctx.restore();
    }
};

function SpaceShip(x, y, rot) {
    this.x = x;
    this.y = y;
    this.rot = rot;
}

SpaceShip.prototype.draw = function(ctx) {
    console.log("SpaceShip.draw()");
    ctx.strokeStyle = "white";
    ctx.rotate(this.rot);
    ctx.beginPath();
    ctx.moveTo(-10, 15);
    ctx.lineTo(0, -15);
    ctx.lineTo(10, 15);
    ctx.fill();
    ctx.closePath();
    ctx.stroke();
};

function Asteroid(x, y, size) {
    this.x = x;
    this.y = y;
    this.size = size;
}

Asteroid.prototype.draw = function(ctx) {
    console.log("Asteroid.draw()");
    ctx.strokeStyle = "white";
    ctx.beginPath();
    ctx.arc(0, 0, this.size*10, this.size*10, Math.PI*2, true); 
    ctx.fill();
    ctx.closePath();
    ctx.stroke();
};

function Projectile(x, y) {
    this.x = x;
    this.y = y;
}

Projectile.prototype.draw = function(ctx) {
    console.log("Projectile.draw()");
    ctx.strokeStyle = "white";
    ctx.fillStyle = "white";
    ctx.beginPath();
    ctx.arc(0, 0, 3, 3, Math.PI*2, true); 
    ctx.closePath();
    ctx.fill();
};

var game;
var ws;

$(document).ready(
    function() {
        game = new Game();
        game.redraw();

        //ws = new WebSocket("ws://192.168.1.100:9160");
        //ws.onopen = function(){
            /*Send a small message to the console once the connection is established */
        //    console.log('Connection open!');
        //    ws.send("asd");
        //};

        $("body").keypress(
            function(e) {
                switch(e.which) {
                case 37: game.thrustLeft = true; break; //left arrow
                case 38: game.thrustUp = true; break; //up arrow 
                case 39: game.thrustRight = true; break; //right arrow
                default: return;
                }
                e.preventDefault();
                return true;
            });
        $.doTimeout('main_loop', 2000, function () { game.update(); return true; });
    }
);
