function Game() {
    var canvas = $("#asteroids_canvas");
    this.ctx = canvas[0].getContext("2d");
    this.width = canvas.attr('width');
    this.height = canvas.attr('height');

    this.playerShip = new SpaceShip(0.5, 0.5, 0);
    this.thrust = [0, 0];
}

Game.prototype.translateTo = function(sprite) {
    this.ctx.translate(sprite.x*this.width, sprite.y*this.height);
};

Game.prototype.playerThrust = function(x) {
    
};

Game.prototype.update = function() {
    this.updatePlayerPosition();
};

Game.prototype.redraw = function() {
    this.ctx.fillStyle = "black";
    this.ctx.fillRect(0, 0, this.width, this.height);

    this.ctx.save();
    this.translateTo(this.playerShip);
    this.playerShip.draw(this.ctx);
    this.ctx.restore();
};

function SpaceShip(x, y, rot) {
    this.x = x;
    this.y = y;
    this.rot = rot;
}

SpaceShip.prototype.draw = function(ctx) {
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
    ctx.strokeStyle = 'white';
    ctx.beginPath();
    ctx.arc(0, 0, this.size*10, this.size*10, Math.PI*2, true); 
    ctx.closePath();
    ctx.stroke();
};

function Projectile(x, y) {
    this.x = x;
    this.y = y;
}

Projectile.prototype.draw = function(ctx) {
    ctx.strokeStyle = 'white';
    ctx.fillStyle = 'white';
    ctx.beginPath();
    ctx.arc(0, 0, 3, 3, Math.PI*2, true); 
    ctx.closePath();
    ctx.fill();
};

var game;

$(document).ready(
    function() {
        game = new Game();
        game.redraw();

        $("body").keypress(
            function(e) {
                switch(e.which) {
                case 37: game.thrustLeft = true; break;
                case 38: game.thrustUp = true; break;
                case 39: game.thrustRight = true; break;
                case 40: game.thrustDown = true; break;
                default: return;
                }
                e.preventDefault();
                return true;
            });
        $.doTimeout('main_loop', 500, function () { game.update(); return true; });
    }
);
