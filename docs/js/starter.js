(function(){
    'use strict';

    var node = document.getElementById("game");

    var flags = node.offsetHeight < 100 ? {width: window.innerWidth, height: window.innerHeight - 5} : {width: node.offsetWidth, height: node.offsetHeight}

    window.app = Elm.Main.init({
        node: node,
        flags: flags
    });

    if (app.ports) {


        if (app.ports.onblur) {
            window.onblur = function () {
                app.ports.onblur.send(null);
            }
        }

        if (app.ports.playSound) {
            var sounds = {
                startGame: new Howl({
                    src: ['sounds/button-2.mp3'],
                    volume: 0.1
                }),
                shot: new Howl({
                    src: ['sounds/button-3.mp3'],
                    volume: 0.1
                }),
                playerCollision: new Howl({
                    src: ['sounds/button-11.mp3'],
                    volume: 0.1
                }),
                shotCollision: new Howl({
                    src: ['sounds/button-4.mp3'],
                    volume: 0.1
                }),
                gameMusic: new Howl({
                    src: ['sounds/bensound-dubstep.mp3'],
                    volume: 0.1
                }),
                menuMusic: new Howl({
                    src: ['sounds/bensound-scifi.mp3'],
                    volume: 0.1
                }),
                toggle: new Howl({
                    src: ['sounds/button-31.mp3'],
                    volume: 0.1
                }),
            };

            window.playSound = function (name) {
                var sound = sounds[name];
                sound.play();
            };
            window.stopSound = function (name) {
                var sound = sounds[name];
                sound.stop();
            };
            window.pauseSound = function (name) {
                var sound = sounds[name];
                sound.pause();
            };

            app.ports.playSound.subscribe(playSound);
            app.ports.stopSound.subscribe(stopSound);
            app.ports.pauseSound.subscribe(pauseSound);
        }
    }
}());
