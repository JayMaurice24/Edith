const app = Elm.Main.init({
    node: document.getElementById('elm-root')
}); 

document.addEventListener('keydown', (events) => {
    app.ports.listenToKeyboardEvents.send(event);
});