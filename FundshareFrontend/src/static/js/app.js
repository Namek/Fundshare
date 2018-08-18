const app = Elm.Main.embed(document.getElementById("elm-main"), localStorage.sessionToken || null);

app.ports.storeToken.subscribe(function(token) {
    localStorage.sessionToken = token;
});
window.addEventListener("storage", function(event) {
    if (event.storageArea === localStorage && event.key === "session") {
        app.ports.onSessionChange.send(event.newValue);
    }
}, false);
