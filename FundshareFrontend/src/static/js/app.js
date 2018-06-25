import "phoenix_html"
import { Socket } from "phoenix"

const Elm = require("../elm/src/Main");
const app = Elm.Main.embed(document.getElementById("elm-main"), localStorage.session || null);

app.ports.storeSession.subscribe(function(session) {
    localStorage.session = session;
});
window.addEventListener("storage", function(event) {
    if (event.storageArea === localStorage && event.key === "session") {
        app.ports.onSessionChange.send(event.newValue);
    }
}, false);
