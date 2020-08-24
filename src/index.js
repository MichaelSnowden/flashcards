import { Elm } from './Main.elm'

const json = window.localStorage.getItem("flashcards");
const flashcardsObject = json === null ? [] : JSON.parse(json);

const app = Elm.Main.init({
    node: document.getElementById("main"),
    flags: flashcardsObject,
});


app.ports.debug.subscribe(function (message) {
    console.debug(message);
});

app.ports.exportFlashcardsJson.subscribe(function (json) {
    console.log("exporting", json);
    const string = JSON.stringify(json);
    window.localStorage.setItem("flashcards", string);
});