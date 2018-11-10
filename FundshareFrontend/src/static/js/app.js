const now = new Date();
const today = {
  day: now.getDate(),
  month: now.getMonth() + 1,
  year: now.getFullYear()
};
const app = Elm.Main.init({
  node: document.getElementById("elm-main"),
  flags: today
});
