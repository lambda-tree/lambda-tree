"use strict";

require("./styles.scss")
const {Elm} = require("./Main")

// Get flags for Elm App from cache
let flags = null
try {
    const cachedString = localStorage.getItem("cache")
    if (cachedString) {
        flags = JSON.parse(cachedString)
    }
} catch (e) {
}

// Init Elm App
var app = Elm.Main.init({flags: flags})

// Subscribe to Elm ports
app.ports.cache.subscribe((data) => {
    localStorage.setItem("cache", JSON.stringify(data))
    console.log(localStorage.getItem("cache"))
})

app.ports.log.subscribe((data) => {
    console.log("LOG: ", data)
})
