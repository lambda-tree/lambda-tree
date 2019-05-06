"use strict";

import "./styles.scss"
import { Elm } from "./Main"
import "dragscroll"

// Get flags for Elm App from cache
const cacheKey = "cache"
let flags = null
try {
    const cachedString = localStorage.getItem(cacheKey)
    if (cachedString) {
        flags = JSON.parse(cachedString)
    }
} catch (e) {
    console.warn("Cannot parse cached object:\n", e)
}

// Init Elm App
var app = Elm.Main.init({flags: flags})

// Subscribe to Elm ports
app.ports.cache.subscribe((data) => {
    localStorage.setItem(cacheKey, JSON.stringify(data))
})

app.ports.log.subscribe((data) => {
    console.log("LOG:", data)
})
