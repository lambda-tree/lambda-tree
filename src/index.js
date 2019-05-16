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

// Caching model
app.ports.cache.subscribe((data) => {
    localStorage.setItem(cacheKey, JSON.stringify(data))

    // Add the text expansion handlers when the model changes (new inputs appear)
    addLambdaExpressionInputShortcutExpansionHandlers()
})

// Logging info to JS
app.ports.log.subscribe((data) => {
    console.log("LOG:", data)
})

// Helpers

// Expand shortcuts for lambda expression symbols content of input fields on input with
const onExprInput = (e) => {
    const el = e.target

    const originalValue = el.value
    const originalPosition = el.selectionStart

    const symbols = {
        lambda: "λ"
        , capitalLambda: "Λ"
        , forAll: "∀"
        , arrow: "→"
    }

    const replacedValue = el.value
        .replace("\\", symbols.lambda)
        .replace(/lambda /g, symbols.lambda)
        .replace(/Lambda /g, symbols.capitalLambda)
        .replace(/\|/g, symbols.capitalLambda)
        .replace(/\^/g, symbols.capitalLambda)
        .replace(/Let /i, "let ")
        .replace(/In /i, "in ")
        .replace(/forall /i, symbols.forAll)
        .replace(/->/, symbols.arrow)
        .replace(/If /i, "if ")
        .replace(/Then /i, "then ")
        .replace(/Else /i, "else ")


    if (replacedValue != originalValue) {
        el.value = replacedValue
        const newPosition = originalPosition + (el.value.length - originalValue.length)
        el.selectionStart = newPosition
        el.selectionEnd = newPosition
        el.dispatchEvent(new Event("input"));
    }
}

// Add to all inputs with the class. Encapsulate if possible to a web component
const addLambdaExpressionInputShortcutExpansionHandlers = () => {
    document.querySelectorAll('input.expand-lambda-shortcuts')
        .forEach(input => input.oninput = onExprInput)
}

addLambdaExpressionInputShortcutExpansionHandlers()
