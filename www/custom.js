window.Shiny = window.Shiny || {};

function flagMarkup(item, kind) {
  const lang = item.value;

  const flags = {
    nb:  "norway.svg",
    nn:  "norway.svg",
    sv: "sv.svg",
    da: "da.svg",
    fi: "fi.svg",
    se:  "sami.svg",
    // smj deaktivert: oversettelsen er forsøkt, men ikke kvalitetssikret nok til utgivelse.
    fkv: "kven.svg",
    fr:  "france.svg",
    es:  "es.svg",
    de:  "germany.svg",
    pl:  "pl.svg",
    lt:  "lt.svg",
    uk: "ua.svg",
    en:  ""
  };

  const names = {
    nb:  "Bokmål",
    nn:  "Nynorsk",
    sv: "Svenska",
    da: "Dansk",
    fi:  "Suomi",
    se:  "Davvisámegiella",
    fkv: "Kainuun kieli",
    fr:  "Français",
    es:  "Español",
    de:  "Deutsch",
    pl:  "Polski",
    lt:  "Lietuvių",
    uk:  "Українська",
    en:  "English"
  };

  const flag = flags[lang] || "";
  const label = names[lang] || lang;
  const rootTag = kind === "item" ? "span" : "div";
  const rootClass = kind === "item" ? "flag-choice flag-choice--item" : "flag-choice flag-choice--option";

  return `
    <${rootTag} class="${rootClass}">
      ${flag ? `<img src="${flag}" height="15" class="flag-choice__flag" alt="">` : ""}
      <span class="flag-choice__label">${label}</span>
    </${rootTag}>
  `;
}

window.Shiny.renderFlagOption = function(item) {
  return flagMarkup(item, "option");
};

window.Shiny.renderFlagItem = function(item) {
  return flagMarkup(item, "item");
};

function sendBrowserLanguageToShiny() {
  if (!window.Shiny || typeof window.Shiny.setInputValue !== "function") {
    return false;
  }

  const languages = navigator.languages || [navigator.language || ""];
  const browserLang = (languages[0] || "").toLowerCase();

  window.Shiny.setInputValue("browser_lang", browserLang, { priority: "event" });
  window.Shiny.setInputValue("browser_langs", languages.join(", "), { priority: "event" });
  return true;
}

function scheduleBrowserLanguageRetry(maxAttempts = 20, delayMs = 250) {
  let attempts = 0;

  function trySend() {
    attempts += 1;

    if (sendBrowserLanguageToShiny() || attempts >= maxAttempts) {
      return;
    }

    window.setTimeout(trySend, delayMs);
  }

  trySend();
}

document.addEventListener("DOMContentLoaded", function() {
  scheduleBrowserLanguageRetry();
});

document.addEventListener("shiny:connected", function() {
  scheduleBrowserLanguageRetry();
});

if (window.Shiny && typeof window.Shiny.addCustomMessageHandler === "function") {
  window.Shiny.addCustomMessageHandler("redirect-to-url", function(message) {
    if (message && message.url) {
      window.location.href = message.url;
    }
  });
}
