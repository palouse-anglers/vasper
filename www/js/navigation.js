(function() {
  "use strict";

  var SET_ACTIVE_PAGE_HANDLER_NAME = "set-active-page";
  var SET_ACTION_ENABLED_HANDLER_NAME = "set-action-enabled";

  function normalizePayload(payload) {
    if (typeof payload === "string") {
      return { page: payload };
    }
    return payload || {};
  }

  function navIdsForPage(page) {
    if (!page || page === "chat") {
      return ["hdr_chat", "nav_chat"];
    }
    return ["hdr_" + page, "nav_" + page];
  }

  function normalizeNavValue(value) {
    return String(value || "")
      .toLowerCase()
      .replace(/[^a-z0-9]+/g, "");
  }

  function findTabAnchor(tabsetId, valueLike) {
    var root = document.getElementById(tabsetId);
    if (!root) return null;

    var anchors = root.querySelectorAll("a[data-value]");
    if (!anchors || anchors.length === 0) return null;

    var wanted = normalizeNavValue(valueLike);
    for (var i = 0; i < anchors.length; i += 1) {
      var candidate = anchors[i].getAttribute("data-value");
      if (normalizeNavValue(candidate) === wanted) {
        return anchors[i];
      }
    }
    return null;
  }

  function showTabByAnchor(anchor) {
    if (!anchor) return false;

    if (window.bootstrap && window.bootstrap.Tab) {
      window.bootstrap.Tab.getOrCreateInstance(anchor).show();
      return true;
    }

    if (typeof anchor.click === "function") {
      anchor.click();
      return true;
    }

    return false;
  }

  function activateHiddenNavPane(tabsetId, value) {
    var anchor = findTabAnchor(tabsetId, value);
    return showTabByAnchor(anchor);
  }

  function notifyServerClientNav(page) {
    if (!window.Shiny || typeof window.Shiny.setInputValue !== "function") return;
    window.Shiny.setInputValue(
      "client_nav_request",
      { page: page, nonce: Date.now() },
      { priority: "event" }
    );
  }

  function scrollChatToBottom() {
    var root = document.getElementById("main_chat");
    if (!root) return;
    var scroller = root.querySelector("shiny-chat-messages") || root;
    scroller.scrollTop = scroller.scrollHeight;
  }

  function handleSetActivePage(payload) {
    var opts = normalizePayload(payload);
    var ids = navIdsForPage(opts.page || "chat");

    var activeNodes = document.querySelectorAll(
      ".app-header-link.is-active, .app-header-menu .action-button.is-active"
    );
    activeNodes.forEach(function(node) {
      node.classList.remove("is-active");
      node.removeAttribute("aria-current");
    });

    ids.forEach(function(id) {
      var node = document.getElementById(id);
      if (!node) return;
      node.classList.add("is-active");
      if (id.indexOf("hdr_") === 0) {
        node.setAttribute("aria-current", "page");
      }
    });
  }

  function activatePageClientSide(page) {
    var targetPage = page || "chat";

    if (targetPage === "chat") {
      activateHiddenNavPane("app_view", "chat");
      handleSetActivePage({ page: "chat" });
      scrollChatToBottom();
      notifyServerClientNav("chat");
      return;
    }

    activateHiddenNavPane("app_view", "pages");
    activateHiddenNavPane("pages_nav", targetPage);
    handleSetActivePage({ page: targetPage });
    notifyServerClientNav(targetPage);
  }

  function parsePageFromNavId(id) {
    if (!id) return null;
    if (id === "hdr_chat" || id === "nav_chat") return "chat";
    if (id.indexOf("hdr_") === 0) return id.slice(4);
    if (id.indexOf("nav_") === 0) return id.slice(4);
    return null;
  }

  function registerClientNavInterceptors() {
    if (window.__vasperClientNavInterceptorRegistered) {
      return;
    }
    window.__vasperClientNavInterceptorRegistered = true;

    document.addEventListener("click", function(event) {
      var disabledAction = event.target && event.target.closest
        ? event.target.closest("a.action-button[data-disabled='true']")
        : null;
      if (disabledAction) {
        event.preventDefault();
        event.stopPropagation();
        return;
      }

      var link = event.target && event.target.closest
        ? event.target.closest("a.action-button[id^='hdr_'], a.action-button[id^='nav_']")
        : null;
      if (!link) return;

      var page = parsePageFromNavId(link.id);
      if (!page) return;

      event.preventDefault();
      activatePageClientSide(page);
    }, true);
  }

  function setActionEnabled(payload) {
    var opts = normalizePayload(payload);
    var id = opts.id;
    if (!id) return;

    var node = document.getElementById(id);
    if (!node) return;

    var enabled = opts.enabled !== false;

    if (enabled) {
      node.removeAttribute("data-disabled");
      node.removeAttribute("aria-disabled");
      node.classList.remove("is-disabled");
      node.removeAttribute("tabindex");
      return;
    }

    node.setAttribute("data-disabled", "true");
    node.setAttribute("aria-disabled", "true");
    node.classList.add("is-disabled");
    node.tabIndex = -1;
  }

  function registerNavigationHandlers() {
    if (!window.Shiny || typeof window.Shiny.addCustomMessageHandler !== "function") {
      window.setTimeout(registerNavigationHandlers, 50);
      return;
    }

    if (window.__vasperNavigationHandlerRegistered) {
      return;
    }
    window.__vasperNavigationHandlerRegistered = true;

    window.Shiny.addCustomMessageHandler(SET_ACTIVE_PAGE_HANDLER_NAME, handleSetActivePage);
    window.Shiny.addCustomMessageHandler(SET_ACTION_ENABLED_HANDLER_NAME, setActionEnabled);
  }

  registerClientNavInterceptors();
  registerNavigationHandlers();
})();
