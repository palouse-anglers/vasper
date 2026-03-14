(function() {
  "use strict";

  var HANDLER_NAME = "scroll-chat-bottom";
  var READY_POLL_MS = 40;
  var MAX_READY_TRIES = 20;
  var SETTLE_DELAYS_MS = [50, 140];
  var CLEAR_DELAY_WITH_RESIZE_MS = 260;
  var CLEAR_DELAY_NO_RESIZE_MS = 220;
  var DEBUG = false;

  function debugLog() {
    if (!DEBUG || !window.console || typeof window.console.log !== "function") return;
    var args = Array.prototype.slice.call(arguments);
    args.unshift("[vasperChat]");
    window.console.log.apply(window.console, args);
  }

  function normalizePayload(payload) {
    if (typeof payload === "string") {
      return { id: payload, phase: "after" };
    }
    return payload || {};
  }

  function findChatRoot(payload) {
    var opts = normalizePayload(payload);
    if (!opts.id) return null;
    return {
      root: document.getElementById(opts.id),
      phase: opts.phase || "after"
    };
  }

  function clearPreviousCleanup(root) {
    if (typeof root._chatBottomCleanup === "function") {
      root._chatBottomCleanup();
      root._chatBottomCleanup = null;
    }
  }

  function isScrollableElement(el) {
    if (!el || el === document.body) return false;
    var cs = window.getComputedStyle(el);
    var overflowY = cs.overflowY;
    return (overflowY === "auto" || overflowY === "scroll" || overflowY === "overlay") &&
      el.scrollHeight > el.clientHeight;
  }

  function getMessageScroller(root) {
    return root.querySelector("shiny-chat-messages") || root;
  }

  function getScrollTargets(root) {
    var targets = [];

    var scroller = getMessageScroller(root);
    if (scroller) targets.push(scroller);

    var panel = root.closest(".tab-pane");
    if (panel) targets.push(panel);

    var el = root.parentElement;
    while (el) {
      if (isScrollableElement(el)) targets.push(el);
      el = el.parentElement;
    }

    var doc = document.scrollingElement || document.documentElement;
    if (doc) targets.push(doc);

    return targets;
  }

  function describeTargets(root) {
    return getScrollTargets(root).map(function(el) {
      var tag = (el.tagName || "").toLowerCase() || "unknown";
      var id = el.id ? ("#" + el.id) : "";
      var cls = (typeof el.className === "string" && el.className.trim())
        ? ("." + el.className.trim().replace(/\s+/g, "."))
        : "";
      return tag + id + cls;
    });
  }

  function isChatPanelReady(root) {
    var panel = root.closest(".tab-pane");
    var panelVisible = !panel || panel.classList.contains("active");
    return panelVisible && root.offsetParent !== null;
  }

  function scrollTargetsToBottom(root) {
    var targets = getScrollTargets(root);
    targets.forEach(function(target) {
      target.scrollTop = target.scrollHeight;
    });
    debugLog("scrollTargetsToBottom", {
      targetCount: targets.length,
      targets: describeTargets(root)
    });
    window.scrollTo(0, document.body.scrollHeight);
  }

  function createScrollController(root) {
    var timers = [];
    var poll = null;
    var resizeObserver = null;
    var canceled = false;
    var interactionTargets = [];

    function clearAll() {
      canceled = true;
      debugLog("clearAll");

      timers.forEach(clearTimeout);
      timers = [];

      if (poll) {
        clearInterval(poll);
        poll = null;
      }

      if (resizeObserver) {
        resizeObserver.disconnect();
        resizeObserver = null;
      }

      interactionTargets.forEach(function(node) {
        node.removeEventListener("wheel", clearAll, { passive: true });
        node.removeEventListener("touchstart", clearAll, { passive: true });
        node.removeEventListener("mousedown", clearAll);
        node.removeEventListener("keydown", clearAll);
      });
      interactionTargets = [];

      root._chatBottomCleanup = null;
    }

    function schedule(fn, ms) {
      var timer = setTimeout(function() {
        if (!canceled) fn();
      }, ms);
      timers.push(timer);
    }

    function attachCancelOnUserInteraction() {
      interactionTargets = getScrollTargets(root).filter(function(node) {
        return !!node && node.addEventListener;
      });

      interactionTargets.forEach(function(node) {
        node.addEventListener("wheel", clearAll, { passive: true });
        node.addEventListener("touchstart", clearAll, { passive: true });
        node.addEventListener("mousedown", clearAll);
        node.addEventListener("keydown", clearAll);
      });
    }

    function runSettleSequence() {
      attachCancelOnUserInteraction();

      scrollTargetsToBottom(root);
      requestAnimationFrame(function() {
        if (!canceled) scrollTargetsToBottom(root);
      });

      SETTLE_DELAYS_MS.forEach(function(ms) {
        schedule(function() {
          scrollTargetsToBottom(root);
        }, ms);
      });

      var scroller = getMessageScroller(root);
      if (scroller && typeof ResizeObserver !== "undefined") {
        resizeObserver = new ResizeObserver(function() {
          if (!canceled) scrollTargetsToBottom(root);
        });
        resizeObserver.observe(scroller);
        schedule(clearAll, CLEAR_DELAY_WITH_RESIZE_MS);
      } else {
        schedule(clearAll, CLEAR_DELAY_NO_RESIZE_MS);
      }
    }

    function waitUntilReadyThenRun() {
      if (isChatPanelReady(root)) {
        debugLog("panel ready immediately");
        runSettleSequence();
        return;
      }

      var tries = 0;
      poll = setInterval(function() {
        if (canceled) return;

        tries += 1;
        if (isChatPanelReady(root)) {
          debugLog("panel ready after poll", { tries: tries });
          clearInterval(poll);
          poll = null;
          runSettleSequence();
          return;
        }

        if (tries >= MAX_READY_TRIES) {
          debugLog("panel not ready within max tries", { tries: tries });
          clearAll();
        }
      }, READY_POLL_MS);
    }

    return {
      clearAll: clearAll,
      waitUntilReadyThenRun: waitUntilReadyThenRun
    };
  }

  function handleScrollChatBottom(payload) {
    debugLog("handleScrollChatBottom", payload);
    var chat = findChatRoot(payload);
    if (!chat || !chat.root) {
      debugLog("chat root not found", payload);
      return;
    }

    var root = chat.root;
    var phase = chat.phase;

    clearPreviousCleanup(root);

    // Pre-phase: best effort before panel is shown.
    if (phase === "pre") {
      scrollTargetsToBottom(root);
      return;
    }

    var controller = createScrollController(root);
    root._chatBottomCleanup = controller.clearAll;
    controller.waitUntilReadyThenRun();
  }

  function setDebug(enabled) {
    DEBUG = !!enabled;
    debugLog("debug enabled");
    return DEBUG;
  }

  function debugScroll(id, phase) {
    handleScrollChatBottom({ id: id, phase: phase || "after" });
  }

  function inspect(id) {
    var root = document.getElementById(id);
    if (!root) return { id: id, found: false };

    return {
      id: id,
      found: true,
      ready: isChatPanelReady(root),
      targets: describeTargets(root)
    };
  }

  function exposeDebugApi() {
    window.vasperChat = window.vasperChat || {};
    window.vasperChat.setDebug = setDebug;
    window.vasperChat.debugScroll = debugScroll;
    window.vasperChat.inspect = inspect;
  }

  function registerScrollHandler() {
    if (!window.Shiny || typeof window.Shiny.addCustomMessageHandler !== "function") {
      window.setTimeout(registerScrollHandler, 50);
      return;
    }

    if (window.__vasperScrollChatBottomHandlerRegistered) {
      return;
    }
    window.__vasperScrollChatBottomHandlerRegistered = true;

    window.Shiny.addCustomMessageHandler(HANDLER_NAME, handleScrollChatBottom);
    debugLog("registered handler", HANDLER_NAME);
  }

  exposeDebugApi();
  registerScrollHandler();
})();
