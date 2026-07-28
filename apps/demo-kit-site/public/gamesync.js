/* GameSync — real-time shared state for classroom games.
 * Served at /gamesync.js. Generated multiplayer games load this via <script src="/gamesync.js">.
 * Protocol: each player owns a top-level key in shared state identified by their player ID.
 * sync.set({ [playerId]: data }) → server merges into state → broadcasts to all players.
 */
(function (global) {
  'use strict';

  function GameSync(room, player) {
    this.room    = room   || 'default';
    this.player  = player || ('p' + Math.random().toString(36).slice(2, 10));
    this._state   = {};
    this._players = [];
    this._onState = [];
    this._ws      = null;
    this._retryMs = 1000;
    this._dead    = false;
    this._connect();
  }

  GameSync.prototype._connect = function () {
    if (this._dead) return;
    var self = this;
    var proto = location.protocol === 'https:' ? 'wss:' : 'ws:';
    var url   = proto + '//' + location.host + '/ws?room=' + encodeURIComponent(this.room);
    var ws    = new WebSocket(url);
    this._ws  = ws;

    ws.onopen = function () {
      self._retryMs = 1000;
      ws.send(JSON.stringify({ type: 'join', player: self.player }));
    };

    ws.onmessage = function (e) {
      var msg;
      try { msg = JSON.parse(e.data); } catch (ex) { return; }
      if (msg.type === 'state') {
        self._state   = msg.state   || {};
        self._players = msg.players || [];
        self._onState.forEach(function (cb) { cb(self._state, self._players); });
      }
    };

    ws.onclose = function () {
      if (self._dead) return;
      self._retryMs = Math.min(self._retryMs * 2, 16000);
      setTimeout(function () { self._connect(); }, self._retryMs);
    };

    ws.onerror = function () { ws.close(); };
  };

  /* Merge patch into shared state under your player key (or any top-level keys). */
  GameSync.prototype.set = function (patch) {
    if (!patch || typeof patch !== 'object') return;
    if (this._ws && this._ws.readyState === WebSocket.OPEN) {
      this._ws.send(JSON.stringify({ type: 'set', player: this.player, data: patch }));
    }
  };

  /* Replace entire shared state (instructor reset). */
  GameSync.prototype.reset = function (initial) {
    if (this._ws && this._ws.readyState === WebSocket.OPEN) {
      this._ws.send(JSON.stringify({ type: 'reset', state: initial || {} }));
    }
  };

  /* Register a callback invoked on every state update. Returns `this` for chaining. */
  GameSync.prototype.onState = function (cb) {
    this._onState.push(cb);
    if (Object.keys(this._state).length || this._players.length) {
      cb(this._state, this._players);
    }
    return this;
  };

  /* Disconnect permanently. */
  GameSync.prototype.destroy = function () {
    this._dead = true;
    if (this._ws) this._ws.close();
  };

  Object.defineProperty(GameSync.prototype, 'state',   { get: function () { return this._state;   } });
  Object.defineProperty(GameSync.prototype, 'players', { get: function () { return this._players; } });

  global.GameSync = GameSync;
})(typeof globalThis !== 'undefined' ? globalThis : window);
