"use strict";

function _instanceof(left, right) { if (right != null && typeof Symbol !== "undefined" && right[Symbol.hasInstance]) { return right[Symbol.hasInstance](left); } else { return left instanceof right; } }

function _typeof(obj) { if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") { _typeof = function _typeof(obj) { return typeof obj; }; } else { _typeof = function _typeof(obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }; } return _typeof(obj); }

function _toConsumableArray(arr) { return _arrayWithoutHoles(arr) || _iterableToArray(arr) || _nonIterableSpread(); }

function _nonIterableSpread() { throw new TypeError("Invalid attempt to spread non-iterable instance"); }

function _iterableToArray(iter) { if (Symbol.iterator in Object(iter) || Object.prototype.toString.call(iter) === "[object Arguments]") return Array.from(iter); }

function _arrayWithoutHoles(arr) { if (Array.isArray(arr)) { for (var i = 0, arr2 = new Array(arr.length); i < arr.length; i++) { arr2[i] = arr[i]; } return arr2; } }

function _classCallCheck(instance, Constructor) { if (!_instanceof(instance, Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } }

function _createClass(Constructor, protoProps, staticProps) { if (protoProps) _defineProperties(Constructor.prototype, protoProps); if (staticProps) _defineProperties(Constructor, staticProps); return Constructor; }

function _possibleConstructorReturn(self, call) { if (call && (_typeof(call) === "object" || typeof call === "function")) { return call; } return _assertThisInitialized(self); }

function _assertThisInitialized(self) { if (self === void 0) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function"); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } }); if (superClass) _setPrototypeOf(subClass, superClass); }

function _wrapNativeSuper(Class) { var _cache = typeof Map === "function" ? new Map() : undefined; _wrapNativeSuper = function _wrapNativeSuper(Class) { if (Class === null || !_isNativeFunction(Class)) return Class; if (typeof Class !== "function") { throw new TypeError("Super expression must either be null or a function"); } if (typeof _cache !== "undefined") { if (_cache.has(Class)) return _cache.get(Class); _cache.set(Class, Wrapper); } function Wrapper() { return _construct(Class, arguments, _getPrototypeOf(this).constructor); } Wrapper.prototype = Object.create(Class.prototype, { constructor: { value: Wrapper, enumerable: false, writable: true, configurable: true } }); return _setPrototypeOf(Wrapper, Class); }; return _wrapNativeSuper(Class); }

function isNativeReflectConstruct() { if (typeof Reflect === "undefined" || !Reflect.construct) return false; if (Reflect.construct.sham) return false; if (typeof Proxy === "function") return true; try { Date.prototype.toString.call(Reflect.construct(Date, [], function () {})); return true; } catch (e) { return false; } }

function _construct(Parent, args, Class) { if (isNativeReflectConstruct()) { _construct = Reflect.construct; } else { _construct = function _construct(Parent, args, Class) { var a = [null]; a.push.apply(a, args); var Constructor = Function.bind.apply(Parent, a); var instance = new Constructor(); if (Class) _setPrototypeOf(instance, Class.prototype); return instance; }; } return _construct.apply(null, arguments); }

function _isNativeFunction(fn) { return Function.toString.call(fn).indexOf("[native code]") !== -1; }

function _setPrototypeOf(o, p) { _setPrototypeOf = Object.setPrototypeOf || function _setPrototypeOf(o, p) { o.__proto__ = p; return o; }; return _setPrototypeOf(o, p); }

function _getPrototypeOf(o) { _getPrototypeOf = Object.setPrototypeOf ? Object.getPrototypeOf : function _getPrototypeOf(o) { return o.__proto__ || Object.getPrototypeOf(o); }; return _getPrototypeOf(o); }

customElements.define("elm-canvas",
/*#__PURE__*/
function (_HTMLElement) {
  _inherits(_class, _HTMLElement);

  function _class() {
    var _this;

    _classCallCheck(this, _class);

    _this = _possibleConstructorReturn(this, _getPrototypeOf(_class).call(this));
    _this.commands = [];
    _this.mounted = false;
    return _this;
  }

  _createClass(_class, [{
    key: "connectedCallback",
    value: function connectedCallback() {
      var _this2 = this;

      // Wait for the inner elements to be rendered before using them
      requestAnimationFrame(function () {
        _this2.canvas = _this2.querySelector("canvas");
        _this2.context = _this2.canvas.getContext("2d");
        _this2.mounted = true;

        _this2.setCanvasDimensions();

        _this2.render();
      });
    }
  }, {
    key: "attributeChangedCallback",
    value: function attributeChangedCallback(name, oldValue, newValue) {
      var _this3 = this;

      if ((name === "width" || name === "height") && oldValue !== newValue) {
        // Wait for Elm to finish rendering and setting its stuff before
        // changing the inner canvas dimensions
        requestAnimationFrame(function () {
          _this3.setCanvasDimensions();
        });
      }
    }
  }, {
    key: "setCanvasDimensions",
    value: function setCanvasDimensions() {
      if (!this.mounted) return; // Get dimensions from the elm-canvas element. If they are not set, try to
      // get them from the canvas element inside (to support elm-canvas@3.0.3)

      var width = Number(this.getAttribute("width") || this.canvas.getAttribute("width"));
      var height = Number(this.getAttribute("height") || this.canvas.getAttribute("height")); // var devicePixelRatio = window.devicePixelRatio || 1;

      var devicePixelRatio = 2;
      this.canvas.style.width = width;
      this.canvas.style.height = height;
      this.canvas.width = width * devicePixelRatio;
      this.canvas.height = height * devicePixelRatio; // Reset current transformation matrix to the identity matrix

      this.context.setTransform(1, 0, 0, 1, 0, 0);
      this.context.scale(devicePixelRatio, devicePixelRatio);
    }
  }, {
    key: "render",
    value: function render() {
      if (!this.mounted) return; // Iterate over the commands in reverse order as that's how the Elm side
      // builds them as with the linked lists

      for (var i = this.commands.length - 1; i >= 0; i--) {
        this.execCommand(this.commands[i]);
      }

      this.commands = [];
    }
  }, {
    key: "execCommand",
    value: function execCommand(cmd) {
      if (cmd.type === "function") {
        var _this$context;

        (_this$context = this.context)[cmd.name].apply(_this$context, _toConsumableArray(cmd.args));
      } else if (cmd.type === "field") {
        this.context[cmd.name] = cmd.value;
      }
    }
  }, {
    key: "cmds",
    set: function set(values) {
      this.commands = values;
      this.render();
    }
  }], [{
    key: "observedAttributes",
    get: function get() {
      return ["width", "height"];
    }
  }]);

  return _class;
}(_wrapNativeSuper(HTMLElement)));
