var cloudeval = angular.module ('cloudeval', []);

// A directive that creates a code editor for a fragment of Haskell
// code. The editor supports evaluation, showing compiler errors in
// the right places, etc.
cloudeval.directive ('haskell', ['$timeout', '$http', function ($timeout, $http) {
  // Resizes the editor so that it fits snugly around the text content
  var resizeEditor = function (editor, elem) {
    elem.css (
      'height',
      editor.getSession ().getLength () * editor.renderer.lineHeight + 'px');
    editor.resize ();
  };

  return {
    restrict: 'E',
    require: 'ngModel',
    scope: true,
    templateUrl: 'template/haskell.html',
    replace: true,
    link: function ($scope, $element, $attrs, ngModel) {
      // Timeout for code changes; don't send a request to the server
      // on every key press; wait a little each time
      var compilationTimeout;

      // The element that the editor should be rendered in
      var editorElem = $element.find ('.haskell-editor');

      // The actual editor
      var editor = ace.edit (editorElem[0]);

      // Load the Ace range implementation so that we can do selection
      // manipulation
      var Range = ace.require ('ace/range').Range;

      // Marks compiler errors in code
      var errorMarkers = [];

      // Elements that the popovers we create are 'bound to'
      var elemsWithPopovers = [];

      // Do I really have to do this myself? What is this, the 90's?
      var htmlEscape = function (str) {
        return String(str)
          .replace(/&/g, '&amp;')
          .replace(/"/g, '&quot;')
          .replace(/</g, '&lt;')
          .replace(/>/g, '&gt;');
      };

      // Convert an error kind to a human readable message
      $scope.describeError = function (kind) {
        switch (kind) {
        case 'compilation':
          return 'The code does not compile';
        case 'restriction':
          return 'An execution restriction was violated';
        case 'ghc':
          return 'The compiler refused to process the code';
        case 'unknown':
          return 'An unknown error has occurred';
        default:
          return 'Some kind of error happened';
        }
      };

      // Does this error fragment lack a code position?
      $scope.withoutPosition = function (fragment) {
        return !fragment.position;
      };

      // Does the specified object have any keys?
      $scope.notEmpty = function (object) {
        return object && Object.keys(object).length > 0;
      };

      // Hide all popovers that resulted from errors by simulating
      // mouse movement
      var clearErrorPopovers = function () {
        var markers = editor.getSession ().getMarkers (true);

        for (var i = 0; i < markers.length; i++) {
          var marker = markers[i];
          if (marker.mouseInside) {
            var elem = $('[data-error-index="' + marker.errorIndex + '"]');
            elem.trigger ('mouseleave.popover');
            marker.mouseInside = false;
          }
        }
      };

      // Remove all error markers and related popovers
      var clearErrors = function () {
        var session = editor.getSession ();
        clearErrorPopovers ();
        for (var i = 0; i < errorMarkers.length; i++) {
          session.removeMarker (errorMarkers[i]);
        }
        errorMarkers = [];
      };

      // Render a compiler error in the editor
      var drawCompilerError = function (n, error) {
        var betterErrorMessage =
          // Wrap everything in a paragraph
          ('<p>' + htmlEscape(error) + '</p>')
        // Create a new paragraph whenever there's an indented line
          .replace(/\n\s+/g, '</p><p>')
        // Something that is enclosed in GHC `quotes' is probably code
          .replace (/`([^\s]+)'/g, function (m, w) {
            return '<code>' + w + '</code>';
          });

        return function (html, range, left, top, config) {
          this.errorIndex = n;
          html.push(
            '<div class="haskell-compiler-error" style="',
            'height:', config.lineHeight, 'px;',
            'width:', (range.end.column - range.start.column) *
              config.characterWidth, 'px;',
            'top:', top, 'px;',
            'left:', left, 'px;" data-error-index="', n,
            '" data-content="', htmlEscape(betterErrorMessage), '"></div>'
          );
        };
      };

      // Extracts the error fragments from the specified list that can
      // be shown in the code editor, and shows them.
      var markErrors = function (fragments) {
        var session = editor.getSession ();
        fragments = fragments || [];

        clearErrors ();

        for (var i = 0; i < fragments.length; i++) {
          // Can the error be associated with a position in the code?
          if (fragments[i].position) {
            var fragment = fragments[i];
            var position = fragment.position;
            var range =
              new Range (position.start.row - 1,
                         position.start.col - 1,
                         position.end.row - 1,
                         Math.max(position.end.col - 1, position.start.col));

            errorMarkers.push (
              session.addMarker (
                range, 'haskell-compiler-error',
                drawCompilerError(i, fragment.message), true));
          }
        }
      };

      $scope.$watch ('error.fragments', function (fragments) {
        markErrors (fragments);
      });

      // Actually sends the code to the server and evaluates it
      var compile = function () {
        var code = ngModel.$viewValue.replace(/--include:([a-zA-Z]+)/g, function (a, v) {
          return $scope.$parent[v] + '\n';
        });
        $http.get ('evaluate', {
          headers: {
            'Content-Type': 'text/x-haskell'
          },
          params: {
            code: Base64.encode(code),
            expression: $scope.expression
          }
        }).success (function (data, status) {
          console.log ('success', status, data);
          if (data.type !== 'error' || data.error.kind !== 'restriction') {
            $scope.scope = data.scope;
          }
          $scope.evaluations = data.evaluations;
          $scope.error = data.error;
        }).error (function (data, status) {
          console.log ('error', status, data);
        });
      };

      // Queues compilation so that the code gets evaluated eventually
      var queueCompilation = function () {
        if (compilationTimeout) {
          $timeout.cancel (compilationTimeout);
        }
        compilationTimeout = $timeout (compile, 300);
      };

      editor.setTheme ('ace/theme/' + ($attrs.theme || 'tomorrow_night'));
      editor.getSession ().setMode ('ace/mode/haskell');

      editor.setFontSize (24);
      editor.setShowPrintMargin (false);
      editor.renderer.setShowGutter (false);
      //editor.setShowInvisibles (true);

      // Sync angular model with editor
      ngModel.$render = function () {
        editor.setValue (ngModel.$viewValue, 1);
        queueCompilation ();
        resizeEditor (editor, editorElem);
      };

      $scope.$watch ('expression', queueCompilation);

      // Sync editor with angular model
      editor.on ('change', function () {
        ngModel.$setViewValue (editor.getValue ());
        queueCompilation ();
        resizeEditor (editor, editorElem);
      });

      // Make popovers disappear if the editor decides to remove a
      // marker
      editor.getSession ().addEventListener ('changeFrontMarker', function () {
        for (var i = 0; i < elemsWithPopovers.length; i++) {
          elemsWithPopovers[i].popover ('hide');
        }
        elemsWithPopovers = [];
      });

      // Configure a popover on the editor
      editorElem.popover ({
        selector: '.haskell-compiler-error',
        trigger: 'hover',
        placement: 'auto',
        container: 'body',
        html: true,
        delay: { show: 300, hide: 1000 }
      });

      // Since the code markers are behind a div, it doesn't receive
      // mouse events.  So we have to intercept editor mouse events,
      // and proxy them to the relevant error marker, to show a popup.
      editor.on ('mousemove', function (e) {
        var pos = e.getDocumentPosition ();
        var markers = editor.getSession ().getMarkers (true);

        for (var i = 0; i < errorMarkers.length; i++) {
          var marker = markers[errorMarkers[i]];
          var elem = $('[data-error-index="' + marker.errorIndex + '"]');

          if (marker.range.compare (pos.row, pos.column) == 0) {
            if (!marker.mouseInside) {
              elem.trigger ('mouseenter.popover');
              elemsWithPopovers.push (elem);
              marker.mouseInside = true;
            }
          } else {
            if (marker.mouseInside) {
              elem.trigger ('mouseleave.popover');
              marker.mouseInside = false;
            }
          }
        }
      });

      // Just remove all popovers when editor is left
      editorElem.on ('mouseleave', clearErrorPopovers);
    },
  };
}]);

// A simple directive that sets the value of a model from the contents
// of a HTML tag... It additionally cuts away conventional HTML
// indentation
cloudeval.directive ('content', function () {
  return {
    restrict: 'E',
    require: 'ngModel',
    terminal: true,
    link: function ($scope, $element, $attrs, ngModel) {
      var content = $element[0].textContent;
      var cut = 0;

      // Find first non-space character and calculate its indentation
      // level.
      for (var i = 0; i < content.length; i++) {
        var char = content.charAt (i);
        if (char == '\n') {
          cut = 0;
        } else if (/\s/.test (char)) {
          cut++;
        } else {
          break;
        }
      }


      // Cut every line to the indentation level of the first line.
      var lines = content.split('\n');
      var result = '';

      for (var i = 0; i < lines.length; i++) {
        result += lines[i].substring(cut) + '\n';
      }

      ngModel.$setViewValue (result.trim());
    }
  };
});

cloudeval.controller ('MainCtrl', ['$scope', function ($scope) {
  $scope.location = window.location.protocol + "//" + window.location.host + window.location.pathname
  // TODO: load code files
}]);
