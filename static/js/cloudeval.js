var cloudeval = angular.module ('cloudeval', []);

// A directive that creates a code editor for a fragment of Haskell
// code. The editor supports evaluation, showing compiler errors in
// the right places, etc.
cloudeval.directive ('haskell', ['$timeout', '$http', function ($timeout, $http) {
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
      var editor = CodeMirror(editorElem[0], {
        mode: 'haskell',
        theme: 'pastel-on-dark'
      });

      // Marks compiler errors in code
      var errorMarkers = [];

      var resizeEditor = function () {
        var height = (editor.lastLine () - editor.firstLine () + 2) * editor.defaultTextHeight ();
        editor.setSize ('100%', height + 'px');
      };

      setInterval(resizeEditor, 1000);

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

      // Remove all error markers and related popovers
      var clearErrorFragments = function () {
        for (var i = 0; i < errorMarkers.length; i++) {
          errorMarkers[i].clear();
        }
        errorMarkers = [];
      };

      var addErrorFragment = function (fragment) {
        var position = fragment.position;
        errorMarkers.push (editor.markText (
          {line: position.start.row - 1, ch: position.start.col - 1},
          {line: position.end.row - 1, ch: position.end.col - 1}, {
            className: 'haskell-compiler-error',
            title: fragment.message
          }
        ));
      };

      // Extracts the error fragments from the specified list that can
      // be shown in the code editor, and shows them.
      var markErrors = function (fragments) {
        fragments = fragments || [];

        clearErrorFragments ();

        for (var i = 0; i < fragments.length; i++) {
          // Can the error be associated with a position in the code?
          if (fragments[i].position) {
            addErrorFragment(fragments[i]);
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

      // Sync angular model with editor
      ngModel.$render = function () {
        editor.setValue (ngModel.$viewValue);
        queueCompilation ();
        resizeEditor ();
      };

      $scope.$watch ('expression', queueCompilation);

      // Sync editor with angular model
      editor.on ('change', function () {
        ngModel.$setViewValue (editor.getValue ());
        queueCompilation ();
        resizeEditor ();
      });
    }
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
