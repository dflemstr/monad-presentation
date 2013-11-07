angular.module ('cloudeval', [])
  .directive ('haskell', ['$timeout', '$http', function ($timeout, $http) {
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
        var compilationTimeout;
        var editorElem = $element.find ('.haskell-editor');
        var editor = ace.edit (editorElem[0]);
        var Range = ace.require ('ace/range').Range;
        var errorMarkers = [];
        var elemsWithPopovers = [];

        var htmlEscape = function (str) {
          return String(str)
            .replace(/&/g, '&amp;')
            .replace(/"/g, '&quot;')
            .replace(/</g, '&lt;')
            .replace(/>/g, '&gt;');
        };

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

        $scope.withoutPosition = function (fragment) {
          return !fragment.position;
        };

        $scope.notEmpty = function (object) {
          return object && Object.keys(object).length > 0;
        };

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

        var clearErrors = function () {
          var session = editor.getSession ();
          clearErrorPopovers ();
          for (var i = 0; i < errorMarkers.length; i++) {
            session.removeMarker (errorMarkers[i]);
          }
          errorMarkers = [];
        };

        var drawCompilerError = function (n, error) {
          var betterErrorMessage =
            ('<p>' + htmlEscape(error) + '</p>')
            .replace(/\n\s+/g, '</p><p>')
            .replace(/<p>(?!.*?<p>)/, function(m, w) {
              return '<p class="bottom">'
            })
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

        var markErrors = function (fragments) {
          var session = editor.getSession ();
          fragments = fragments || [];

          clearErrors ();

          for (var i = 0; i < fragments.length; i++) {
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

        var compile = function () {
          $http.post ('http://localhost:3000/evaluate', ngModel.$viewValue, {
            headers: {
              'Content-Type': 'text/x-haskell'
            },
            params: {
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

        var queueCompilation = function () {
          if (compilationTimeout) {
            $timeout.cancel (compilationTimeout);
          }
          compilationTimeout = $timeout (compile, 300);
        };

        editor.setTheme ('ace/theme/' + ($attrs.theme || 'tomorrow_night'));
        editor.getSession ().setMode ('ace/mode/haskell');

        editor.setFontSize (32);
        editor.setShowPrintMargin (false);
        editor.setShowInvisibles (true);

        ngModel.$render = function () {
          editor.setValue (ngModel.$viewValue, 1);
          queueCompilation ();
          resizeEditor (editor, editorElem);
        };

        $scope.$watch ('expression', queueCompilation);

        editor.on ('change', function () {
          ngModel.$setViewValue (editor.getValue ());
          queueCompilation ();
          resizeEditor (editor, editorElem);
        });

        editor.getSession ().addEventListener ('changeFrontMarker', function () {
          for (var i = 0; i < elemsWithPopovers.length; i++) {
            elemsWithPopovers[i].popover ('hide');
          }
          elemsWithPopovers = [];
        });

        editorElem.popover ({
          selector: '.haskell-compiler-error',
          trigger: 'hover',
          placement: 'auto',
          container: 'body',
          html: true,
          delay: { show: 300, hide: 1000 }
        });

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

        editorElem.on ('mouseleave', clearErrorPopovers);
      },
    };
  }]).directive ('content', function () {
    return {
      restrict: 'E',
      require: 'ngModel',
      terminal: true,
      link: function ($scope, $element, $attrs, ngModel) {
        var content = $element[0].textContent;
        var cut = 0;

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

        var lines = content.split('\n');
        var result = '';

        for (var i = 0; i < lines.length; i++) {
          result += lines[i].substring(cut) + '\n';
        }

        ngModel.$setViewValue (result.trim());
      }
    };
  }).controller ('MainCtrl', ['$scope', function ($scope) {
    $scope.location = window.location.protocol + "//" + window.location.host + window.location.pathname
    // TODO: load code files
  }]);
