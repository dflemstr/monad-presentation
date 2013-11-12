var cloudeval = angular.module ('cloudeval', []);

// A directive that creates a code editor for a fragment of Haskell
// code. The editor supports evaluation, showing compiler errors in
// the right places, etc.
cloudeval.directive ('haskell', function () {
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
      // The element that the editor should be rendered in
      var editorElem = $element.find ('div');

      // The actual editor
      var editor = ace.edit (editorElem[0]);

      $scope.withoutPosition = function (fragment) {
        return !fragment.position;
      };

      // Does the specified object have any keys?
      $scope.notEmpty = function (object) {
        return object && Object.keys(object).length > 0;
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
        resizeEditor (editor, editorElem);
      };

      // Sync editor with angular model
      editor.on ('change', function () {
        ngModel.$setViewValue (editor.getValue ());
        resizeEditor (editor, editorElem);
      });
    },
  };
});

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
