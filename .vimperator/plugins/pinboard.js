(function() {

  function buildURL(url, queries) {
    var result = [url];
    for (var i = 0; i < queries.length; i++)
      if (queries[i][1])
        result.push('&', queries[i][0], '=', encodeURIComponent(queries[i][1]));
    return result.join("");
  }

  function httpPost(url, callback) {
    try {
      let xmlhttp = new XMLHttpRequest();
      if (callback)
        xmlhttp.onreadystatechange = function() {
          if (xmlhttp.readyState == 4)
            callback(xmlhttp);
        }
      xmlhttp.open('POST', url, !!callback);
      xmlhttp.send(null);
      return xmlhttp;
    } catch (e) {
      liberator.log('Error opening ' + url + ': ' + e, 1);
    }
  }

  commands.addUserCommand(['pin[board]'], 'Bookmark page at pinboard.in', function(args) {
    var url = buffer.URL;
    var description = args['-d'] || buffer.title || url;
    var note = args['-n'] || String(window.content.getSelection());
    var shared = args['-s'] ? 'yes' : 'no';
    var toread = args['-r'] ? 'no' : 'yes';

    httpPost(buildURL('https://api.pinboard.in/v1/posts/add?',
                      [['url', url],
                       ['description', description],
                       ['extended', note],
                       ['tags', args.join(" ")],
                       ['shared', shared],
                       ['toread', toread]]), function(xhr) {
      var result = xhr.status == 200 ?
                    xhr.responseXML.documentElement.getAttribute('code') :
                    'failed with status ' + xhr.status;
      liberator.echo('Bookmarking ' + url + ' at pinboard.in ' + result);
    });
  }, {
    argCount: '*',
    options: [[['-d', 'description'], commands.OPTION_STRING, null, function() [[buffer.title]]],
              [['-n', 'note'], commands.OPTION_STRING, null, null],
              [['-s', 'shared'], commands.OPTION_NOARG, null, null],
              [['-r', 'markasread'], commands.OPTION_NOARG, null, null]],
    completer: function(context) {
      if (context.result) {
        context.completions = context.result;
        return;
      }

      context.title = ['Tags', 'Type'];
      context.incomplete = true;

      var xhr = util.httpGet(buildURL('https://api.pinboard.in/v1/posts/suggest?',
                                      [['url', buffer.URL]]), function(xhr) {
        context.incomplete = false;

        if (xhr.status != 200) {
          context.completions = context.result = [];
          return;
        }

        var result = [];
        var tags = xhr.responseXML.documentElement.getElementsByTagName('*');
        for (var i = 0; i < tags.length; i++)
          result.push([tags[i].textContent, tags[i].localName]);

        context.completions = context.result = result;
      });
      context.cancel = function() xhr.abort();
    }
  }, true);

})();
