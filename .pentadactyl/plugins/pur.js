
// Vimperator Plugin: 'pinboard'
// Last Change: 22-Jan-2011
// License: MIT
// Maintainer: Jon Austin <jon@jonaustin.org>
// URL: 
// Usage: Use :pinboard <tags delimited by spaces> command
// Usage: if successfully posted you will see "done" echoed

group.commands.add(['pur'], "Save page as a bookmark on pinboard",
                        function(args) {
                            location.href='http://purifyr.com/?url='+encodeURIComponent(location.href)
                            url += "&url=" + encodeURIComponent(buffer.URL);
                            url += "&description=" + encodeURIComponent(buffer.title);
                            url += "&tags=" + encodeURIComponent("purifyr readkindletmp readkindle"+args.string);
                            
                            var xhr = new XMLHttpRequest();
                            xhr.open("POST", url, false);
                            xhr.send(null);
                            var xml = (new DOMParser()).parseFromString(xhr.responseText, "text/xml");
                            var status = xml.getElementsByTagName('result')[0].getAttribute('code');
                            
                            dactyl.echo(status);
                        });
