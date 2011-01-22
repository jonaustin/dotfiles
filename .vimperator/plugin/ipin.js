// Vimperator Plugin: 'pinboard'
// Last Change: 22-Jan-2011
// License: MIT
// Maintainer: Jon Austin <jon@jonaustin.org>
// URL: 
// Usage: Use :pinboard <tags delimited by spaces> command
// Usage: if successfully posted you will see "done" echoed

commands.addUserCommand(['pinboard'], "Save page as a bookmark on pinboard",
                        function(args) {
                            var url = "https://api.pinboard.in/v1/posts/add?";
                            url += "&url=" + encodeURIComponent("http://www.instapaper.com/m?u=" + escape(buffer.URL));
                            url += "&description=" + encodeURIComponent(buffer.title);
                            url += "&tags=" + encodeURIComponent(args.string);
                            
                            var xhr = new XMLHttpRequest();
                            xhr.open("POST", url, false);
                            xhr.send(null);
                            var xml = (new DOMParser()).parseFromString(xhr.responseText, "text/xml");
                            var status = xml.getElementsByTagName('result')[0].getAttribute('code');
                            
                            liberator.echo(status);
                        });
