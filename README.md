scrz supervisor
---------------

The `supervisor` manages one or more child processes (services). In that
regard it's quite similar to Ubuntu's [upstart][upstart] or D. J. Bernstein's
[daemontools][daemontools]. However, instead of simply reading configuration
from the local harddrive, it fetches it dynamically from an `authority`. This
allows you to reconfigure or deploy new versions of your application without
having to log into the servers.

Currently we only support one type of `authority`: The [scrz http
authority][scrz-http-authority] server. It is a Ruby on Rails project with
a simple web interface.

    $ scrz http://<http-authority-server>/<service>



[upstart]: http://upstart.ubuntu.com/
[daemontools]: http://cr.yp.to/daemontools.html
[scrz-http-authority]: https://github.com/wereHamster/scrz-http-authority
