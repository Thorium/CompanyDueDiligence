// Hash links for the demo: #/company/<code>/<number>, #/search/<term> and #/person/<name>. Hash routes need no server rewrite, so they work
// under GitHub Pages' sub-path. replaceState doesn't fire hashchange, so writing a link never runs it again.
window.cddLinks = {
    hash: () => location.hash,
    replace: (hash) => history.replaceState(null, '', location.href.split('#')[0] + hash),
    listen: (listener) => window.addEventListener('hashchange', () => listener.invokeMethodAsync('HashChanged', location.hash)),
    // True when copied; when the browser refuses the clipboard, the link goes in the address bar instead and the answer is false.
    copy: async (hash) => {
        const link = location.href.split('#')[0] + hash;
        try { await navigator.clipboard.writeText(link); return true; }
        catch { history.replaceState(null, '', link); return false; }
    }
};

// The local server's access token: taken from ?token= in the address the server printed, kept in this browser, and left out of the address bar.
window.cddToken = () => {
    const url = new URL(location.href);
    const given = url.searchParams.get('token');
    if (given) {
        localStorage.setItem('cdd-server-token', given);
        url.searchParams.delete('token');
        history.replaceState(null, '', url.toString());
    }
    return localStorage.getItem('cdd-server-token') || '';
};
