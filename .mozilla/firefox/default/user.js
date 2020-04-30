// Based on https://github.com/ghacksuserjs/ghacks-user.js/blob/bd384622db70eaf6893d32a0c1c4b99d5516fa5b/user.js
// Compare with master using https://github.com/ghacksuserjs/ghacks-user.js/compare/HASH1...HASH2

// Enable OpenGL Off-Main-Thread Compositing (OMTC)
user_pref("layers.acceleration.force-enabled", true);
// Allow unsigned Add-ons. Or at least try...
user_pref("xpinstall.signatures.required", false);
// Read userChrome.css and userContent.css
user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);
// Dark about:* pages.
user_pref("browser.in-content.dark-mode", true);
// 0000: disable about:config warning
user_pref("browser.aboutConfig.showWarning", false);

/*** [SECTION 0100]: STARTUP ***/

// 0101: disable default browser check
user_pref("browser.shell.checkDefaultBrowser", false);
// 0102: set START page (0=blank, 1=home, 2=last visited page, 3=resume previous session)
user_pref("browser.startup.page", 0);
// 0103: set HOME+NEWWINDOW page
user_pref("browser.startup.homepage", "about:blank");
// 0104: set NEWTAB page
user_pref("browser.newtabpage.enabled", false);
user_pref("browser.newtab.preload", false);
// 0105a: disable Activity Stream telemetry
user_pref("browser.newtabpage.activity-stream.feeds.telemetry", false);
user_pref("browser.newtabpage.activity-stream.telemetry", false);
// 0105b: disable Activity Stream Snippets
user_pref("browser.newtabpage.activity-stream.feeds.snippets", false);
user_pref("browser.newtabpage.activity-stream.asrouter.providers.snippets", "");
// 0105c: disable Activity Stream Top Stories, Pocket-based and/or sponsored content
user_pref("browser.newtabpage.activity-stream.feeds.section.topstories", false);
user_pref("browser.newtabpage.activity-stream.section.highlights.includePocket", false);
user_pref("browser.newtabpage.activity-stream.showSponsored", false);
user_pref("browser.newtabpage.activity-stream.feeds.discoverystreamfeed", false);

/*** [SECTION 0300]: QUIET FOX ***/

// 0302a: disable auto-INSTALLING Firefox updates
user_pref("app.update.auto", false);
// 0306: disable extension metadata
user_pref("extensions.getAddons.cache.enabled", false);
// 0310: disable sending the URL of the website where a plugin crashed
user_pref("dom.ipc.plugins.reportCrashURL", false);
// 0320: disable about:addons' Recommendations pane (uses Google Analytics)
user_pref("extensions.getAddons.showPane", false);
// 0321: disable recommendations in about:addons' Extensions and Themes panes
user_pref("extensions.htmlaboutaddons.recommendations.enabled", false);
// 0330: disable telemetry
user_pref("toolkit.telemetry.unified", false);
user_pref("toolkit.telemetry.enabled", false);
user_pref("toolkit.telemetry.server", "data:,");
user_pref("toolkit.telemetry.archive.enabled", false);
user_pref("toolkit.telemetry.newProfilePing.enabled", false);
user_pref("toolkit.telemetry.shutdownPingSender.enabled", false);
user_pref("toolkit.telemetry.updatePing.enabled", false);
user_pref("toolkit.telemetry.bhrPing.enabled", false);
user_pref("toolkit.telemetry.firstShutdownPing.enabled", false);
// 0331: disable Telemetry Coverage
user_pref("toolkit.telemetry.coverage.opt-out", true);
user_pref("toolkit.coverage.opt-out", true);
user_pref("toolkit.coverage.endpoint.base", "");
// 0340: disable Health Reports
user_pref("datareporting.healthreport.uploadEnabled", false);
// 0341: disable new data submission, master kill switch
user_pref("datareporting.policy.dataSubmissionEnabled", false);
// 0342: disable Studies (see 0503)
user_pref("app.shield.optoutstudies.enabled", false);
// 0343: disable personalized Extension Recommendations in about:addons and AMO
user_pref("browser.discovery.enabled", false);
// 0350: disable Crash Reports
user_pref("breakpad.reportURL", "");
user_pref("browser.tabs.crashReporting.sendReport", false);
user_pref("browser.crashReports.unsubmittedCheck.enabled", false);
// 0351: disable backlogged Crash Reports
user_pref("browser.crashReports.unsubmittedCheck.autoSubmit2", false);

/*** [SECTION 0400]: BLOCKLISTS / SAFE BROWSING (SB) ***/

// 0401: enforce Firefox blocklist, but sanitize blocklist url
user_pref("extensions.blocklist.enabled", true);
user_pref("extensions.blocklist.url", "https://blocklists.settings.services.mozilla.com/v1/blocklist/3/%APP_ID%/%APP_VERSION%/");
// 0412: disable SB checks for downloads (remote)
user_pref("browser.safebrowsing.downloads.remote.enabled", false);
user_pref("browser.safebrowsing.downloads.remote.url", "");

/*** [SECTION 0500]: SYSTEM ADD-ONS / EXPERIMENTS ***/

// 0503: disable Normandy/Shield
user_pref("app.normandy.enabled", false);
user_pref("app.normandy.api_url", "");
// 0505: disable System Add-on updates
user_pref("extensions.systemAddon.update.enabled", false);
user_pref("extensions.systemAddon.update.url", "");
// 0506: disable PingCentre telemetry (used in several System Add-ons)
user_pref("browser.ping-centre.telemetry", false);
// 0518: disable Web Compatibility Reporter
user_pref("extensions.webcompat-reporter.enabled", false);

/*** [SECTION 0700]: HTTP* / TCP/IP / DNS / PROXY / SOCKS etc ***/

// 0704: enforce the proxy server to do any DNS lookups when using SOCKS
user_pref("network.proxy.socks_remote_dns", true);

/*** [SECTION 0800]: LOCATION BAR / SEARCH BAR / SUGGESTIONS / HISTORY / FORMS ***/

// 0801: disable location bar using search
user_pref("keyword.enabled", false);
// 0802: disable location bar domain guessing
user_pref("browser.fixup.alternate.enabled", false);
// 0803: display all parts of the url in the location bar
user_pref("browser.urlbar.trimURLs", false);
// 0805: disable coloring of visited links - CSS history leak
user_pref("layout.css.visited_links_enabled", false);
// 0807: disable live search suggestions
user_pref("browser.search.suggest.enabled", false);
user_pref("browser.urlbar.suggest.searches", false);
// 0809: disable location bar suggesting "preloaded" top websites
user_pref("browser.urlbar.usepreloadedtopurls.enabled", false);
// 0850e: disable location bar one-off searches
user_pref("browser.urlbar.oneOffSearches", false);
// 0860: disable search and form history
user_pref("browser.formfill.enable", false);

/*** [SECTION 0900]: PASSWORDS ***/

// 0901: disable saving passwords
user_pref("signon.rememberSignons", false);
// 0912: limit (or disable) HTTP authentication credentials dialogs triggered by sub-resources
user_pref("network.auth.subresource-http-auth-allow", 1);

/*** [SECTION 1200]: HTTPS (SSL/TLS / OCSP / CERTS / HPKP / CIPHERS) ***/

// 1201: require safe negotiation
user_pref("security.ssl.require_safe_negotiation", true);
// 1204: disable SSL session tracking
user_pref("security.ssl.disable_session_identifiers", true);
// 1205: disable SSL Error Reporting
user_pref("security.ssl.errorReporting.automatic", false);
user_pref("security.ssl.errorReporting.enabled", false);
user_pref("security.ssl.errorReporting.url", "");
// 1206: disable TLS1.3 0-RTT (round-trip time)
user_pref("security.tls.enable_0rtt_data", false);
// 1210: enable OCSP Stapling
user_pref("security.ssl.enable_ocsp_stapling", true);
// 1211: control when to use OCSP fetching (to confirm current validity of certificates)
user_pref("security.OCSP.enabled", 1);
// 1212: set OCSP fetch failures (non-stapled, see 1211) to hard-fail
user_pref("security.OCSP.require", true);
// 1220: disable or limit SHA-1 certificates
user_pref("security.pki.sha1_enforcement_level", 1);
// 1223: enforce strict pinning
user_pref("security.cert_pinning.enforcement_level", 2);
// 1270: display warning on the padlock for "broken security" (if 1201 is false)
user_pref("security.ssl.treat_unsafe_negotiation_as_broken", true);
// 1271: control "Add Security Exception" dialog on SSL warnings
user_pref("browser.ssl_override_behavior", 1);
// 1272: display advanced information on Insecure Connection warning pages
user_pref("browser.xul.error_pages.expert_bad_cert", true);
// 1273: display "insecure" icon and "Not Secure" text on HTTP sites
user_pref("security.insecure_connection_icon.enabled", true);
user_pref("security.insecure_connection_text.enabled", true);

/*** [SECTION 1600]: HEADERS / REFERERS ***/

// 1603: CROSS ORIGIN: control when to send a referer
user_pref("network.http.referer.XOriginPolicy", 1);
// 1610: enable the DNT (Do Not Track) HTTP header
user_pref("privacy.donottrackheader.enabled", true);

/*** [SECTION 1700]: CONTAINERS ***/

// 1701: enable Container Tabs setting in preferences (see 1702)
user_pref("privacy.userContext.ui.enabled", true);

/*** [SECTION 1800]: PLUGINS ***/

// 1820: disable GMP (Gecko Media Plugins)
user_pref("media.gmp-provider.enabled", false);
// 1825: disable widevine CDM (Content Decryption Module)
user_pref("media.gmp-widevinecdm.visible", false);
user_pref("media.gmp-widevinecdm.enabled", false);
// 1830: disable all DRM content (EME: Encryption Media Extension)
user_pref("media.eme.enabled", false);

/*** [SECTION 2000]: MEDIA / CAMERA / MIC ***/

// 2002: limit WebRTC IP leaks if using WebRTC
user_pref("media.peerconnection.ice.default_address_only", true);
user_pref("media.peerconnection.ice.no_host", true);
user_pref("media.peerconnection.ice.proxy_only_if_behind_proxy", true);
// 2030: disable autoplay of HTML5 media
user_pref("media.autoplay.default", 5);
// 2031: disable autoplay of HTML5 media if you interacted with the site
user_pref("media.autoplay.enabled.user-gestures-needed", false);

/*** [SECTION 2200]: WINDOW MEDDLING & LEAKS / POPUPS ***/

// 2201: prevent websites from disabling new window features
user_pref("dom.disable_window_open_feature.close", true);
user_pref("dom.disable_window_open_feature.location", true);
user_pref("dom.disable_window_open_feature.menubar", true);
user_pref("dom.disable_window_open_feature.minimizable", true);
user_pref("dom.disable_window_open_feature.personalbar", true);
user_pref("dom.disable_window_open_feature.resizable", true);
user_pref("dom.disable_window_open_feature.status", true);
user_pref("dom.disable_window_open_feature.titlebar", true);
user_pref("dom.disable_window_open_feature.toolbar", true);
// 2202: prevent scripts from moving and resizing open windows
user_pref("dom.disable_window_move_resize", true);
// 2203: open links targeting new windows in a new tab instead
user_pref("browser.link.open_newwindow", 3);
user_pref("browser.link.open_newwindow.restriction", 0);
// 2210: block popup windows
user_pref("dom.disable_open_during_load", true);
// 2212: limit events that can cause a popup
user_pref("dom.popup_allowed_events", "click dblclick");

/*** [SECTION 2300]: WEB WORKERS ***/

// 2302: disable service workers
user_pref("dom.serviceWorkers.enabled", false);

/*** [SECTION 2400]: DOM (DOCUMENT OBJECT MODEL) & JAVASCRIPT ***/

// 2402: disable website access to clipboard events/content
user_pref("dom.event.clipboardevents.enabled", false);
// 2405: disable "Confirm you want to leave" dialog on page close
user_pref("dom.disable_beforeunload", true);
// 2414: disable shaking the screen
user_pref("dom.vibrator.enabled", false);
// 2429: enable (limited but sufficient) window.opener protection
user_pref("dom.targetBlankNoOpener.enabled", true)

/*** [SECTION 2600]: MISCELLANEOUS ***/

// 2601: prevent accessibility services from accessing your browser
user_pref("accessibility.force_disabled", 1);
// 2602: disable sending additional analytics to web servers
user_pref("beacon.enabled", false);
// 2603: remove temp files opened with an external application
user_pref("browser.helperApps.deleteTempFileOnExit", true);
// 2605: block web content in file processes
user_pref("browser.tabs.remote.allowLinkedWebInFileUriProcess", false);
// 2606: disable UITour backend so there is no chance that a remote page can use it
user_pref("browser.uitour.enabled", false);
user_pref("browser.uitour.url", "");
// 2611: disable middle mouse click opening links from clipboard
user_pref("middlemouse.contentLoadURL", false);
// 2614: limit HTTP redirects (this does not control redirects with HTML meta tags or JS)
user_pref("network.http.redirection-limit", 10);
// 2616: remove special permissions for certain mozilla domains
user_pref("permissions.manager.defaultsUrl", "");
// 2617: remove webchannel whitelist
user_pref("webchannel.allowObject.urlWhitelist", "");

/** DOWNLOADS ***/

// 2650: discourage downloading to desktop
user_pref("browser.download.folderList", 1);
// 2651: enforce user interaction for security by always asking where to download
user_pref("browser.download.useDownloadDir", false);
// 2652: disable adding downloads to the system's "recent documents" list
user_pref("browser.download.manager.addToRecentDocs", false);
// 2653: disable hiding mime types (Options>General>Applications) not associated with a plugin
user_pref("browser.download.hide_plugins_without_extensions", false);
// 2654: disable "open with" in download dialog
user_pref("browser.download.forbid_open_with", true);

/** SECURITY ***/

// 2680: enforce CSP (Content Security Policy)
user_pref("security.csp.enable", true);

/*** [SECTION 2700]: PERSISTENT STORAGE ***/

// 2701: disable 3rd-party cookies and site-data
user_pref("network.cookie.cookieBehavior", 1);
user_pref("browser.contentblocking.category", "custom");
// 2702: set third-party cookies (i.e ALL) (if enabled, see 2701) to session-only
user_pref("network.cookie.thirdparty.sessionOnly", true);
user_pref("network.cookie.thirdparty.nonsecureSessionOnly", true);

/*** [SECTION 4000]: FPI (FIRST PARTY ISOLATION) ***/

// 4001: enable First Party Isolation
user_pref("privacy.firstparty.isolate", true);

/*** [SECTION 4500]: RFP (RESIST FINGERPRINTING) ***/

// 4501: enable privacy.resistFingerprinting
user_pref("privacy.resistFingerprinting", true);
// 4503: disable mozAddonManager Web API
user_pref("privacy.resistFingerprinting.block_mozAddonManager", true);
// 4510: disable showing about:blank as soon as possible during startup
user_pref("browser.startup.blankWindow", false);
