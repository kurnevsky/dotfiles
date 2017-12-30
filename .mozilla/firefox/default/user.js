// Show a blank page when Firefox starts
user_pref("browser.startup.homepage", "about:blank");
user_pref("browser.startup.page", 0);
user_pref("browser.rights.3.shown", true);
user_pref("browser.startup.homepage_override.mstone", "ignore");
user_pref("startup.homepage_welcome_url", "");
user_pref("startup.homepage_welcome_url.additional", "");
user_pref("startup.homepage_override_url", "");
user_pref("browser.laterrun.enabled", false);

// Show a blank page when opening new tab
user_pref("browser.newtabpage.enabled", false);
user_pref("browser.newtabpage.enhanced", false);
user_pref("browser.newtab.preload", false);
user_pref("browser.newtabpage.introShown", true);
user_pref("browser.newtabpage.directory.source", "");
user_pref("browser.newtabpage.activity-stream.enabled", false);

// Disable Pocket
user_pref("extensions.pocket.enabled", false);

// Always ask me where to save download files
user_pref("browser.download.useDownloadDir", false);

// Request that sites not track me
user_pref("privacy.donottrackheader.enabled", true);

// Enable OpenGL Off-Main-Thread Compositing (OMTC)
user_pref("layers.acceleration.force-enabled", true);

// Set dark theme
user_pref("lightweightThemes.selectedThemeID", "firefox-compact-dark@mozilla.org");
