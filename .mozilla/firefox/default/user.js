// Show a blank page when Firefox starts
user_pref("browser.startup.page", 0);
user_pref("browser.startup.homepage", "about:blank");
user_pref("browser.startup.homepage_override.mstone", "ignore");
user_pref("startup.homepage_welcome_url", "");
user_pref("startup.homepage_welcome_url.additional", "");
user_pref("startup.homepage_override_url", "");

// Show a blank page when opening new tab
user_pref("browser.newtabpage.enabled", false);
user_pref("browser.newtab.preload", false);

// Disable Pocket
user_pref("extensions.pocket.enabled", false);

// Always ask me where to save download files
user_pref("browser.download.useDownloadDir", false);

// Request that sites not track me
user_pref("privacy.donottrackheader.enabled", true);

// Enable Tracking Protection.
user_pref("privacy.trackingprotection.enabled", true);
user_pref("privacy.trackingprotection.pbmode.enabled", true);
user_pref("privacy.trackingprotection.introCount", 20);

// Enable Content Blocking.
user_pref("browser.contentblocking.enabled", true);
user_pref("browser.contentblocking.introCount", 20);

// Enable First Party Isolation.
user_pref("privacy.firstparty.isolate", true);
user_pref("privacy.firstparty.isolate.restrict_opener_access", true);

// Disable Health Report.
user_pref("datareporting.healthreport.uploadEnabled", false);
user_pref("datareporting.policy.dataSubmissionEnabled", false);

// Disable Crash Reporter.
user_pref("breakpad.reportURL", "");
user_pref("browser.tabs.crashReporting.sendReport", false);
user_pref("browser.crashReports.unsubmittedCheck.enabled", false);
user_pref("browser.crashReports.unsubmittedCheck.autoSubmit", false);
user_pref("browser.crashReports.unsubmittedCheck.autoSubmit2", false);

// Disable Browser Error Reporter.
user_pref("browser.chrome.errorReporter.enabled", false);
user_pref("browser.chrome.errorReporter.submitUrl", "");

// Disable Telemetry.
user_pref("toolkit.telemetry.enabled", false);
user_pref("toolkit.telemetry.unified", false);
user_pref("toolkit.telemetry.archive.enabled", false);
user_pref("toolkit.telemetry.cachedClientID", "");
user_pref("toolkit.telemetry.server", "data:,");
user_pref("toolkit.telemetry.newProfilePing.enabled", false);
user_pref("toolkit.telemetry.shutdownPingSender.enabled", false);
user_pref("toolkit.telemetry.updatePing.enabled", false);
user_pref("toolkit.telemetry.bhrPing.enabled", false);
user_pref("toolkit.telemetry.firstShutdownPing.enabled", false);
user_pref("toolkit.telemetry.hybridContent.enabled", false);

// Enable OpenGL Off-Main-Thread Compositing (OMTC)
user_pref("layers.acceleration.force-enabled", true);

// Set dark theme
user_pref("lightweightThemes.selectedThemeID", "firefox-compact-dark@mozilla.org");

// Disable saving passwords
user_pref("signon.rememberSignons", false);
