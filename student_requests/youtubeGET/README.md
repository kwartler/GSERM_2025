# Youtube Closed Caption Functions

## Method to obtain Youtube Close Captions from individual videos

This [repository]('https://github.com/kwartler/yt-timed-text') is used to create the application for obtaining the video.  It is a single page application using the browser to store the files.

## How to use

Nagivate to the [releases page](https://github.com/kwartler/yt-timed-text/releases) and download the appropriate one for your software.  *These binaries are not code-signed, so your OS will warn you the first time.*

- **Version 2.4 is the up to date and useful release**

### MAC

- Mac: "cannot be opened because it is from an unidentified developer."
- Right-click yt-timed-text.app → Open → click Open in the dialog.

### WINDOWS

- Windows: "Windows protected your PC" (SmartScreen).
- Click More info → Run anyway.

### More info is available on the original repo readme.

## Small issue
Because the app is small when you close the tab the backend shuts down extremely fast, making MacOS thinks it is a crash.  This would then block it from opening again immediatley.  So either shut down using the "**quit**" button or worse case just kill the background activity.