# mm-el

A Simple (unfinished, unpolished) Mattermost Client for Emacs

## Features:

- See all new posts in your **\*Messages\*** buffer
- Use `mm-reply` to interactively select a post and send a reply
- Sometimes the websocket drops and it stops receiving posts

## Setup:

Drop `mm.el` somewhere on your filesystem and add the following to your `.emacs.d/init`:

    (load "PATH_TO_MM.EL")
    (setq mm-url "mattermost_url.server.com")
    (setq mm-pat "your_very_secret_mattermost_personal_access_token")

Use `mm-connect` to connect to mattermost and `mm-disconnect` to end your session.
