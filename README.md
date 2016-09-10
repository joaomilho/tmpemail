# tmpemail

Creates a new email and polls for received emails using guerrilla mail.

#### Install

You need [stack](https://docs.haskellstack.org/en/stable/README/) to build it:

```
stack install
```

#### Usage

Without proxy:

```
tmpemail
```

With proxy (recommended):

```
tmpemail 127.0.0.1 8118
```

Using a proxy with not log your real IP on guerilla mail's logs, making you further untraceable.
