# org-timeline

Add graphical view of agenda to agenda buffer.

# Installation

After you install this package from MELPA Stable, add the following line to your org configuration:

``` emacs-lisp
(add-hook 'org-agenda-finalize-hook 'org-timeline-insert-timeline :append)
```

# How it works

This package adds a graphical view of the agenda after the last agenda line.  By default the display starts at 5 AM today and goes up to 4 AM next day (this covers 24 hours).

Scheduled tasks or tasks with time ranges are rendered in the display with `RoyalBlue` color.  Clocked entires are displayed in `Grey`.  The background of timeslots which are in the past is highlighted with `#555555` color.

You can use custom color for a task by adding the property `TIMELINE_FACE` with either a string which is a color name or a list which specifies the face properties or a symbol which is taken to be a face name.

# TODO

- [ ] Add faces instead of colors
- [ ] Make "midnight"/change-of-day configurable (currently 5 AM)
