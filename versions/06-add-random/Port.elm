port module Port exposing (onblur)

-- ██████   ██████  ██████  ████████
-- ██   ██ ██    ██ ██   ██    ██
-- ██████  ██    ██ ██████     ██
-- ██      ██    ██ ██   ██    ██
-- ██       ██████  ██   ██    ██


port onblur : (() -> msg) -> Sub msg
