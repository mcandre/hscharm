{-# LANGUAGE ForeignFunctionInterface #-}

-- | HsCharm wraps charm, a minimal ncurses-like terminal UI library
module HsCharm (
    charmVersion,

    getWidth,
    getHeight,

    cursorOff,
    cursorOn,

    echoOff,
    echoOn,

    rawOn,
    rawOff,

    getCursor,
    moveCursor,
    blotChar,
    blotString,
    hCenterString,
    vCenterString,

    clearScreen,

    handleSignal,

    startCharm,
    endCharm,

    getKey,
    Key(
        KeyBackspace,
        KeyTab,
        KeyNewline,

        KeySpace,
        KeyExclamation,
        KeyDoubleQuote,
        KeyHash,
        KeyDollar,
        KeyPercent,
        KeyAmpersand,
        KeySingleQuote,
        KeyLeftParen,
        KeyRightParen,
        KeyAsterisk,
        KeyPlus,
        KeyComma,
        KeyMinus,
        KeyPeriod,
        KeySlash,

        KeyZero,
        KeyOne,
        KeyTwo,
        KeyThree,
        KeyFour,
        KeyFive,
        KeySix,
        KeySeven,
        KeyEight,
        KeyNine,

        KeyColon,
        KeySemicolon,
        KeyLessThan,
        KeyEquals,
        KeyGreaterThan,
        KeyQuestion,
        KeyAt,

        KeyCapitalA,
        KeyCapitalB,
        KeyCapitalC,
        KeyCapitalD,
        KeyCapitalE,
        KeyCapitalF,
        KeyCapitalG,
        KeyCapitalH,
        KeyCapitalI,
        KeyCapitalJ,
        KeyCapitalK,
        KeyCapitalL,
        KeyCapitalM,
        KeyCapitalN,
        KeyCapitalO,
        KeyCapitalP,
        KeyCapitalQ,
        KeyCapitalR,
        KeyCapitalS,
        KeyCapitalT,
        KeyCapitalU,
        KeyCapitalV,
        KeyCapitalW,
        KeyCapitalX,
        KeyCapitalY,
        KeyCapitalZ,

        KeyLeftBracket,
        KeyBackslash,
        KeyRightBracket,
        KeyCaret,
        KeyUnderscore,
        KeyBacktick,

        KeyA,
        KeyB,
        KeyC,
        KeyD,
        KeyE,
        KeyF,
        KeyG,
        KeyH,
        KeyI,
        KeyJ,
        KeyK,
        KeyL,
        KeyM,
        KeyN,
        KeyO,
        KeyP,
        KeyQ,
        KeyR,
        KeyS,
        KeyT,
        KeyU,
        KeyV,
        KeyW,
        KeyX,
        KeyY,
        KeyZ,

        KeyLeftBrace,
        KeyPipe,
        KeyRightBrace,
        KeyTilde,

        KeyUp,
        KeyDown,
        KeyRight,
        KeyLeft,

        KeyEscape,
        KeyUnknown
        )
    ) where

import Foreign.C

-- | charmVersion is semver.
charmVersion :: String
charmVersion = "0.0.1"

-- | getWidth queries terminal width.
foreign import ccall "charm.h get_width" getWidth :: IO Int

-- | getHeight queries terminal height
foreign import ccall "charm.h get_height" getHeight :: IO Int

-- | cursorOff hides the cursor.
foreign import ccall "charm.h cursor_off" cursorOff :: IO ()

-- | cursorOn shows the cursor.
foreign import ccall "charm.h cursor_on" cursorOn :: IO ()

-- | echoOff disables key echoing.
foreign import ccall "charm.h echo_off" echoOff :: IO ()

-- | echoOn enables key echoing.
foreign import ccall "charm.h echo_on" echoOn :: IO ()

-- | rawOn enables raw manipulation.
foreign import ccall "charm.h raw_on" rawOn :: IO ()

-- | rawOff disables raw manipulation.
foreign import ccall "charm.h raw_off" rawOff :: IO ()

-- | getX gets the cursor X coordinate.
foreign import ccall "charm.h get_x" getX :: IO Int

-- | getY gets the cursor Y coordinate.
foreign import ccall "charm.h get_y" getY :: IO Int

-- | getCursor queries the cursor position.
getCursor :: IO (Int, Int)
getCursor = do
    x <- getX
    y <- getY

    return (x, y)

-- | moveCursor repositions the cursor.
foreign import ccall "charm.h move_cursor" moveCursor :: Int -> Int -> IO ()

-- | blotChar renders a chacter.
foreign import ccall "charm.h blot_char" blotChar :: Char -> IO ()

-- | blogString' renders a C string.
foreign import ccall "charm.h blot_string" blotString' :: CString -> IO ()

-- | blotString renders a string message.
blotString :: String -> IO ()
blotString s = do
    s' <- newCString s
    blotString' s'

-- | hCenterString' centers C strings horizontally.
foreign import ccall "charm.h hcenter_string" hCenterString' :: CString -> IO ()

-- | hCenterString displays a string centered horizontally on screen.
hCenterString :: String -> IO ()
hCenterString s = do
    s' <- newCString s
    hCenterString' s'

-- | vCenterString' centers C strings vertically.
foreign import ccall "charm.h vcenter_string" vCenterString' :: CString -> IO ()

-- | vCenterString displays a string centered vertically on screen.
vCenterString :: String -> IO ()
vCenterString s = do
    s' <- newCString s
    vCenterString' s'

-- | clearScreen wipes the terminal display.
foreign import ccall "charm.h clear_screen" clearScreen :: IO ()

-- | handleSignal dispatches events.
foreign import ccall "charm.h handle_signal" handleSignal :: Int -> IO ()

-- | startCharm prepares the charm session.
foreign import ccall "charm.h start_charm" startCharm :: IO ()

-- | endCharm tears down charm session resources.
foreign import ccall "charm.h end_charm" endCharm :: IO ()

-- | getKey' queries keyboard input.
foreign import ccall "charm.h get_key" getKey' :: IO Int

-- | Key models keybard input.
data Key
    = KeyBackspace
    | KeyTab
    | KeyNewline

    | KeySpace
    | KeyExclamation
    | KeyDoubleQuote
    | KeyHash
    | KeyDollar
    | KeyPercent
    | KeyAmpersand
    | KeySingleQuote
    | KeyLeftParen
    | KeyRightParen
    | KeyAsterisk
    | KeyPlus
    | KeyComma
    | KeyMinus
    | KeyPeriod
    | KeySlash

    | KeyZero
    | KeyOne
    | KeyTwo
    | KeyThree
    | KeyFour
    | KeyFive
    | KeySix
    | KeySeven
    | KeyEight
    | KeyNine

    | KeyColon
    | KeySemicolon
    | KeyLessThan
    | KeyEquals
    | KeyGreaterThan
    | KeyQuestion
    | KeyAt

    | KeyCapitalA
    | KeyCapitalB
    | KeyCapitalC
    | KeyCapitalD
    | KeyCapitalE
    | KeyCapitalF
    | KeyCapitalG
    | KeyCapitalH
    | KeyCapitalI
    | KeyCapitalJ
    | KeyCapitalK
    | KeyCapitalL
    | KeyCapitalM
    | KeyCapitalN
    | KeyCapitalO
    | KeyCapitalP
    | KeyCapitalQ
    | KeyCapitalR
    | KeyCapitalS
    | KeyCapitalT
    | KeyCapitalU
    | KeyCapitalV
    | KeyCapitalW
    | KeyCapitalX
    | KeyCapitalY
    | KeyCapitalZ

    | KeyLeftBracket
    | KeyBackslash
    | KeyRightBracket
    | KeyCaret
    | KeyUnderscore
    | KeyBacktick

    | KeyA
    | KeyB
    | KeyC
    | KeyD
    | KeyE
    | KeyF
    | KeyG
    | KeyH
    | KeyI
    | KeyJ
    | KeyK
    | KeyL
    | KeyM
    | KeyN
    | KeyO
    | KeyP
    | KeyQ
    | KeyR
    | KeyS
    | KeyT
    | KeyU
    | KeyV
    | KeyW
    | KeyX
    | KeyY
    | KeyZ

    | KeyLeftBrace
    | KeyPipe
    | KeyRightBrace
    | KeyTilde
    | KeyUp
    | KeyDown
    | KeyRight
    | KeyLeft

    | KeyEscape
    | KeyUnknown

    deriving (Eq, Ord, Enum, Show)

-- | getKey queries key presses.
getKey :: IO Key
getKey = do
    k <- getKey'
    return (toEnum k :: Key)
