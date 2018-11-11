import Development.Shake
import Development.Shake.FilePath
import System.Directory as Dir

main :: IO ()
main = do
    let tarball = "dist/hscharm-0.0.2.tar.gz"
    homeDir <- Dir.getHomeDirectory

    shakeArgs shakeOptions{ shakeFiles="dist" } $ do
        want ["dist/bin/hellocharm" <.> exe]

        "dist/bin/hellocharm" <.> exe %> \out ->
            cmd_ "cabal" "install" "--bindir" "dist/bin"

        phony "hlint" $
            cmd_ "hlint" "."

        phony "lint" $
            need ["hlint"]

        phony "install" $
            cmd_ "cabal" "install"

        phony "uninstall" $ do
            cmd_ "ghc-pkg" "unregister" "--force" "hscharm"
            removeFilesAfter homeDir ["/.cabal/bin/hellocharm" <.> exe]
            removeFilesAfter homeDir ["/.cabal/bin/rl" <.> exe]
            removeFilesAfter homeDir ["/.cabal/bin/ddr" <.> exe]

        phony "build" $
            cmd_ "cabal" "build"

        phony "haddock" $
            cmd_ "cabal" "haddock"

        tarball %> \_ -> do
            need ["build", "haddock"]
            cmd_ "cabal" "sdist"

        phony "sdist" $
            need [tarball]

        phony "publish" $ do
            need ["sdist"]
            cmd_ "cabal" "upload" tarball

        phony "clean" $
            cmd_ "cabal" "clean"
