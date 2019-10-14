module Localization exposing (Language(..), animationSpeedTitle, drawnGame, easyDifficulty, englishLanguage, fastSpeed, finalScore, firstTurnDescription, firstTurnTitle, gameModeHint, gameModeSettings, gameOver, germanLanguage, highDifficulty, languageSetting, lastSeedsDescription, lastSeedsTitle, mediumDifficulty, moreInfo, normalSpeed, numberOfSeeds, opponentDescription, opponentLevelTitle, opponentsStoreDescription, opponentsStoreTitle, opponentsTurn, player, presentationSettings, restart, settings, slowSpeed, tabletModeDescription, tabletModeTitle, youLoose, youWin, yourTurn)


type Language
    = German
    | English


settings : Language -> String
settings language =
    case language of
        German ->
            "Einstellungen"

        English ->
            "Settings"


moreInfo : Language -> String
moreInfo language =
    case language of
        German ->
            "Mehr Infos"

        English ->
            "About"


player : Language -> String
player language =
    case language of
        German ->
            "Spieler"

        English ->
            "Player"


yourTurn : Language -> String
yourTurn language =
    case language of
        German ->
            "Du bist am Zug."

        English ->
            "It's your turn."


opponentsTurn : Language -> String -> String
opponentsTurn language opponent =
    case language of
        German ->
            opponent ++ " ist am Zug."

        English ->
            "It's " ++ opponent ++ "'s turn."


gameOver : Language -> String
gameOver language =
    case language of
        German ->
            "Spiel beendet."

        English ->
            "Game over."


drawnGame : Language -> String
drawnGame language =
    case language of
        German ->
            "Es ist unentschieden."

        English ->
            "It's a draw."


youWin : Language -> String
youWin language =
    case language of
        German ->
            "Du hast gewonnen."

        English ->
            "You have won."


youLoose : Language -> String
youLoose language =
    case language of
        German ->
            "Leider verloren."

        English ->
            "You have lost."


finalScore : Language -> String
finalScore language =
    case language of
        German ->
            "Endstand"

        English ->
            "Final Score"


restart : Language -> String
restart language =
    case language of
        German ->
            "Neues Spiel"

        English ->
            "Restart"


presentationSettings : Language -> String
presentationSettings language =
    case language of
        German ->
            "Einstellungen zur Darstellung"

        English ->
            "Presentation Settings"


tabletModeTitle : Language -> String
tabletModeTitle language =
    case language of
        German ->
            "Tablet-Modus"

        English ->
            "Tablet-Mode"


tabletModeDescription : Language -> String
tabletModeDescription language =
    case language of
        German ->
            "Die Spielelemente für Spieler 2 werden kopfüber dargestellt."

        English ->
            "The elements for Player 2 are displayed upside down."


animationSpeedTitle : Language -> String
animationSpeedTitle language =
    case language of
        German ->
            "Geschwindigkeit der Animation"

        English ->
            "Animation Speed"


slowSpeed : Language -> String
slowSpeed language =
    case language of
        German ->
            "Langsam"

        English ->
            "Slow"


normalSpeed : Language -> String
normalSpeed language =
    case language of
        German ->
            "Normal"

        English ->
            "Normal"


fastSpeed : Language -> String
fastSpeed language =
    case language of
        German ->
            "Schnell"

        English ->
            "Fast"


languageSetting : Language -> String
languageSetting language =
    case language of
        German ->
            "Sprache"

        English ->
            "Language"


germanLanguage : Language -> String
germanLanguage language =
    case language of
        German ->
            "deutsch"

        English ->
            "german"


englishLanguage : Language -> String
englishLanguage language =
    case language of
        German ->
            "englisch"

        English ->
            "english"


gameModeSettings : Language -> String
gameModeSettings language =
    case language of
        German ->
            "Einstellungen des Spielmodus"

        English ->
            "Settings of Game Mode"


gameModeHint : Language -> String
gameModeHint language =
    case language of
        German ->
            "Hinweis: Das Spiel wird bei Änderung neu gestartet."

        English ->
            "Hint: Game will be restarted when settings change."


numberOfSeeds : Language -> String
numberOfSeeds language =
    case language of
        German ->
            "Anzahl der Steine pro Mulde"

        English ->
            "Number of seeds per house"


lastSeedsTitle : Language -> String
lastSeedsTitle language =
    case language of
        German ->
            "Verteilung übriger Steine"

        English ->
            "Last seeds"


lastSeedsDescription : Language -> String
lastSeedsDescription language =
    case language of
        German ->
            "Am Ende des Spieles erhält der Spieler, der keine Steine mehr in seiner Reihe hat, die übrig gebliebenen Steine."

        English ->
            "At the end of the game, the first player running out of seeds gets the remaining ones."


opponentsStoreTitle : Language -> String
opponentsStoreTitle language =
    case language of
        German ->
            "Gegnerisches Kalaha"

        English ->
            "Opponent's Store"


opponentsStoreDescription : Language -> String
opponentsStoreDescription language =
    case language of
        German ->
            "Steine werden auch in das gegnerische Kalaha (große Mulde) verteilt."

        English ->
            "Seeds are also sown to opponent's store."


firstTurnTitle : Language -> String
firstTurnTitle language =
    case language of
        German ->
            "Erster Zug"

        English ->
            "First Turn"


firstTurnDescription : Language -> String
firstTurnDescription language =
    case language of
        German ->
            "Spieler 2 beginnt mit dem ersten Zug"

        English ->
            "Player 2 makes the first turn."


opponentLevelTitle : Language -> String
opponentLevelTitle language =
    case language of
        German ->
            "Gegenspieler & Schwierigkeitsstufe"

        English ->
            "Opponent & Difficulty"


opponentDescription : Language -> String
opponentDescription language =
    case language of
        German ->
            "Gegen den Computer spielen"

        English ->
            "Play against the computer"


easyDifficulty : Language -> String
easyDifficulty language =
    case language of
        German ->
            "Leicht"

        English ->
            "Easy"


mediumDifficulty : Language -> String
mediumDifficulty language =
    case language of
        German ->
            "Mittel"

        English ->
            "Medium"


highDifficulty : Language -> String
highDifficulty language =
    case language of
        German ->
            "Schwer"

        English ->
            "Difficult"
