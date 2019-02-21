# Wallpaper Changer Project

Wallpaper Changer is a program to change desktop wallpapers. 

### Motivation

There are many other programs to change desktop wallpaper. But none of them allow to set up own scenario. This differs this projects from similar software.

### Features

 - Has **simple** (like many other similar programs) and **scripted** (which allows to provide user created scenarios) modes.
 - In simple mode supports:
   - recursive search for images in directory tree
   - static or random delay between wallpapers changing
   - static  or random frequency of appearance for images
 - In scripted mode it may:
   - create list of wallpaper images to be set with specified delay for each
   - apply individual style for each wallpaper
   - set wallpapers with probability
   - set wallpapers at specified date / time
   - choose wallpaper from given list with different probability
   - choose wallpaper by date, time, season, month, weekday
   - repeat commands sequences
   - and others
 - Easy to use
 - Lightweight
 - Cross-platform

### Supported platforms

For now Windows and Linux are supported.
Also it is possible to provide custom wallpaper setter for unsupported by default environment.

##### Supported Windows versions

Works in Windows XP and higher, but Windows 7 or higher recommended.

Also works in [ReactOS](https://github.com/reactos/reactos).

##### Supported Linux desktop environments

 - Gnome 3
 - Gnome Classic
 - MATE
 - Cinnamon
 - KDE Plasma
 - XFCE
 - LXDE
 - Unity
 - Pantheon
 - Budgie

### How to get Wallpaper Changer

Binaries could be downloaded on [releases page](https://github.com/MykolaMorhun/WallpaperChanger/releases).
Also one may build binaries from sources.

### How to build from sources

Just clone source code and compile with Lazarus.
Lazarus 1.8.2 and fpc 3.0.4 or higher versions recommended.

### Contribution

Open a pull request in [official repository](https://github.com/MykolaMorhun/WallpaperChanger).

### Documentation

For more details about Wallpaper Changer see [documentation](https://github.com/MykolaMorhun/WallpaperChanger/blob/master/resources/docs/en/wallpaper-changer.adoc).

### License

Wallpaper Changer is licensed under the terms of GNU GENERAL PUBLIC LICENSE version 2
