#lang typed/racket

;;; https://developer.apple.com/library/archive/documentation/CoreFoundation/Conceptual/CFBundles/BundleTypes/BundleTypes.html#//apple_ref/doc/uid/10000123i-CH101-SW5

(provide (all-defined-out))

(define apple-info.plist-database : (Immutable-HashTable Symbol Symbol)
  (make-immutable-hash
   (list
    ;;; Required Keys
    ; The bundle display name is the name displayed underneath the application icon.
    ; This value should be localized for all supported languages.
    (cons 'collection 'CFBundleDisplayName)

    ; The bundle identifier string identifies your application to the system.
    ; This string must be a uniform type identifier (UTI) that contains only alphanumeric (A-Z,a-z,0-9), hyphen (-), and period (.) characters.
    ; The string should also be in reverse-DNS format. For example,
    ; The bundle identifier is used in validating the application signature.
    (cons 'id 'CFBundleIdentifier)

    ; The bundle version string specifies the build version number of the bundle.
    ; This value is a monotonically increased string, comprised of one or more period-separated integers.
    ; This value cannot be localized.
    (cons 'version 'CFBundleVersion)

    ; An array of strings containing the filenames of the images used for the application’s assorted icons.
    ; Although technically not required, it is strongly encouraged that you use it.
    (cons 'icons 'CFBundleIconFiles)

    ; A Boolean value that indicates whether the bundle can run on iOS only.
    (cons 'iOS-only? 'LSRequiresIPhoneOS)

    ; https://developer.apple.com/library/archive/documentation/General/Reference/InfoPlistKeyReference/Introduction/Introduction.html#//apple_ref/doc/uid/TP40009247
    ; A key that tells iTunes and the App Store know which device-related features an application requires in order to run.
    ; iTunes and the mobile App Store use this list to prevent customers from installing applications on a device that does not support the listed capabilities.
    ; The value of this key is either an array or a dictionary.
    ;   If you use an array, the presence of a given key indicates the corresponding feature is required.
    ;   If you use a dictionary, you must specify a Boolean value for each key indicating whether the feature is required.
    ; In both cases, not including a key indicates that the feature is not required.
    (cons 'capabilities 'UIRequiredDeviceCapabilities)

    ;;; Recommand Keys
    ; A string that identifies the name of the application’s main nib file.
    ; If you want to use a nib file other than the default one created for your project, associate the name of that nib file with this key.
    ; The name of the nib file should not include the .nib filename extension.
    (cons 'nib 'NSMainNibFile)

    ; A string that identifies the style of the status bar as the application launches.
    ; This value is based on the UIStatusBarStyle constants declared in UIApplication.h header file.
    ; The default style is UIStatusBarStyleDefault.
    ; The application can change this initial status-bar style when it finishes launching.
    (cons 'statusbar 'UIStatusBarStyle)

    ; A Boolean value that determines whether the status bar is initially hidden when the application launches.
    ; Set it to true to hide the status bar.
    (cons 'statusbar? 'UIStatusBarHidden)

    ; A string that identifies the initial orientation of the application’s user interface.
    ; This value is based on the UIInterfaceOrientation constants declared in the UIApplication.h header file.
    ; The default style is UIInterfaceOrientationPortrait.
    (cons 'orientation 'UIInterfaceOrientation)

    ; A Boolean value that indicates whether the application icon already includes gloss and bevel effects.
    ; The default value is false.
    ; Set it to true if you do not want the system to add these effects to your artwork.
    (cons 'artwork? 'UIPrerenderedIcon)

    ; A Boolean value that notifies the system that the application uses the Wi-Fi network for communication.
    ; Applications that use Wi-Fi for any period of time must set this key to true;
    ;   otherwise, after 30 minutes, the device shuts down Wi-Fi connections to save power.
    ; Setting this flag also lets the system know that it should display the network selection dialog when Wi-Fi is available but not currently being used.
    ; The default value is false.
    ; Even if the value of this property is true, this key has no effect when the device is idle (that is, screen-locked).
    ;   During that time, the application is considered inactive and, although it may function on some levels, it has no Wi-Fi connection.
    (cons 'WiFi? 'UIRequiresPersistentWiFi)

    ; A String containing the base filename used by the application’s launch images.
    ; If you do not specify this key, the base name is assumed to be the string Default.
    (cons 'launcher 'UILaunchImageFile))))

(define apple-info.plist : (-> Symbol Any (Values Symbol Any))
  (lambda [var val]
    (values (hash-ref apple-info.plist-database var (λ [] var)) val)))
