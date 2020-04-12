#lang typed/racket

(require digimon/plist)

(define plist : PList-Datum
  ((inst make-hasheq Symbol PList-Datum)
   '((CFBundleDevelopmentRegion . "$(DEVELOPMENT_LANGUAGE)")
     (CFBundleExecutable . "$(EXECUTABLE_NAME)")
     (CFBundleIdentifier . "$(PRODUCT_BUNDLE_IDENTIFIER)")
     (CFBundleInfoDictionaryVersion . "6.0")
     (CFBundleName . "$(PRODUCT_NAME)")
     (CFBundlePackageType . "$(PRODUCT_BUNDLE_PACKAGE_TYPE)")
     (CFBundleShortVersionString . "1.0")
     (CFBundleVersion . "1")
     (LSRequiresIPhoneOS . 1)
     (UIRequiredDeviceCapabilities . #["armv7"])
     (UIStatusBarHidden . 1)
     (UISupportedInterfaceOrientations . #["UIInterfaceOrientationPortrait" "UIInterfaceOrientationLandscapeLeft" "UIInterfaceOrientationLandscapeRight"])
     (UISupportedInterfaceOrientations~ipad . #["UIInterfaceOrientationPortrait"
                                                "UIInterfaceOrientationPortraitUpsideDown"
                                                "UIInterfaceOrientationLandscapeLeft"
                                                "UIInterfaceOrientationLandscapeRight"]))))
