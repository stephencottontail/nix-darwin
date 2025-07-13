final: prev: {
  plan9port = prev.plan9port.overrideAttrs (previousAttrs: {
    postInstall =
      (previousAttrs.postInstall or "")
      + ''
        			mkdir -p $out/Applications/{Acme,Sam}.app/Contents/{MacOS,Resources}
        			echo -e "#!/usr/bin/env sh\n" \
        				"\n" \
        				"9 acme \"\$*\"" > $out/Applications/Acme.app/Contents/MacOS/acme
        			echo -e "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" \
        				"<!DOCTYPE plist PUBLIC \"-//Apple Computer//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\n" \
        				"<plist version=\"1.0\">\n" \
        				"<dict>\n" \
        				"\t<key>CFBundleDevelopmentRegion</key>\n" \
        				"\t<string>English</string>\n" \
        				"\t<key>CFBundleExecutable</key>\n" \
        				"\t<string>acme</string>\n" \
        				"\t<key>CFBundleGetInfoString</key>\n" \
        				"\t<string>0.1-1</string>\n" \
        				"\t<key>CFBundleIconName</key>\n" \
        				"\t<string>spaceglenda</string>\n" \
        				"\t<key>CFBundleIconFile</key>\n" \
        				"\t<string>spaceglenda.icns</string>\n" \
        				"\t<key>CFBundleIdentifier</key>\n" \
        				"\t<string>com.swtch.acme</string>\n" \
        				"\t<key>CFBundleInfoDictionaryVersion</key>\n" \
        				"\t<string>6.0</string>\n" \
        				"\t<key>CFBundleName</key>\n" \
        				"\t<string>Acme</string>\n" \
        				"\t<key>CFBundlePackageType</key>\n" \
        				"\t<string>APPL</string>\n" \
        				"\t<key>CFBundleShortVersionString</key>\n" \
        				"\t<string>0.1</string>\n" \
        				"\t<key>CFBundleSignature</key>\n" \
        				"\t<string>Acme1</string>\n" \
        				"\t<key>CFBundleVersion</key>\n" \
        				"\t<string>1</string>\n" \
        				"</dict>\n" \
        				"</plist>" > $out/Applications/Acme.app/Contents/Info.plist

        			echo -e "#!/usr/bin/env sh\n" \
        				"\n" \
        				"9 sam \"\$*\"" > $out/Applications/Sam.app/Contents/MacOS/sam
        			echo -e "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" \
        				"<!DOCTYPE plist PUBLIC \"-//Apple Computer//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\n" \
        				"<plist version=\"1.0\">\n" \
        				"<dict>\n" \
        				"\t<key>CFBundleDevelopmentRegion</key>\n" \
        				"\t<string>English</string>\n" \
        				"\t<key>CFBundleExecutable</key>\n" \
        				"\t<string>sam</string>\n" \
        				"\t<key>CFBundleGetInfoString</key>\n" \
        				"\t<string>0.1-1</string>\n" \
        				"\t<key>CFBundleIconName</key>\n" \
        				"\t<string>spaceglenda</string>\n" \
        				"\t<key>CFBundleIconFile</key>\n" \
        				"\t<string>spaceglenda.icns</string>\n" \
        				"\t<key>CFBundleIdentifier</key>\n" \
        				"\t<string>com.swtch.sam</string>\n" \
        				"\t<key>CFBundleInfoDictionaryVersion</key>\n" \
        				"\t<string>6.0</string>\n" \
        				"\t<key>CFBundleName</key>\n" \
        				"\t<string>Sam</string>\n" \
        				"\t<key>CFBundlePackageType</key>\n" \
        				"\t<string>APPL</string>\n" \
        				"\t<key>CFBundleShortVersionString</key>\n" \
        				"\t<string>0.1</string>\n" \
        				"\t<key>CFBundleSignature</key>\n" \
        				"\t<string>Sam1</string>\n" \
        				"\t<key>CFBundleVersion</key>\n" \
        				"\t<string>1</string>\n" \
        				"</dict>\n" \
        				"</plist>" > $out/Applications/Sam.app/Contents/Info.plist

        			chmod a+x $out/Applications/Acme.app/Contents/MacOS/acme
        			chmod a+x $out/Applications/Sam.app/Contents/MacOS/sam

        			cp $out/plan9/mac/spaceglenda.icns $out/Applications/Acme.app/Contents/Resources
        			cp $out/plan9/mac/spaceglenda.icns $out/Applications/Sam.app/Contents/Resources
        			cp -r $out/plan9/mac/9term.app $out/Applications
        		'';
  });
}
