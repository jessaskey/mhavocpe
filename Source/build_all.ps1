
$Driver = 'mhavocpe'
$FilePrefix = 'mhpe'
$TournamentPrefix = 'te'

#Get Current Build Version
$MinorVersion = (Select-String -Path 'havoc.ah' '^MINOR_VERSION =\s?\$(\d+)\s?$' -AllMatches).Matches.Groups[1].Value
$MajorVersion = (Select-String -Path 'havoc.ah' '^MAJOR_VERSION =\s?\$(\d+)\s?$' -AllMatches).Matches.Groups[1].Value

$FolderPath = "C:\Users\jaskey\Documents\GitHub\jessaskey\mhavocpe\ROMs\_builds\"

function Build-Edition() {
	param([string]$OutputPath, [string]$LanguageCode, [string]$MajorVersion, [string]$MinorVersion)
	
	Output-ROMS $OutputPath $LanguageCode 'promisedend' $MajorVersion $MinorVersion 'adpcm'
	Output-ROMS $OutputPath $LanguageCode 'tournament' $MajorVersion $MinorVersion 'adpcm'
	Output-ROMS $OutputPath $LanguageCode 'promisedend' $MajorVersion $MinorVersion 'lpc'
	Output-ROMS $OutputPath $LanguageCode 'tournament' $MajorVersion $MinorVersion 'lpc'
}

function Output-ROMS() {
	param([string]$OutputPath, [string]$LanguageCode, [string]$EditionCode, [string]$MajorVersion, [string]$MinorVersion, [string]$SpeechCode)

	$LanguageValue = '0'
	Switch($LanguageCode) {
		'en' {$LanguageValue = '0'}
		'de' {$LanguageValue = '1'}
		'fr' {$LanguageValue = '2'}
	}

	$EditionValue = '0'
	switch($EditionCode) {
		'tournament' {$EditionValue = '1'}
	}
	
	$SpeechValue = '0'
	Switch($SpeechCode) {
		'adpcm' {$SpeechValue = '0'}
		'lpc' {$SpeechValue = '1'}
	}
	
	(Get-Content 'havoc.ah') -replace '^SPEECH = .$', ('SPEECH = ' + $SpeechValue) -replace '^LANGUAGE = .$', ('LANGUAGE = ' + $LanguageValue) -replace '^TOURNAMENT_EDITION = .$', ('TOURNAMENT_EDITION = ' + $EditionValue) -replace '^DEBUG = .$', 'DEBUG = 0' | Set-Content -Path 'havoc.ah'

	$TargetArchive = ($Driver + "_" + $LanguageCode)
    If ($EditionCode -ne 'promisedend') { $TargetArchive = ($TargetArchive + "_" + $EditionCode) }
    $TargetArchive = ($TargetArchive + "_" + $SpeechCode + "_" + $MajorVersion + $MinorVersion)
    $LstArchive = ($TargetArchive + "_listings.zip")
    $TargetArchive = ($TargetArchive + ".zip")
	
    #echo this
    ("Building... " + $TargetArchive)
    $TargetArchive = ($OutputPath + "\\" + $TargetArchive)
    $LstArchive = ($OutputPath + "\\" + $LstArchive)
    
    Build-Files $LanguageCode
	Build-Files $LanguageCode
	
	$MazeCollectionName = (Select-String -Path 'lst/auxpgm_6.lst' 'MAZECOLLECTION\: ([\S ]*)$' -AllMatches).Matches.Groups[1].Value
	$BuildDate = Get-Date -Format "dddd MM/dd/yyyy HH:mm K"
	
	("BuildDate: " + $BuildDate + "`r`nVersion: " + $MajorVersion + $MinorVersion + "`r`nLanguage: " + $LanguageCode + "`r`nEdition: " + $EditionCode + "`r`nCollection: " + $MazeCollectionName + "`r`nSpeech: " + $SpeechValue) | Set-Content -Path 'rom\buildinfo.txt'
    
    $FileName = ("rom\\" + $FilePrefix)
	If ($LanguageCode -ne 'en') { $FileName = ($FileName + $LanguageCode) }
	If ($EditionCode -eq "tournament") { $FileName = ($FileName + $TournamentPrefix) }	
	$FileName = ($FileName + $MajorVersion + $MinorVersion)
     
	#Copy the Speech ROM to target always
	cmd /c copy /b rom\mhavocpe.x1 ($FileName + ".x1") >$null

	Switch($SpeechCode) {
		'adpcm' {
			Get-ChildItem -Path rom\buildinfo.txt, rom\136002-125.6c,($FileName+".1l"),($FileName+".1mn"),($FileName+".1np"),($FileName+".1q"),($FileName+".6h"),($FileName+".6jk"),($FileName+".6kl"),($FileName+".9s"),($FileName+".x1") | Compress-Archive -Force -DestinationPath $TargetArchive
		}
		'lpc' {
			Get-ChildItem -Path rom\buildinfo.txt, rom\136002-125.6c,($FileName+".1l"),($FileName+".1mn"),($FileName+".1np"),($FileName+".1q"),($FileName+".6h"),($FileName+".6jk"),($FileName+".6kl"),($FileName+".9s") | Compress-Archive -Force -DestinationPath $TargetArchive
		}
	}
    
    Compress-Archive -Force -Path lst -DestinationPath $LstArchive
	
}

#function Build-File() {
#	param([string]$filename)
#	
#	$Command = ".\tasmx.exe -65 -g3 -c -fff -s -e -ll -y -wmhtext.tab " + $filename + ".asm .\obj\" + $filename + ".obj .\lst\" + $filename + ".lst .\exp\" + $filename + ".exp .\sym\" + $filename + ".sym"
#	return Invoke-Expression $Command
#}


function Build-Files() {
    param([string]$LanguageCode)
#	$ReturnValue = Build-File "auxpgm_0"
#	if ($ReturnValue -match "Number of errors = 0") {
#		"NO ERRORS"
#	}
#	else {
#		"THERE WERE ERRORS"
#	}
	Start-Process -WindowStyle Hidden -Wait -FilePath tasmx.exe -ArgumentList "-65 -g3 -c -fff -s -e -ll -y -wmhtext.tab auxpgm_0.asm .\obj\auxpgm_0.obj .\lst\auxpgm_0.lst .\exp\auxpgm_0.exp .\sym\auxpgm_0.sym"
	Start-Process -WindowStyle Hidden -Wait -FilePath tasmx.exe -ArgumentList "-65 -g3 -c -fff -s -e -ll -y -wmhtext.tab auxpgm_1.asm .\obj\auxpgm_1.obj .\lst\auxpgm_1.lst .\exp\auxpgm_1.exp .\sym\auxpgm_1.sym"
	Start-Process -WindowStyle Hidden -Wait -FilePath tasmx.exe -ArgumentList "-65 -g3 -c -fff -s -e -ll -y -wmhtext.tab auxpgm_2.asm .\obj\auxpgm_2.obj .\lst\auxpgm_2.lst .\exp\auxpgm_2.exp .\sym\auxpgm_2.sym"
	Start-Process -WindowStyle Hidden -Wait -FilePath tasmx.exe -ArgumentList "-65 -g3 -c -fff -s -e -ll -y -wmhtext.tab auxpgm_3.asm .\obj\auxpgm_3.obj .\lst\auxpgm_3.lst .\exp\auxpgm_3.exp .\sym\auxpgm_3.sym"
	Start-Process -WindowStyle Hidden -Wait -FilePath tasmx.exe -ArgumentList "-65 -g3 -c -fff -s -e -ll -y -wmhtext.tab auxpgm_4.asm .\obj\auxpgm_4.obj .\lst\auxpgm_4.lst .\exp\auxpgm_4.exp .\sym\auxpgm_4.sym"
	Start-Process -WindowStyle Hidden -Wait -FilePath tasmx.exe -ArgumentList "-65 -g3 -c -fff -s -e -ll -y -wmhtext.tab auxpgm_5.asm .\obj\auxpgm_5.obj .\lst\auxpgm_5.lst .\exp\auxpgm_5.exp .\sym\auxpgm_5.sym" 
	Start-Process -WindowStyle Hidden -Wait -FilePath tasmx.exe -ArgumentList "-65 -g3 -c -fff -s -e -ll -y -wmhtext.tab auxpgm_6.asm .\obj\auxpgm_6.obj .\lst\auxpgm_6.lst .\exp\auxpgm_6.exp .\sym\auxpgm_6.sym"
	Start-Process -WindowStyle Hidden -Wait -FilePath tasmx.exe -ArgumentList "-65 -g3 -c -fff -s -e -ll -y -wmhtext.tab auxpgm_7.asm .\obj\auxpgm_7.obj .\lst\auxpgm_7.lst .\exp\auxpgm_7.exp .\sym\auxpgm_7.sym" 
	Start-Process -WindowStyle Hidden -Wait -FilePath tasmx.exe -ArgumentList "-65 -g3 -c -fff -s -e -ll -y -wmhtext.tab mh_vrom.asm .\obj\mh_vrom.obj .\lst\mh_vrom.lst .\exp\mh_vrom.exp .\sym\mh_vrom.sym"
	Start-Process -WindowStyle Hidden -Wait -FilePath tasmx.exe -ArgumentList "-65 -g3 -c -fff -s -e -ll -y -wmhtext.tab mh_vrom0.asm .\obj\mh_vrom0.obj .\lst\mh_vrom0.lst .\exp\mh_vrom0.exp .\sym\mh_vrom0.sym" 
	Start-Process -WindowStyle Hidden -Wait -FilePath tasmx.exe -ArgumentList "-65 -g3 -c -fff -s -e -ll -y -wmhtext.tab mh_vrom1.asm .\obj\mh_vrom1.obj .\lst\mh_vrom1.lst .\exp\mh_vrom1.exp .\sym\mh_vrom1.sym"
	Start-Process -WindowStyle Hidden -Wait -FilePath tasmx.exe -ArgumentList "-65 -g3 -c -fff -s -e -ll -y -wmhtext.tab mh_vrom2.asm .\obj\mh_vrom2.obj .\lst\mh_vrom2.lst .\exp\mh_vrom2.exp .\sym\mh_vrom2.sym"
	Start-Process -WindowStyle Hidden -Wait -FilePath tasmx.exe -ArgumentList "-65 -g3 -c -fff -s -e -ll -y -wmhtext.tab mh_vrom3.asm .\obj\mh_vrom3.obj .\lst\mh_vrom3.lst .\exp\mh_vrom3.exp .\sym\mh_vrom3.sym"
	Start-Process -WindowStyle Hidden -Wait -FilePath tasmx.exe -ArgumentList "-65 -g3 -c -fff -s -e -ll -y -wmhtext.tab mh_gamma.asm .\obj\mh_gamma.obj .\lst\mh_gamma.lst .\exp\mh_gamma.exp .\sym\mh_gamma.sym"
	Start-Process -WindowStyle Hidden -Wait -FilePath tasmx.exe -ArgumentList "-65 -g3 -c -fff -s -e -ll -y -wmhtext.tab mh_alpha.asm .\obj\mh_alpha.obj .\lst\mh_alpha.lst .\exp\mh_alpha.exp .\sym\mh_alpha.sym"
	#Start-Process -WindowStyle Hidden -Wait -FilePath tasmx.exe -ArgumentList "-65 -g3 -c -q mhavocpe.asm .\obj\mhavocpe.obj .\lst\mhavocpe.lst .\exp\mhavocpe.exp"
	#Start-Process -FilePath tasmx.exe -ArgumentList "-65 -g3 -c -fff -s -e -ll -y mh_beta.asm .\obj\mh_beta.obj .\lst\mh_beta.lst .\exp\mh_beta.exp .\sym\mh_beta.sym 

    #$LanguageCode

	$FileNamePrefix =  ("rom\" + $FilePrefix)   
    If ($LanguageCode -ne 'en') { $FileNamePrefix = ($FileNamePrefix + $LanguageCode)}
	If ($EditionCode -eq "tournament") { $FileNamePrefix = ($FileNamePrefix + $TournamentPrefix) }
	$FileNamePrefix = ($FileNamePrefix + $MajorVersion + $MinorVersion)	
	If ($SpeechCode -eq "lpc") { $FileNamePrefix = ($FileNamePrefix + "_" + $SpeechCode) }
	
	.\split obj\mh_alpha.obj 16384 >$null
	cmd /c copy /b obj\mh_alpha.obj.1 ($FileNamePrefix + ".1mn") >$null
	cmd /c copy /b obj\mh_alpha.obj.2 ($FileNamePrefix + ".1l") >$null
	cmd /c copy /b obj\auxpgm_0.obj+obj\auxpgm_1.obj+obj\auxpgm_4.obj+obj\auxpgm_5.obj ($FileNamePrefix + ".1q") >$null
	cmd /c copy /b obj\auxpgm_2.obj+obj\auxpgm_3.obj+obj\auxpgm_6.obj+obj\auxpgm_7.obj ($FileNamePrefix + ".1np") >$null

    cmd /c copy /b obj\mh_vrom.obj ($FileNamePrefix + ".6kl") >$null
	cmd /c copy /b obj\mh_vrom0.obj + obj\mh_vrom1.obj ($FileNamePrefix + ".6h") /B >$null
	cmd /c copy /b obj\mh_vrom2.obj + obj\mh_vrom3.obj ($FileNamePrefix + ".6jk") /B >$null
	cmd /c copy /b obj\mh_gamma.obj ($FileNamePrefix + ".9s") >$null
    
    cmd /c copy /b obj\mh_gamma.obj ($FileNamePrefix + ".9s") >$null
	
	#.\split obj\mh_beta.obj 16384
	#cmd /c copy /b obj\mh_beta.obj.1 rom\$Driver.1bc
	#cmd /c copy /b obj\mh_beta.obj.2 rom\$Driver.1d
}

#**********************************
#MAIN SCRIPT STARTS HERE
#**********************************

$OutputPath = ($FolderPath + $MajorVersion + "." + $MinorVersion)

if (![System.IO.Directory]::Exists($OutputPath)) {
	New-Item -Path ($OutputPath) -ItemType Directory
}

Build-Edition $OutputPath 'en' $MajorVersion $MinorVersion
Build-Edition $OutputPath 'de' $MajorVersion $MinorVersion
Build-Edition $OutputPath 'fr' $MajorVersion $MinorVersion

#restore default build flags
(Get-Content 'havoc.ah') -replace '^SPEECH = .$', 'SPEECH = 0' -replace '^LANGUAGE = .$', 'LANGUAGE = 0' -replace '^TOURNAMENT_EDITION = .$', 'TOURNAMENT_EDITION = 0' -replace '^DEBUG = .$', 'DEBUG = 0' | Set-Content -Path 'havoc.ah'